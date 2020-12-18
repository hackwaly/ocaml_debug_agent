open Types

(* Use lwt facility instead of react *)

type conn = Types.conn = {
  in_ : Lwt_io.input_channel;
  out : Lwt_io.output_channel;
}

type remote_debugger_version = OCaml_400 | OCaml_410

type options = {
  remote_debugger_version : remote_debugger_version;
  debug_connnection : conn;
  time_slice : int;
  symbols_file : string;
}

type src_pos = Types.src_pos = { source : string; line : int; column : int }
[@@deriving show]

type pc = Types.pc

type module_info = Symbols.module_info = {
  frag : int;
  id : string;
  resolved_source : string option;
  events : Instruct.debug_event array;
}

module Breakpoint = Breakpoint

type breakpoint = Breakpoint.t

type status = Running | Entrypoint | Breakpoint | Uncaught_exc | Exited
[@@deriving show]

type stack_frame = {
  index : int;
  stack_pos : int;
  pc : pc;
  debug_event : Instruct.debug_event;
}

type action = Wake_up | Continue | Pause | Step_in

type t = {
  rdbg : (module REMOTE_DEBUGGER);
  conn : conn;
  symbols : Symbols.t;
  loop_promise : unit Lwt.t;
  breakpoints : Breakpoints.t;
  status_s : status React.S.t;
  wake_up : action -> unit Lwt.t;
  push_pending : (unit -> unit Lwt.t) -> unit;
}

let start opts =
  let rdbg =
    match opts.remote_debugger_version with
    | OCaml_400 -> assert false
    | OCaml_410 -> (module Remote_debugger_410 : REMOTE_DEBUGGER)
  in
  let (module Rdbg) = rdbg in
  let conn = opts.debug_connnection in
  let symbols = Symbols.make () in
  let breakpoints = Breakpoints.make () in
  let%lwt pid = Rdbg.get_pid conn in
  ignore pid;
  Symbols.load symbols 0 opts.symbols_file;%lwt
  let pendings = ref [] in
  let push_pending f = pendings := f :: !pendings in
  let status_s, set_status = React.S.create Entrypoint in
  let action_mvar = Lwt_mvar.create_empty () in
  let wake_up action =
    Log.debug (fun m -> m "wake up");%lwt
    Lwt_mvar.take_available action_mvar |> ignore;
    Lwt_mvar.put action_mvar action
  in
  let sync () =
    Log.debug (fun m -> m "sync start");%lwt
    Symbols.commit symbols (module Rdbg) conn;%lwt
    let tasks = !pendings in
    pendings := [];
    tasks |> Lwt_list.iter_s (fun f -> f ());%lwt
    Breakpoints.commit breakpoints (module Rdbg) conn;%lwt
    Log.debug (fun m -> m "sync end")
  in
  let get_status report =
    match report.rep_type with
    | Exited -> Lwt.return Exited
    | Breakpoint -> Lwt.return Breakpoint
    | Uncaught_exc -> Lwt.return Uncaught_exc
    | _ -> [%lwt assert false]
  in
  let sleep () =
    Log.debug (fun m -> m "sleep");%lwt
    (* Do we need mutex here? *)
    let%lwt action = Lwt_mvar.take action_mvar in
    Lwt_mvar.put action_mvar action;%lwt
    Log.debug (fun m -> m "waked up")
  in
  let do_continue () =
    set_status Running;
    let rec go () =
      let%lwt report = Rdbg.go conn opts.time_slice in
      sync ();%lwt
      let action = Lwt_mvar.take_available action_mvar in
      match action with
      | Some Pause -> Lwt.return report
      | _ -> (
        match report.rep_type with
        | Breakpoint ->
            let%lwt bp =
              Breakpoints.check_breakpoint breakpoints report.rep_program_pointer
            in
            if Option.is_some bp then Lwt.return report else go ()
        | Exited | Uncaught_exc -> Lwt.return report
        | _ -> go ()
      )
    in
    let%lwt report = go () in
    let%lwt status = get_status report in
    set_status status;
    if status = Exited then Lwt.fail Exit else Lwt.return ();%lwt
    Lwt.return ()
  in
  let do_step_in () =
    set_status Running;
    let go () =
      let%lwt report = Rdbg.go conn 1 in
      sync ();%lwt
      Lwt.return report
    in
    let%lwt report = go () in
    let%lwt status = get_status report in
    set_status status;
    if status = Exited then Lwt.fail Exit else Lwt.return ();%lwt
    Lwt.return ()
  in
  let execute () =
    match%lwt Lwt_mvar.take action_mvar with
    | Continue ->
      do_continue ()
    | Step_in ->
      do_step_in ()
    | Pause
    | Wake_up -> Lwt.return ()
  in
  let loop () =
    try%lwt
      while%lwt true do
        sync ();%lwt
        sleep ();%lwt
        execute ()
      done
    with Exit -> Lwt.return ()
  in
  let loop_promise = loop () in
  Lwt.return
    {
      rdbg;
      conn;
      symbols;
      loop_promise;
      breakpoints;
      wake_up;
      status_s;
      push_pending;
    }

let lexing_pos_of_debug_event = Symbols.lexing_pos_of_debug_event

let push_pending agent f =
  agent.push_pending f;
  agent.wake_up Wake_up

let resolve agent src_pos = Symbols.resolve agent.symbols src_pos

let status_signal agent = agent.status_s

let symbols_change_event agent = Symbols.change_event agent.symbols

let module_info_list agent = Symbols.module_info_list agent.symbols

let find_module_info_by_src_pos agent =
  Symbols.find_module_info_by_src_pos agent.symbols

let find_module_info_by_id agent = Symbols.find_module_info_by_id agent.symbols

let stack_trace agent =
  match agent.status_s |> React.S.value with
  | Running -> [%lwt assert false]
  | Entrypoint -> Lwt.return []
  | _ ->
      let promise, resolver = Lwt.task () in
      push_pending agent (fun () ->
          let (module Rdbg) = agent.rdbg in
          let conn = agent.conn in
          let%lwt curr_fr_sp, _ = Rdbg.get_frame conn in
          let make_frame index sp pc =
            {
              index;
              stack_pos = sp;
              pc;
              debug_event = Symbols.lookup_event agent.symbols pc;
            }
          in
          let rec walk_up index stacksize frames =
            let index = index + 1 in
            match%lwt Rdbg.up_frame conn stacksize with
            | Some (sp, pc) ->
                let frame = make_frame index sp pc in
                walk_up index frame.debug_event.ev_stacksize (frame :: frames)
            | None -> Lwt.return frames
          in
          (let%lwt sp, pc = Rdbg.initial_frame conn in
           let intial_frame = make_frame 0 sp pc in
           let%lwt frames =
             walk_up 0 intial_frame.debug_event.ev_stacksize [ intial_frame ]
           in
           let frames = List.rev frames in
           Lwt.wakeup_later resolver frames;
           Lwt.return ())
            [%finally Rdbg.set_frame conn curr_fr_sp]);%lwt
      promise

let set_breakpoint agent bp =
  Breakpoints.set_breakpoint agent.breakpoints bp;%lwt
  agent.wake_up Wake_up

let remove_breakpoint agent bp =
  Breakpoints.remove_breakpoint agent.breakpoints bp;%lwt
  agent.wake_up Wake_up

let check_breakpoint agent pc =
  Breakpoints.check_breakpoint agent.breakpoints pc

let terminate agent =
  Lwt.cancel agent.loop_promise;
  Lwt.return ()

let continue agent = agent.wake_up Continue

let pause agent = agent.wake_up Pause

let next agent =
  ignore agent;
  Lwt.return ()

let step_in agent = agent.wake_up Step_in

let step_out agent =
  ignore agent;
  Lwt.return ()
