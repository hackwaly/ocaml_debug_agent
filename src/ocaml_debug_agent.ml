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

module Breakpoint = Breakpoints.Breakpoint

type breakpoint = Breakpoints.breakpoint

type stop_reason = Step | Pause | Breakpoint | Exception [@@deriving show]

type status = Entry | Running | Stopped of stop_reason | Exited
[@@deriving show]

type stack_frame = {
  index : int;
  stack_pos : int;
  pc : pc;
  debug_event : Instruct.debug_event;
}

type action = Wake_up_act | Continue_act | Pause_act | Step_in_act

type t = {
  rdbg : (module REMOTE_DEBUGGER);
  symbols : Symbols.t;
  loop_promise : unit Lwt.t;
  breakpoints : Breakpoints.t;
  status_s : status React.S.t;
  wake_up : action -> unit Lwt.t;
  push_pending : (unit -> unit Lwt.t) -> unit;
}

let start opts =
  let conn = opts.debug_connnection in
  let rdbg =
    match opts.remote_debugger_version with
    | OCaml_400 -> assert false
    | OCaml_410 -> Remote_debugger_410.attach conn
  in
  let (module Rdbg) = rdbg in
  let symbols = Symbols.make () in
  let breakpoints = Breakpoints.make () in
  let%lwt pid = Rdbg.get_pid () in
  ignore pid;
  Symbols.load symbols 0 opts.symbols_file;%lwt
  let pendings = ref [] in
  let push_pending f = pendings := f :: !pendings in
  let status_s, set_status = React.S.create Entry in
  let action_mvar = Lwt_mvar.create_empty () in
  let wake_up action =
    Log.debug (fun m -> m "wake up");%lwt
    Lwt_mvar.take_available action_mvar |> ignore;
    Lwt_mvar.put action_mvar action
  in
  let sync () =
    Log.debug (fun m -> m "sync start");%lwt
    Symbols.commit symbols (module Rdbg);%lwt
    let tasks = !pendings in
    pendings := [];
    tasks |> Lwt_list.iter_s (fun f -> f ());%lwt
    Breakpoints.commit breakpoints (module Rdbg);%lwt
    Log.debug (fun m -> m "sync end")
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
      let%lwt report = Rdbg.go opts.time_slice in
      sync ();%lwt
      let action = Lwt_mvar.take_available action_mvar in
      match action with
      | Some Pause_act -> Lwt.return (report, Pause)
      | _ -> (
          match report.rep_type with
          | Breakpoint ->
              let%lwt bp =
                Breakpoints.check_breakpoint breakpoints
                  report.rep_program_pointer
              in
              if Option.is_some bp then Lwt.return (report, Breakpoint)
              else go ()
          | Exited -> Lwt.fail Exit
          | Uncaught_exc -> Lwt.return (report, Exception)
          | _ -> go () )
    in
    let%lwt _report, reason = go () in
    set_status (Stopped reason);
    Lwt.return ()
  in
  let do_step_in () =
    set_status Running;
    let go () =
      let%lwt report = Rdbg.go 1 in
      sync ();%lwt
      match report.rep_type with
      | Breakpoint ->
          let%lwt bp =
            Breakpoints.check_breakpoint breakpoints report.rep_program_pointer
          in
          if Option.is_some bp then Lwt.return (report, Breakpoint)
          else Lwt.return (report, Step)
      | Exited -> Lwt.fail Exit
      | Uncaught_exc -> Lwt.return (report, Exception)
      | _ -> Lwt.return (report, Step)
    in
    let%lwt _report, reason = go () in
    set_status (Stopped reason);
    Lwt.return ()
  in
  let execute () =
    match%lwt Lwt_mvar.take action_mvar with
    | Continue_act -> do_continue ()
    | Step_in_act -> do_step_in ()
    | Pause_act | Wake_up_act -> Lwt.return ()
  in
  let loop () =
    try%lwt
      while%lwt true do
        sync ();%lwt
        sleep ();%lwt
        execute ()
      done
    with Exit ->
      set_status Exited;
      Lwt.return ()
  in
  let loop_promise = loop () in
  Lwt.return
    {
      rdbg;
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
  agent.wake_up Wake_up_act

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
  | Entry | Exited -> Lwt.return []
  | Stopped _ ->
      let promise, resolver = Lwt.task () in
      push_pending agent (fun () ->
          let (module Rdbg) = agent.rdbg in
          let%lwt curr_fr_sp, _ = Rdbg.get_frame () in
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
            match%lwt Rdbg.up_frame stacksize with
            | Some (sp, pc) ->
                let frame = make_frame index sp pc in
                walk_up index frame.debug_event.ev_stacksize (frame :: frames)
            | None -> Lwt.return frames
          in
          (let%lwt sp, pc = Rdbg.initial_frame () in
           let intial_frame = make_frame 0 sp pc in
           let%lwt frames =
             walk_up 0 intial_frame.debug_event.ev_stacksize [ intial_frame ]
           in
           let frames = List.rev frames in
           Lwt.wakeup_later resolver frames;
           Lwt.return ())
            [%finally Rdbg.set_frame curr_fr_sp]);%lwt
      promise

let set_breakpoint agent bp =
  Breakpoints.set_breakpoint agent.breakpoints bp;%lwt
  agent.wake_up Wake_up_act

let remove_breakpoint agent bp =
  Breakpoints.remove_breakpoint agent.breakpoints bp;%lwt
  agent.wake_up Wake_up_act

let check_breakpoint agent pc =
  Breakpoints.check_breakpoint agent.breakpoints pc

let terminate agent =
  Lwt.cancel agent.loop_promise;
  Lwt.return ()

let continue agent =
  match agent.status_s |> React.S.value with
  | Running | Exited -> Lwt.return ()
  | Entry | Stopped _ -> agent.wake_up Continue_act

let pause agent =
  match agent.status_s |> React.S.value with
  | Running -> agent.wake_up Pause_act
  | Entry | Exited | Stopped _ -> Lwt.return ()

let next agent =
  ignore agent;
  Lwt.return ()

let step_in agent =
  match agent.status_s |> React.S.value with
  | Running | Exited -> Lwt.return ()
  | Entry | Stopped _ -> agent.wake_up Step_in_act

let step_out agent =
  ignore agent;
  Lwt.return ()
