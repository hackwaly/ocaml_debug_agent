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

type t = {
  rdbg : (module REMOTE_DEBUGGER);
  conn : conn;
  symbols : Symbols.t;
  loop_promise : unit Lwt.t;
  breakpoints : Breakpoints.t;
  set_pause_flag : bool -> unit;
  status_s : status React.S.t;
  wake_up : unit -> unit Lwt.t;
  mutable pendings : (unit -> unit Lwt.t) list;
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
  let status_s, set_status = React.S.create Entrypoint in
  let pause_flag = ref true in
  let set_pause_flag v = pause_flag := v in
  let wake_up_mvar = Lwt_mvar.create () in
  let wake_up () =
    Log.debug (fun m -> m "wake up");%lwt
    Lwt_mvar.take_available wake_up_mvar |> ignore;
    Lwt_mvar.put wake_up_mvar ()
  in
  let loop () =
    let commit () =
      Symbols.commit symbols (module Rdbg) conn;%lwt
      Breakpoints.commit breakpoints (module Rdbg) conn
    in
    let rec run_slice () =
      let%lwt report = Rdbg.go conn opts.time_slice in
      match report.rep_type with
      | Breakpoint ->
          let%lwt bp =
            Breakpoints.check_breakpoint breakpoints report.rep_program_pointer
          in
          if Option.is_some bp then Lwt.return report else run_slice ()
      | Exited | Uncaught_exc -> Lwt.return report
      | Event -> if !pause_flag then Lwt.return report else run_slice ()
      | _ -> run_slice ()
    in
    let break = ref false in
    while%lwt not !break do
      Log.debug (fun m -> m "commit start");%lwt
      commit ();%lwt
      Log.debug (fun m -> m "commit end");%lwt
      Log.debug (fun m -> m "sleep");%lwt
      if !pause_flag then Lwt_mvar.take wake_up_mvar else Lwt.return ();%lwt
      Log.debug (fun m -> m "waked up");%lwt
      if not !pause_flag then (
        set_status Running;
        Log.debug (fun m -> m "run_slice start");%lwt
        let%lwt report = run_slice () in
        Log.debug (fun m -> m "run_slice end");%lwt
        set_pause_flag true;
        match report.rep_type with
        | Exited ->
            Lwt.pause ();%lwt
            set_status Exited;
            break := true;
            Lwt.return ()
        | Breakpoint | Uncaught_exc ->
            Lwt.pause ();%lwt
            set_status
              ( match report.rep_type with
              | Breakpoint -> Breakpoint
              | Uncaught_exc -> Uncaught_exc
              | _ -> assert false );
            Lwt.return ()
        | _ -> [%lwt assert false] )
      else Lwt.return ()
    done
  in
  let loop_promise = loop () in
  Lwt.return
    {
      rdbg;
      conn;
      symbols;
      loop_promise;
      breakpoints;
      set_pause_flag;
      wake_up;
      status_s;
      pendings = [];
    }

let push_pending agent f =
  agent.pendings <- f :: agent.pendings;
  agent.wake_up ()

let resolve agent src_pos = Symbols.resolve agent.symbols src_pos

let status_signal agent = agent.status_s

let symbols_change_event agent = Symbols.change_event agent.symbols

let sources agent = Symbols.sources agent.symbols

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
           Lwt.wakeup_later resolver frames;
           Lwt.return ())
            [%finally Rdbg.set_frame conn curr_fr_sp]);%lwt
      promise

let set_breakpoint agent bp =
  Breakpoints.set_breakpoint agent.breakpoints bp;%lwt
  agent.wake_up ()

let remove_breakpoint agent bp =
  Breakpoints.remove_breakpoint agent.breakpoints bp;%lwt
  agent.wake_up ()

let check_breakpoint agent pc =
  Breakpoints.check_breakpoint agent.breakpoints pc

let terminate agent =
  Lwt.cancel agent.loop_promise;
  Lwt.return ()

let continue agent =
  agent.set_pause_flag false;
  agent.wake_up ()

let pause agent =
  agent.set_pause_flag true;
  Lwt.return ()

let next agent =
  ignore agent;
  Lwt.return ()

let step_in agent =
  ignore agent;
  Lwt.return ()

let step_out agent =
  ignore agent;
  Lwt.return ()
