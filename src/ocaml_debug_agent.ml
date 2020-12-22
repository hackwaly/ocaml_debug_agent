open Remote_debugger
module Log = Log

type pc = Remote_debugger.pc = { frag : int; pos : int }

type remote_debugger_version = OCaml_400 | OCaml_410

type options = {
  remote_debugger_version : remote_debugger_version; [@default OCaml_410]
  debug_socket : Lwt_unix.file_descr;
  symbols_file : string;
  yield_point : int; [@default 1024]
}
[@@deriving make]

type status =
  | Entry
  | Running
  | Stopped of { breakpoint : bool }
  | Exited of { uncaught_exc : bool }

type action = [ `Wake_up | `Run | `Step_in | `Pause | `Stop ]

type stopped_action = [ `Wake_up | `Run | `Step_in | `Stop ]

type t = {
  options : options;
  remote_debugger : (module Remote_debugger.S);
  status_s : status Lwt_react.S.t;
  set_status : status -> unit;
  action_e : action Lwt_react.E.t;
  emit_action : action -> unit;
  symbols : Symbols.t;
  symbols_updated_e : unit Lwt_react.E.t;
  emit_symbols_updated : unit -> unit;
  breakpoints : Breakpoints.t;
  mutable pendings : (conn -> unit Lwt.t) list;
}

module Module = Symbols.Module
module Stack_frame = Stack_frame

let create options =
  let status_s, set_status = React.S.create Entry in
  let action_e, emit_action = Lwt_react.E.create () in
  let symbols_updated_e, emit_symbols_updated = Lwt_react.E.create () in
  let symbols = Symbols.create () in
  let breakpoints = Breakpoints.create () in
  {
    remote_debugger =
      ( match options.remote_debugger_version with
      | OCaml_400 -> failwith "Not yet implemented"
      | OCaml_410 -> (module Remote_debugger_410 : Remote_debugger.S) );
    options;
    status_s;
    set_status;
    action_e;
    emit_action;
    symbols;
    symbols_updated_e;
    emit_symbols_updated;
    breakpoints;
    pendings = [];
  }

let symbols_updated_event agent = agent.symbols_updated_e

let to_seq_modules agent = Symbols.to_seq_modules agent.symbols

let find_module_by_source agent source =
  Symbols.find_module_by_source agent.symbols source

let find_module agent id = Symbols.find_module agent.symbols id

let is_running agent =
  match agent.status_s |> Lwt_react.S.value with Running -> true | _ -> false

let status_signal agent = agent.status_s

let set_breakpoint agent pc =
  Breakpoints.set agent.breakpoints pc;
  agent.emit_action `Wake_up

let remove_breakpoint agent pc =
  Breakpoints.remove agent.breakpoints pc;
  agent.emit_action `Wake_up

let run agent = agent.emit_action `Run

let step_in agent = agent.emit_action `Step_in

let pause agent = agent.emit_action `Pause

let stop agent = agent.emit_action `Stop

let push_pending agent f =
  agent.pendings <- f :: agent.pendings;
  agent.emit_action `Wake_up

let stack_trace agent =
  match agent.status_s |> React.S.value with
  | Running -> [%lwt assert false]
  | Exited _ -> Lwt.return []
  | Entry -> Lwt.return []
  | Stopped _ ->
      let promise, resolver = Lwt.task () in
      push_pending agent (fun conn ->
          let (module Rdbg) = agent.remote_debugger in
          let%lwt curr_fr_sp, _ = Rdbg.get_frame conn in
          let make_frame index sp (pc : pc) =
            let event = Symbols.find_event agent.symbols pc in
            let module_ = Symbols.find_module agent.symbols event.ev_module in
            { Stack_frame.index; stack_pos = sp; module_; event }
          in
          let rec walk_up index stacksize frames =
            let index = index + 1 in
            match%lwt Rdbg.up_frame conn stacksize with
            | Some (sp, pc) ->
                let frame = make_frame index sp pc in
                walk_up index (Stack_frame.stacksize frame) (frame :: frames)
            | None -> Lwt.return frames
          in
          (let%lwt sp, pc = Rdbg.initial_frame conn in
           let intial_frame = make_frame 0 sp pc in
           let%lwt frames =
             walk_up 0 (Stack_frame.stacksize intial_frame) [ intial_frame ]
           in
           let frames = List.rev frames in
           Lwt.wakeup_later resolver frames;
           Lwt.return ())
            [%finally Rdbg.set_frame conn curr_fr_sp]);
      promise

let start agent =
  let (module Rdbg) = agent.remote_debugger in
  let%lwt fd, _ = Lwt_unix.accept agent.options.debug_socket in
  let conn =
    {
      io_in = Lwt_io.(of_fd ~mode:input fd);
      io_out = Lwt_io.(of_fd ~mode:output fd);
    }
  in
  let%lwt pid = Rdbg.get_pid conn in
  ignore pid;
  let flush_pendings () =
    while%lwt agent.pendings <> [] do
      let pendings = List.rev agent.pendings in
      agent.pendings <- [];
      pendings |> Lwt_list.iter_s (fun f -> f conn)
    done
  in
  let sync () =
    Symbols.commit agent.symbols (module Rdbg) conn;%lwt
    Breakpoints.commit agent.breakpoints (module Rdbg) conn;%lwt
    flush_pendings ()
  in
  let check report =
    [%lwt assert (is_running agent)];%lwt
    sync ();%lwt
    match report.rep_type with
    | Breakpoint ->
        Breakpoints.check agent.breakpoints report.rep_program_pointer
    | Uncaught_exc | Exited -> Lwt.return true
    | _ -> Lwt.return false
  in
  let wait_action () =
    agent.action_e
    |> Lwt_react.E.fmap (fun action ->
           match action with `Pause -> None | #stopped_action as x -> Some x)
    |> Lwt_react.E.once |> Lwt_react.E.to_stream |> Lwt_stream.next
  in
  let execute =
    let run () =
      let rec loop () =
        let%lwt report = Rdbg.go conn agent.options.yield_point in
        if%lwt check report then Lwt.return report else loop ()
      in
      agent.set_status Running;
      let%lwt report = loop () in
      agent.set_status
        ( match report.rep_type with
        | Breakpoint -> Stopped { breakpoint = true }
        | Event -> Stopped { breakpoint = false }
        | Uncaught_exc -> Exited { uncaught_exc = true }
        | Exited -> Exited { uncaught_exc = false }
        | _ -> assert false );
      Lwt.return ()
    in
    let step_in () =
      let go () =
        Rdbg.go conn 1
      in
      agent.set_status Running;
      let%lwt report = go () in
      agent.set_status
        ( match report.rep_type with
        | Breakpoint -> Stopped { breakpoint = true }
        | Event -> Stopped { breakpoint = false }
        | Uncaught_exc -> Exited { uncaught_exc = true }
        | Exited -> Exited { uncaught_exc = false }
        | _ -> assert false );
      Lwt.return ()
    in
    let stop () =
      Rdbg.stop conn;%lwt
      Lwt.fail Exit
    in
    function
    | `Run -> run ()
    | `Step_in -> step_in ()
    | `Stop -> stop ()
    | `Wake_up -> Lwt.return ()
  in
  Symbols.load agent.symbols ~frag:0 agent.options.symbols_file;%lwt
  agent.emit_symbols_updated ();
  try%lwt
    while%lwt true do
      sync ();%lwt
      let%lwt action = wait_action () in
      execute action;%lwt
      if%lwt
        Lwt.return
          ( match agent.status_s |> Lwt_react.S.value with
          | Exited _ -> true
          | _ -> false )
      then Lwt.fail Exit
    done
  with Exit -> Lwt.return ()
