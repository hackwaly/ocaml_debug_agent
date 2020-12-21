open Remote_debugger

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

type mixed_action = [ `Sync | `Run | `Pause | `Stop ]

type stopped_action = [ `Sync | `Run | `Stop ]

type running_action = [ `Sync | `Pause | `Stop ]

type t = {
  remote_debugger : (module Remote_debugger.S);
  debug_socket : Lwt_unix.file_descr;
  yield_point : int;
  status_s : status Lwt_react.S.t;
  set_status : status -> unit;
  action_e : mixed_action Lwt_react.E.t;
  emit_action : mixed_action -> unit;
}

let create opts =
  let status_s, set_status = React.S.create Entry in
  let action_e, emit_action = Lwt_react.E.create () in
  {
    remote_debugger =
      ( match opts.remote_debugger_version with
      | OCaml_400 -> failwith "Not yet implemented"
      | OCaml_410 -> (module Remote_debugger_410 : Remote_debugger.S) );
    debug_socket = opts.debug_socket;
    yield_point = opts.yield_point;
    status_s;
    set_status;
    action_e;
    emit_action;
  }

let is_running agent =
  match agent.status_s |> Lwt_react.S.value with Running -> true | _ -> false

let start agent =
  let (module Remote_debugger) = agent.remote_debugger in
  let%lwt fd, _ = Lwt_unix.accept agent.debug_socket in
  let conn =
    {
      io_in = Lwt_io.(of_fd ~mode:input fd);
      io_out = Lwt_io.(of_fd ~mode:output fd);
    }
  in
  let sync () = Lwt.return () in
  let check _report =
    [%lwt assert (is_running agent)];%lwt
    sync ();%lwt
    Lwt.return false
  in
  let action_stream =
    agent.action_e
    |> Lwt_react.E.fmap (fun action ->
           match action with `Pause -> None | #stopped_action as x -> Some x)
    |> Lwt_react.E.to_stream
  in
  let execute =
    let run () =
      let rec loop () =
        let%lwt report = Remote_debugger.go conn 0 in
        if%lwt check report then Lwt.return report
        else loop ()
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
    let stop () = Lwt.return () in
    function `Run -> run () | `Stop -> stop () | `Sync -> Lwt.return ()
  in
  try%lwt
    while%lwt true do
      sync ();%lwt
      let%lwt action = Lwt_stream.next action_stream in
      execute action
    done
  with Exit -> Lwt.return ()
