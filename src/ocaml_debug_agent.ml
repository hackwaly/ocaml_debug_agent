open Types

type conn = Types.conn = {in_: Lwt_io.input_channel; out: Lwt_io.output_channel}

type remote_debugger_version = OCaml_400 | OCaml_410

type options =
  { remote_debugger_version: remote_debugger_version
  ; debug_connnection: conn
  ; time_slice: int
  ; symbols_file: string }

type src_pos = Types.src_pos

type pc = Types.pc

type breakpoint = Breakpoints.breakpoint

type status = Running | Entrypoint | Breakpoint | Uncaught_exc | Exited
[@@deriving show]

type t =
  { rdbg: (module REMOTE_DEBUGGER)
  ; conn: conn
  ; symbols: Symbols.t
  ; loop_promise: unit Lwt.t
  ; emit_wakeup: unit -> unit
  ; breakpoints: Breakpoints.t
  ; set_pause_flag: bool -> unit
  ; status_s: status React.S.t }

let start opts =
  let rdbg =
    match opts.remote_debugger_version with
    | OCaml_400 ->
        assert false
    | OCaml_410 ->
        (module Remote_debugger_410 : REMOTE_DEBUGGER)
  in
  let (module Rdbg) = rdbg in
  let conn = opts.debug_connnection in
  let symbols = Symbols.make () in
  let breakpoints = Breakpoints.make () in
  Symbols.load symbols 0 opts.symbols_file ;%lwt
  let wakeup_e, emit_wakeup = React.E.create () in
  let status_s, set_status = React.S.create Entrypoint in
  let pause_flag_s, set_pause_flag = React.S.create true in
  let loop () =
    let commit () =
      Breakpoints.commit breakpoints (module Rdbg) conn;
    in
    let rec next () =
      let%lwt report = Rdbg.go conn opts.time_slice in
      match report.rep_type with
      | Exited | Breakpoint | Uncaught_exc ->
          Lwt.return report
      | Event ->
          Log.debug (fun m -> m "next event");%lwt
          if React.S.value pause_flag_s then Lwt.return report else next ()
      | _ ->
          next ()
    in
    let break = ref false in
    while%lwt not !break do
      Log.debug (fun m -> m "commit start");%lwt
      commit () ;%lwt
      Log.debug (fun m -> m "commit end");%lwt
      Log.debug (fun m -> m "pull start");%lwt
      if%lwt Lwt.return (React.S.value pause_flag_s) then
        Lwt_react.E.next
          (Lwt_react.E.select
             [ pause_flag_s |> React.S.changes
               |> React.E.fmap (fun pause ->
                      if not pause then Some () else None)
             ; wakeup_e ]) ;%lwt
      Log.debug (fun m -> m "pull end");%lwt
      if%lwt Lwt.return (not (React.S.value pause_flag_s)) then (
        Lwt.pause () ;%lwt
        set_status Running ;
        Log.debug (fun m -> m "next start");%lwt
        let%lwt report = next () in
        Log.debug (fun m -> m "next end");%lwt
        set_pause_flag true ;
        match report.rep_type with
        | Exited ->
            Lwt.pause () ;%lwt
            set_status Exited ;
            break := true ;
            Lwt.return ()
        | Breakpoint | Uncaught_exc ->
            Lwt.pause () ;%lwt
            set_status
              ( match report.rep_type with
              | Breakpoint ->
                  Breakpoint
              | Uncaught_exc ->
                  Uncaught_exc
              | _ ->
                  assert false ) ;
            Lwt.return ()
        | _ ->
            [%lwt assert false] )
    done
  in
  let loop_promise = loop () in
  Lwt.return
    { rdbg
    ; conn
    ; symbols
    ; loop_promise
    ; emit_wakeup
    ; breakpoints
    ; set_pause_flag
    ; status_s }

let resolve agent src_pos = Symbols.resolve agent.symbols src_pos

let status_signal agent = agent.status_s

let symbols_change_event agent = Symbols.change_event agent.symbols

let sources agent = Symbols.sources agent.symbols

let make_breakpoint = Breakpoints.make_breakpoint

let set_breakpoint agent bp =
  Breakpoints.set_breakpoint agent.breakpoints bp ;
  agent.emit_wakeup () ;
  Lwt.return ()

let remove_breakpoint agent bp =
  Breakpoints.remove_breakpoint agent.breakpoints bp ;
  Lwt.return ()

let terminate agent =
  Lwt.cancel agent.loop_promise ;
  Lwt.return ()

let continue agent =
  Lwt.pause () ;%lwt
  agent.set_pause_flag false ;
  agent.emit_wakeup () ;
  Lwt.return ()

let pause agent = Lwt.pause () ;%lwt agent.set_pause_flag true ; Lwt.return ()

let next agent = ignore agent ; Lwt.return ()

let step_in agent = ignore agent ; Lwt.return ()

let step_out agent = ignore agent ; Lwt.return ()
