open Types

type conn = Types.conn = {in_: Lwt_io.input_channel; out: Lwt_io.output_channel}

type remote_debugger_version = OCaml_400 | OCaml_410

type options =
  { remote_debugger_version: remote_debugger_version
  ; debug_connnection: conn
  ; symbols_file: string }

type src_pos = Types.src_pos

type pc = Types.pc

type breakpoint = Breakpoints.breakpoint

type t =
  { rdbg: (module REMOTE_DEBUGGER)
  ; conn: conn
  ; symbols: Symbols.t
  ; loop_promise: unit Lwt.t
  ; emit_wakeup: unit -> unit
  ; breakpoints: Breakpoints.t }

let start opts =
  let rdbg =
    match opts.remote_debugger_version with
    | OCaml_400 ->
        assert false
    | OCaml_410 ->
        (module Remote_debugger_410 : REMOTE_DEBUGGER)
  in
  let symbols = Symbols.make () in
  Symbols.load symbols 0 opts.symbols_file ;%lwt
  let wakeup_e, emit_wakeup = React.E.create () in
  let (module Rdbg) = rdbg in
  ignore wakeup_e ;
  let loop () = Lwt.return () in
  let loop_promise = loop () in
  Lwt.return
    { rdbg
    ; conn= opts.debug_connnection
    ; symbols
    ; loop_promise
    ; emit_wakeup
    ; breakpoints= Breakpoints.make () }

let resolve agent src_pos = Symbols.resolve agent.symbols src_pos

let symbols_change_event agent = agent.symbols.change_e

let make_breakpoint = Breakpoints.make_breakpoint

let set_breakpoint agent bp = Breakpoints.set_breakpoint agent.breakpoints bp

let remove_breakpoint agent bp =
  Breakpoints.remove_breakpoint agent.breakpoints bp

let terminate agent =
  Lwt.cancel agent.loop_promise ;
  Lwt.return ()

let continue agent = ignore agent ; Lwt.return ()

let pause agent = ignore agent ; Lwt.return ()

let next agent = ignore agent ; Lwt.return ()

let step_in agent = ignore agent ; Lwt.return ()

let step_out agent = ignore agent ; Lwt.return ()
