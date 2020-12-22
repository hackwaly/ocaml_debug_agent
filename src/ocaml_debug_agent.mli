type pc = Remote_debugger.pc = {
  frag : int;
  pos : int;
}

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

type t

type stack_frame = {
  index : int;
  stack_pos : int;
  debug_event : Instruct.debug_event;
}

val create : options -> t

val status_signal : t -> status Lwt_react.S.t

val set_breakpoint : t -> pc -> unit

val remove_breakpoint : t -> pc -> unit

val run : t -> unit

val pause : t -> unit

val stop : t -> unit

val stack_trace : t -> stack_frame list Lwt.t

val start : t -> unit Lwt.t
