type t

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

type stopped_reason =
  | Entry | Step | Pause | Breakpoint | Exception
[@@deriving show]

type status = Running | Stopped of stopped_reason | Exited
[@@deriving show]

type stack_frame = {
  index : int;
  stack_pos : int;
  pc : pc;
  debug_event : Instruct.debug_event;
}

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

val start : options -> t Lwt.t

val resolve : t -> src_pos -> (pc * src_pos) option Lwt.t

val lexing_pos_of_debug_event : Instruct.debug_event -> Lexing.position

val module_info_list : t -> module_info list Lwt.t

val find_module_info_by_src_pos : t -> src_pos -> module_info Lwt.t

val find_module_info_by_id : t -> string -> module_info Lwt.t

val stack_trace : t -> stack_frame list Lwt.t

val status_signal : t -> status React.S.t

val symbols_change_event : t -> unit React.E.t

val set_breakpoint : t -> breakpoint -> unit Lwt.t

val remove_breakpoint : t -> breakpoint -> unit Lwt.t

val check_breakpoint : t -> pc -> breakpoint option Lwt.t

val terminate : t -> unit Lwt.t

val continue : t -> unit Lwt.t

val pause : t -> unit Lwt.t

val next : t -> unit Lwt.t

val step_in : t -> unit Lwt.t

val step_out : t -> unit Lwt.t
