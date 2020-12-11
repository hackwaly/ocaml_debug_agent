type t

type src_pos

type pc

type breakpoint

type conn = Types.conn = {in_: Lwt_io.input_channel; out: Lwt_io.output_channel}

type remote_debugger_version = OCaml_400 | OCaml_410

type options =
  { remote_debugger_version: remote_debugger_version
  ; debug_connnection: conn
  ; symbols_file: string }

val start : options -> t Lwt.t

val resolve : t -> src_pos -> (pc * src_pos) option Lwt.t

val symbols_change_event : t -> unit React.E.t

val make_breakpoint : pc:pc -> unit -> breakpoint

val set_breakpoint : t -> breakpoint -> unit

val remove_breakpoint : t -> breakpoint -> unit

val terminate : t -> unit Lwt.t

val continue : t -> unit Lwt.t

val pause : t -> unit Lwt.t

val next : t -> unit Lwt.t

val step_in : t -> unit Lwt.t

val step_out : t -> unit Lwt.t
