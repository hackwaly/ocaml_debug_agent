open Types

type breakpoint = {
  id : int;
  pc : pc;
  active_s : bool React.S.t;
  set_active : bool -> unit;
}

type t

val make : unit -> t

val set_breakpoint : t -> breakpoint -> unit Lwt.t

val remove_breakpoint : t -> breakpoint -> unit Lwt.t

val check_breakpoint : t -> pc -> breakpoint option Lwt.t

val commit : t -> (module REMOTE_DEBUGGER) -> unit Lwt.t
