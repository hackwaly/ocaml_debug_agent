open Types

type breakpoint = {
  pc : pc;
  active_s : bool React.S.t;
  set_active : bool -> unit;
}

type t

val make : unit -> t

val set_breakpoint : t -> breakpoint -> unit Lwt.t

val remove_breakpoint : t -> breakpoint -> unit Lwt.t

val commit : t -> (module REMOTE_DEBUGGER) -> conn -> unit Lwt.t
