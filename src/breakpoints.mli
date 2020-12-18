open Types

type breakpoint = {
  id : int;
  pc : pc;
  active_s : bool React.S.t;
  set_active : bool -> unit;
}

module Breakpoint : sig
  type t = breakpoint

  val id : t -> int

  val make : id:int -> pc:pc -> unit -> t
  val active_signal : t -> bool React.S.t
end

type t

val make : unit -> t

val set_breakpoint : t -> breakpoint -> unit Lwt.t

val remove_breakpoint : t -> breakpoint -> unit Lwt.t

val check_breakpoint : t -> pc -> breakpoint option Lwt.t

val commit : t -> (module REMOTE_DEBUGGER) -> unit Lwt.t
