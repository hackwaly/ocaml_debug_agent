open Types

type t = Breakpoints.breakpoint

val id : t -> int

val make : id:int -> pc:pc -> unit -> t

val active_signal : t -> bool React.S.t
