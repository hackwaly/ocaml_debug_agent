open Types

type t = Breakpoints.breakpoint

val make : pc:pc -> unit -> t

val active_signal : t -> bool React.S.t
