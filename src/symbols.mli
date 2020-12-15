open Types

type t

val make :
  ?derive_source_paths:(string -> string list -> string list Lwt.t) -> unit -> t

val commit : t -> (module REMOTE_DEBUGGER) -> conn -> unit Lwt.t

val change_event : t -> unit React.E.t

val load : t -> int -> string -> unit Lwt.t

val resolve : t -> src_pos -> (pc * src_pos) option Lwt.t

val sources : t -> string list Lwt.t
