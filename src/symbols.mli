open Types

type module_info = {
  frag : int;
  id : string;
  resolved_source : string option;
  events : Instruct.debug_event array;
}

type t

val make :
  ?derive_source_paths:(string -> string list -> string list Lwt.t) -> unit -> t

val commit : t -> (module REMOTE_DEBUGGER) -> conn -> unit Lwt.t

val change_event : t -> unit React.E.t

val load : t -> int -> string -> unit Lwt.t

val resolve : t -> src_pos -> (pc * src_pos) option Lwt.t

val lookup_event : t -> pc -> Instruct.debug_event

val lexing_pos_of_debug_event : Instruct.debug_event -> Lexing.position

val find_module_info_by_src_pos : t -> src_pos -> module_info Lwt.t

val find_module_info_by_id : t -> string -> module_info Lwt.t

val module_info_list : t -> module_info list Lwt.t
