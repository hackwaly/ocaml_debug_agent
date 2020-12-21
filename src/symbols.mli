open Remote_debugger

type module_ = {
  frag : int;
  id : string;
  resolved_source : string option;
  events : Instruct.debug_event array;
}

type t

val create :
  ?derive_source_paths:(string -> string list -> string list Lwt.t) -> unit -> t

val commit : t -> (module Remote_debugger.S) -> conn -> unit Lwt.t

val did_commit_hook : t -> (t -> unit Lwt.t) ref

val load : t -> int -> string -> unit Lwt.t

val to_seq_modules : t -> module_ Seq.t

val to_seq_events : t -> Instruct.debug_event Seq.t

val find_event_by_pc : t -> pc -> Instruct.debug_event Lwt.t

val find_module_by_id : t -> string -> module_ Lwt.t

val find_module_by_src_path : t -> string -> module_ Lwt.t

val find_event_in_module : module_ -> line:int -> column:int -> Instruct.debug_event Lwt.t
