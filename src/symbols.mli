open Remote_debugger

type module_ = {
  frag : int;
  id : string;
  resolved_source : string option;
  events : Instruct.debug_event array;
}

module Module : sig
  type t = module_

  val find_event : t -> int -> int -> Instruct.debug_event Lwt.t
end

type t

val create : unit -> t

val commit : t -> (module Remote_debugger.S) -> conn -> unit Lwt.t

val load : t -> frag:int -> string -> unit Lwt.t

val to_seq_modules : t -> module_ Seq.t

val to_seq_events : t -> Instruct.debug_event Seq.t

val find_event : t -> pc -> Instruct.debug_event

val find_module : t -> string -> module_ Lwt.t

val find_module_by_source : t -> string -> module_ Lwt.t
