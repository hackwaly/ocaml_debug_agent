type scope = Local | Heap | Rec | Global

type value =
  | Int of int
  | Double of float
  | Bool of bool
  | Char of char
  | String of string
  | Module of Symbols.Module.t
  | Scope of scope
  | Unknown

type key = Indexed of int | Named of string

type t

type path = {
  scene_id : int64;  (** Equals to [report.rep_event_count] *)
  scope : scope;
  keys : key list;
}

type obj = { path : path; value : value; name : string }

type ident = Ident of string | Qualified of string * ident

type expr = Lookup of ident | Field of expr * ident | Component of expr * int

val create :
  symbols:Symbols.t ->
  remote_debugger:(module Remote_debugger.S) ->
  conn:Remote_debugger.conn ->
  unit ->
  t

val update_scene : t -> Remote_debugger.report -> unit

val scope_obj : t -> scope -> obj Lwt.t

val list_obj : t -> obj -> obj list Lwt.t
