type src_pos = { source : string; line : int; column : int } [@@deriving show]

type pc = { frag : int; pos : int } [@@deriving show]

type conn = { in_ : Lwt_io.input_channel; out : Lwt_io.output_channel }

type fork_mode = Fork_child | Fork_parent [@@deriving show]

type debug_info = { eventlists : Instruct.debug_event list array }

let pp_debug_info fmt _ = Format.pp_print_string fmt "<debug info>"

type execution_summary =
  | Event
  | Breakpoint
  | Exited
  | Trap
  | Uncaught_exc
  | Code_debug_info of debug_info
  | Code_loaded of int
  | Code_unloaded of int
[@@deriving show]

type report = {
  rep_type : execution_summary;
  rep_event_count : int64;
  rep_stack_pointer : int;
  rep_program_pointer : pc;
}
[@@deriving show]

type checkpoint_report = Checkpoint_done of int | Checkpoint_failed
[@@deriving show]

type remote_value = nativeint
[@@deriving show]

type get_field_result = Remote_value of remote_value | Double of float
[@@deriving show]

module type REMOTE_DEBUGGER = sig
  val get_pid : unit -> int Lwt.t

  val set_event : pc -> unit Lwt.t

  val set_breakpoint : pc -> unit Lwt.t

  val reset_instr : pc -> unit Lwt.t

  val checkpoint : unit -> checkpoint_report Lwt.t

  val go : int -> report Lwt.t

  val stop : unit -> unit Lwt.t

  val wait : unit -> unit Lwt.t

  val initial_frame : unit -> (int * pc) Lwt.t

  val get_frame : unit -> (int * pc) Lwt.t

  val set_frame : int -> unit Lwt.t

  val up_frame : int -> (int * pc) option Lwt.t

  val set_trap_barrier : int -> unit Lwt.t

  val get_local : int -> remote_value Lwt.t

  val get_environment : int -> remote_value Lwt.t

  val get_global : int -> remote_value Lwt.t

  val get_accu : unit -> remote_value Lwt.t

  val get_header : remote_value -> int Lwt.t

  val get_field : remote_value -> int -> get_field_result Lwt.t

  val marshal_obj : remote_value -> 'a Lwt.t

  val get_closure_code : remote_value -> pc Lwt.t

  val set_fork_mode : fork_mode -> unit Lwt.t
end
