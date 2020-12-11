type src_pos = {source: string; line: string; column: string}

type pc = {frag: int; pos: int}

type conn = {in_: Lwt_io.input_channel; out: Lwt_io.output_channel}

type fork_mode = Fork_child | Fork_parent

type execution_summary =
  | Event
  | Breakpoint
  | Exited
  | Trap
  | Uncaught_exc
  | Code_debug_info of Instruct.debug_event list array
  | Code_loaded of int
  | Code_unloaded of int

type report =
  { rep_type: execution_summary
  ; rep_event_count: int64
  ; rep_stack_pointer: int
  ; rep_program_pointer: pc }

type checkpoint_report = Checkpoint_done of int | Checkpoint_failed

type remote_value = nativeint

type get_field_result = Remote_value of remote_value | Double of float

module type REMOTE_DEBUGGER = sig
  val get_pid : conn -> int Lwt.t

  val set_event : conn -> pc -> unit Lwt.t

  val set_breakpoint : conn -> pc -> unit Lwt.t

  val reset_instr : conn -> pc -> unit Lwt.t

  val checkpoint : conn -> checkpoint_report Lwt.t

  val go : conn -> int -> report Lwt.t

  val stop : conn -> unit Lwt.t

  val wait : conn -> unit Lwt.t

  val initial_frame : conn -> (int * pc) Lwt.t

  val get_frame : conn -> (int * pc) Lwt.t

  val set_frame : conn -> int -> unit Lwt.t

  val up_frame : conn -> int -> unit Lwt.t

  val set_trap_barrier : conn -> int -> unit Lwt.t

  val get_local : conn -> int -> remote_value Lwt.t

  val get_environment : conn -> int -> remote_value Lwt.t

  val get_global : conn -> int -> remote_value Lwt.t

  val get_accu : conn -> remote_value Lwt.t

  val get_header : conn -> remote_value -> int Lwt.t

  val get_field : conn -> remote_value -> int -> get_field_result Lwt.t

  val marshal_obj : conn -> remote_value -> 'a Lwt.t

  val get_closure_code : conn -> remote_value -> pc Lwt.t

  val set_fork_mode : conn -> fork_mode -> unit Lwt.t
end
