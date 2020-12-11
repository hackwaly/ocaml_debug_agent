open Types

let get_pid conn =
  let%lwt neg_one = Lwt_io.BE.read_int conn.in_ in
  [%lwt assert (neg_one = -1)] ;%lwt
  let%lwt pid = Lwt_io.BE.read_int conn.in_ in
  Lwt.return pid

let set_event conn pc =
  Lwt_io.write_char conn.out 'e' ;%lwt
  Lwt_io.BE.write_int conn.out pc.frag ;%lwt
  Lwt_io.BE.write_int conn.out pc.pos

let set_breakpoint conn pc =
  Lwt_io.write_char conn.out 'B' ;%lwt
  Lwt_io.BE.write_int conn.out pc.frag ;%lwt
  Lwt_io.BE.write_int conn.out pc.pos

let reset_instr conn pc =
  Lwt_io.write_char conn.out 'i' ;%lwt
  Lwt_io.BE.write_int conn.out pc.frag ;%lwt
  Lwt_io.BE.write_int conn.out pc.pos

let checkpoint conn =
  assert (not Sys.win32) ;
  let%lwt pid = Lwt_io.BE.read_int conn.in_ in
  Lwt.return (if pid = -1 then Checkpoint_failed else Checkpoint_done pid)

let go conn n =
  Lwt_io.write_char conn.out 'g' ;%lwt
  Lwt_io.BE.write_int conn.out n ;%lwt
  let%lwt summary =
    match%lwt Lwt_io.read_char conn.in_ with
    | 'e' ->
        Lwt.return Event
    | 'b' ->
        Lwt.return Breakpoint
    | 'x' ->
        Lwt.return Exited
    | 's' ->
        Lwt.return Trap
    | 'u' ->
        Lwt.return Uncaught_exc
    | 'D' ->
        let%lwt debug_info = Lwt_io.read_value conn.in_ in
        Lwt.return (Code_debug_info debug_info)
    | 'L' ->
        let%lwt frag = Lwt_io.BE.read_int conn.in_ in
        Lwt.return (Code_loaded frag)
    | 'U' ->
        let%lwt frag = Lwt_io.BE.read_int conn.in_ in
        Lwt.return (Code_unloaded frag)
    | _ ->
        [%lwt assert false]
  in
  let%lwt event_counter = Lwt_io.BE.read_int conn.in_ in
  let%lwt stack_pos = Lwt_io.BE.read_int conn.in_ in
  let%lwt frag = Lwt_io.BE.read_int conn.in_ in
  let%lwt pos = Lwt_io.BE.read_int conn.in_ in
  Lwt.return
    { rep_type= summary
    ; rep_event_count= Int64.of_int event_counter
    ; rep_stack_pointer= stack_pos
    ; rep_program_pointer= {frag; pos} }

let stop conn = Lwt_io.write_char conn.out 's'

let wait conn = Lwt_io.write_char conn.out 'w'

let initial_frame conn =
  Lwt_io.write_char conn.out '0' ;%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn.in_ in
  let%lwt frag = Lwt_io.BE.read_int conn.in_ in
  let%lwt pos = Lwt_io.BE.read_int conn.in_ in
  Lwt.return (stack_pos, {frag; pos})

let get_frame conn =
  Lwt_io.write_char conn.out 'f' ;%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn.in_ in
  let%lwt frag = Lwt_io.BE.read_int conn.in_ in
  let%lwt pos = Lwt_io.BE.read_int conn.in_ in
  Lwt.return (stack_pos, {frag; pos})

let set_frame conn stack_pos =
  Lwt_io.write_char conn.out 'S' ;%lwt
  Lwt_io.BE.write_int conn.out stack_pos

let up_frame conn stacksize =
  Lwt_io.write_char conn.out 'U' ;%lwt
  Lwt_io.BE.write_int conn.out stacksize

let set_trap_barrier conn pos =
  Lwt_io.write_char conn.out 'b' ;%lwt
  Lwt_io.BE.write_int conn.out pos

let get_local conn index =
  Lwt_io.write_char conn.out 'L' ;%lwt
  Lwt_io.BE.write_int conn.out index ;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.in_ in
  Lwt.return rv

let get_environment conn index =
  Lwt_io.write_char conn.out 'E' ;%lwt
  Lwt_io.BE.write_int conn.out index ;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.in_ in
  Lwt.return rv

let get_global conn index =
  Lwt_io.write_char conn.out 'G' ;%lwt
  Lwt_io.BE.write_int conn.out index ;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.in_ in
  Lwt.return rv

let get_accu conn =
  Lwt_io.write_char conn.out 'A' ;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.in_ in
  Lwt.return rv

let get_header conn rv =
  Lwt_io.write_char conn.out 'H' ;%lwt
  Lwt_util.write_nativeint_be conn.out rv ;%lwt
  let%lwt hdr = Lwt_io.BE.read_int conn.in_ in
  Lwt.return hdr

let get_field conn rv index =
  Lwt_io.write_char conn.out 'F' ;%lwt
  Lwt_util.write_nativeint_be conn.out rv ;%lwt
  Lwt_io.BE.write_int conn.out index ;%lwt
  match%lwt Lwt_io.read_char conn.in_ with
  | '\000' ->
      let%lwt rv = Lwt_util.read_nativeint_be conn.in_ in
      Lwt.return (Remote_value rv)
  | '\001' ->
      let%lwt v = Lwt_io.read_float64 conn.in_ in
      Lwt.return (Double v)
  | _ ->
      [%lwt assert false]

let marshal_obj conn rv =
  Lwt_io.write_char conn.out 'M' ;%lwt
  Lwt_util.write_nativeint_be conn.out rv ;%lwt
  let%lwt v = Lwt_io.read_value conn.in_ in
  Lwt.return v

let get_closure_code conn rv =
  Lwt_io.write_char conn.out 'M' ;%lwt
  Lwt_util.write_nativeint_be conn.out rv ;%lwt
  let%lwt frag = Lwt_io.BE.read_int conn.in_ in
  let%lwt pos = Lwt_io.BE.read_int conn.in_ in
  Lwt.return {frag; pos}

let set_fork_mode conn mode =
  Lwt_io.write_char conn.out 'K' ;%lwt
  Lwt_io.BE.write_int conn.out
    (match mode with Fork_child -> 0 | Fork_parent -> 1)
