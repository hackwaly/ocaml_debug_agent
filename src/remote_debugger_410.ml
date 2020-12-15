open Types

let get_pid conn =
  Log.debug (fun m -> m "get_pid");%lwt
  let%lwt neg_one = Lwt_io.BE.read_int conn.in_ in
  [%lwt assert (neg_one = -1)] ;%lwt
  let%lwt pid = Lwt_io.BE.read_int conn.in_ in
  Lwt.return pid

let set_event conn pc =
  Log.debug (fun m -> m "set_event pc:%s" (show_pc pc));%lwt
  Lwt_io.write_char conn.out 'e' ;%lwt
  Lwt_io.BE.write_int conn.out pc.frag ;%lwt
  Lwt_io.BE.write_int conn.out pc.pos

let set_breakpoint conn pc =
  Log.debug (fun m -> m "set_breakpoint pc:%s" (show_pc pc));%lwt
  Lwt_io.write_char conn.out 'B' ;%lwt
  Lwt_io.BE.write_int conn.out pc.frag ;%lwt
  Lwt_io.BE.write_int conn.out pc.pos

let reset_instr conn pc =
  Log.debug (fun m -> m "reset_instr pc:%s" (show_pc pc));%lwt
  Lwt_io.write_char conn.out 'i' ;%lwt
  Lwt_io.BE.write_int conn.out pc.frag ;%lwt
  Lwt_io.BE.write_int conn.out pc.pos

let checkpoint conn =
  Log.debug (fun m -> m "checkpoint");%lwt
  assert (not Sys.win32) ;
  let%lwt pid = Lwt_io.BE.read_int conn.in_ in
  Lwt.return (if pid = -1 then Checkpoint_failed else Checkpoint_done pid)

let go conn n =
  Log.debug (fun m -> m "go n:%d" n);%lwt
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
        let%lwt eventlists = Lwt_io.read_value conn.in_ in
        Lwt.return (Code_debug_info {eventlists})
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
  Log.debug (fun m -> m "event_counter %d" event_counter);%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn.in_ in
  Log.debug (fun m -> m "stack_pos %d" stack_pos);%lwt
  let%lwt frag = Lwt_io.BE.read_int conn.in_ in
  Log.debug (fun m -> m "frag %d" frag);%lwt
  let%lwt pos = Lwt_io.BE.read_int conn.in_ in
  Log.debug (fun m -> m "pos %d" pos);%lwt
  Lwt.return
    { rep_type= summary
    ; rep_event_count= Int64.of_int event_counter
    ; rep_stack_pointer= stack_pos
    ; rep_program_pointer= {frag; pos} }

let stop conn =
  Log.debug (fun m -> m "stop");%lwt
  Lwt_io.write_char conn.out 's'

let wait conn =
  Log.debug (fun m -> m "wait");%lwt
  Lwt_io.write_char conn.out 'w'

let initial_frame conn =
  Log.debug (fun m -> m "initial_frame");%lwt
  Lwt_io.write_char conn.out '0' ;%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn.in_ in
  let%lwt frag = Lwt_io.BE.read_int conn.in_ in
  let%lwt pos = Lwt_io.BE.read_int conn.in_ in
  Lwt.return (stack_pos, {frag; pos})

let get_frame conn =
  Log.debug (fun m -> m "get_frame");%lwt
  Lwt_io.write_char conn.out 'f' ;%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn.in_ in
  let%lwt frag = Lwt_io.BE.read_int conn.in_ in
  let%lwt pos = Lwt_io.BE.read_int conn.in_ in
  Lwt.return (stack_pos, {frag; pos})

let set_frame conn stack_pos =
  Log.debug (fun m -> m "set_frame stack_pos:%d" stack_pos);%lwt
  Lwt_io.write_char conn.out 'S' ;%lwt
  Lwt_io.BE.write_int conn.out stack_pos

let up_frame conn stacksize =
  Log.debug (fun m -> m "up_frame stacksize:%d" stacksize);%lwt
  Lwt_io.write_char conn.out 'U' ;%lwt
  Lwt_io.BE.write_int conn.out stacksize

let set_trap_barrier conn pos =
  Log.debug (fun m -> m "set_trap_barrier pos:%d" pos);%lwt
  Lwt_io.write_char conn.out 'b' ;%lwt
  Lwt_io.BE.write_int conn.out pos

let get_local conn index =
  Log.debug (fun m -> m "get_local index:%d" index);%lwt
  Lwt_io.write_char conn.out 'L' ;%lwt
  Lwt_io.BE.write_int conn.out index ;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.in_ in
  Lwt.return rv

let get_environment conn index =
  Log.debug (fun m -> m "get_environment index:%d" index);%lwt
  Lwt_io.write_char conn.out 'E' ;%lwt
  Lwt_io.BE.write_int conn.out index ;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.in_ in
  Lwt.return rv

let get_global conn index =
  Log.debug (fun m -> m "get_environment index:%d" index);%lwt
  Lwt_io.write_char conn.out 'G' ;%lwt
  Lwt_io.BE.write_int conn.out index ;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.in_ in
  Lwt.return rv

let get_accu conn =
  Log.debug (fun m -> m "get_accu");%lwt
  Lwt_io.write_char conn.out 'A' ;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.in_ in
  Lwt.return rv

let get_header conn rv =
  Log.debug (fun m -> m "get_header rv:%s" (Nativeint.to_string rv));%lwt
  Lwt_io.write_char conn.out 'H' ;%lwt
  Lwt_util.write_nativeint_be conn.out rv ;%lwt
  let%lwt hdr = Lwt_io.BE.read_int conn.in_ in
  Lwt.return hdr

let get_field conn rv index =
  Log.debug (fun m -> m "get_field index:%d" index);%lwt
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
  Log.debug (fun m -> m "marshal_obj rv:%s" (Nativeint.to_string rv));%lwt
  Lwt_io.write_char conn.out 'M' ;%lwt
  Lwt_util.write_nativeint_be conn.out rv ;%lwt
  let%lwt v = Lwt_io.read_value conn.in_ in
  Lwt.return v

let get_closure_code conn rv =
  Log.debug (fun m -> m "get_closure_code rv:%s" (Nativeint.to_string rv));%lwt
  Lwt_io.write_char conn.out 'M' ;%lwt
  Lwt_util.write_nativeint_be conn.out rv ;%lwt
  let%lwt frag = Lwt_io.BE.read_int conn.in_ in
  let%lwt pos = Lwt_io.BE.read_int conn.in_ in
  Lwt.return {frag; pos}

let set_fork_mode conn mode =
  Log.debug (fun m -> m "set_fork_mode mode:%s" (show_fork_mode mode));%lwt
  Lwt_io.write_char conn.out 'K' ;%lwt
  Lwt_io.BE.write_int conn.out
    (match mode with Fork_child -> 0 | Fork_parent -> 1)
