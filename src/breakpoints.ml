open Types

type pc = Types.pc

type breakpoint = {
  id : int;
  pc : pc;
  active_s : bool React.S.t;
  set_active : bool -> unit;
}

module Breakpoint = struct
  type t = breakpoint

  let id t = t.id

  let make ~id ~pc () =
    let active_s, set_active = React.S.create false in
    { id; pc; active_s; set_active }

  let active_signal b = b.active_s
end

type t = {
  breakpoint_by_pc : (pc, breakpoint) Hashtbl.t;
  commit_queue : (pc, unit) Hashtbl.t;
  committed : (pc, unit) Hashtbl.t;
}

let make () =
  {
    breakpoint_by_pc = Hashtbl.create 0;
    commit_queue = Hashtbl.create 0;
    committed = Hashtbl.create 0;
  }

let set_breakpoint t b =
  Hashtbl.replace t.breakpoint_by_pc b.pc b;
  Hashtbl.replace t.commit_queue b.pc ();
  Lwt.return ()

let remove_breakpoint t b =
  Hashtbl.remove t.breakpoint_by_pc b.pc;
  Hashtbl.replace t.commit_queue b.pc ();
  b.set_active false;
  Lwt.return ()

(* TODO: Conditional breakpoint *)
let check_breakpoint t pc =
  Lwt.return (Hashtbl.find_opt t.breakpoint_by_pc pc)

let commit t (module Rdbg : REMOTE_DEBUGGER) =
  let commit_one pc =
    let removed = not (Hashtbl.mem t.breakpoint_by_pc pc) in
    let committed = Hashtbl.mem t.committed pc in
    Log.debug (fun m ->
        m "pc: (%d,%d), removed: %b, committed: %b" pc.frag pc.pos removed
          committed);%lwt
    match (removed, committed) with
    | true, true ->
        Rdbg.reset_instr pc;%lwt
        Rdbg.set_event pc;%lwt
        Hashtbl.remove t.committed pc |> Lwt.return
    | false, false ->
        Rdbg.reset_instr pc;%lwt
        Rdbg.set_breakpoint pc;%lwt
        let bp = Hashtbl.find t.breakpoint_by_pc pc in
        bp.set_active true;
        Hashtbl.replace t.committed pc () |> Lwt.return
    | _ -> Lwt.return ()
  in
  Log.debug (fun m -> m "breakpoints commit start");%lwt
  t.commit_queue |> Hashtbl.to_seq_keys |> List.of_seq
  |> Lwt_list.iter_s commit_one;%lwt
  Hashtbl.reset t.commit_queue;
  Log.debug (fun m -> m "breakpoints commit start");%lwt
  Lwt.return ()
