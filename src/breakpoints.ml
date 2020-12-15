open Types

type pc = Types.pc

type breakpoint = {pc: pc}

let make_breakpoint ~pc () = {pc}

type t =
  { breakpoint_by_pc: (pc, breakpoint) Hashtbl.t
  ; commit_queue: (pc, unit) Hashtbl.t
  ; committed: (pc, unit) Hashtbl.t }

let make () =
  { breakpoint_by_pc= Hashtbl.create 0
  ; commit_queue= Hashtbl.create 0
  ; committed= Hashtbl.create 0 }

let set_breakpoint t b =
  Hashtbl.replace t.breakpoint_by_pc b.pc b ;
  Hashtbl.replace t.commit_queue b.pc ()

let remove_breakpoint t b =
  Hashtbl.remove t.breakpoint_by_pc b.pc ;
  Hashtbl.replace t.commit_queue b.pc ()

let commit t (module Rdbg : REMOTE_DEBUGGER) conn =
  let commit_one pc =
    let removed = not (Hashtbl.mem t.breakpoint_by_pc pc) in
    let committed = Hashtbl.mem t.committed pc in
    match (removed, committed) with
    | true, true ->
        Rdbg.reset_instr conn pc ;%lwt
        Rdbg.set_event conn pc ;%lwt
        Hashtbl.remove t.committed pc |> Lwt.return
    | false, false ->
        Rdbg.reset_instr conn pc ;%lwt
        Rdbg.set_breakpoint conn pc ;%lwt
        Hashtbl.replace t.committed pc () |> Lwt.return
    | _ ->
        Lwt.return ()
  in
  t.commit_queue |> Hashtbl.to_seq_keys |> List.of_seq
  |> Lwt_list.iter_s commit_one ;%lwt
  Hashtbl.reset t.commit_queue ;
  Lwt.return ()
