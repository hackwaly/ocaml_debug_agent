open Types

type pc = Types.pc

type breakpoint = {pc: pc}

let make_breakpoint ~pc () = {pc}

type t =
  { breakpoints: (pc, breakpoint) Hashtbl.t
  ; commit_queue: (pc, unit) Hashtbl.t
  ; committed: (pc, unit) Hashtbl.t }

let make () =
  { breakpoints= Hashtbl.create 0
  ; commit_queue= Hashtbl.create 0
  ; committed= Hashtbl.create 0 }

let set_breakpoint t b =
  Hashtbl.replace t.breakpoints b.pc b ;
  Hashtbl.replace t.commit_queue b.pc ()

let remove_breakpoint t b =
  Hashtbl.remove t.breakpoints b.pc ;
  Hashtbl.replace t.commit_queue b.pc ()

let commit t (module Rdbg : REMOTE_DEBUGGER) conn =
  let commit_one pc =
    let removed = not (Hashtbl.mem t.breakpoints pc) in
    let committed = Hashtbl.mem t.committed pc in
    match (removed, committed) with
    | true, true ->
        Rdbg.reset_instr conn pc ;%lwt Rdbg.set_event conn pc
    | false, false ->
        Rdbg.reset_instr conn pc ;%lwt
        Rdbg.set_breakpoint conn pc
    | _ ->
        Lwt.return ()
  in
  t.commit_queue |> Hashtbl.to_seq_keys |> List.of_seq
  |> Lwt_list.iter_s commit_one ;%lwt
  Hashtbl.reset t.commit_queue ;
  Lwt.return ()
