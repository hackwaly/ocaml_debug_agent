open Remote_debugger

type breakpoint = {
  pc : pc;
}

type t = {
  bp_by_pc : (pc, breakpoint) Hashtbl.t;
  commit_queue : (pc, unit) Hashtbl.t;
  committed : (pc, unit) Hashtbl.t;
}

let commit t (module Remote_debugger : Remote_debugger.S) conn =
  let commit_one pc =
    let removed = not (Hashtbl.mem t.bp_by_pc pc) in
    let committed = Hashtbl.mem t.committed pc in
    match (removed, committed) with
    | true, true ->
        Remote_debugger.reset_instr conn pc;%lwt
        Remote_debugger.set_event conn pc;%lwt
        Hashtbl.remove t.committed pc |> Lwt.return
    | false, false ->
        Remote_debugger.reset_instr conn pc;%lwt
        Remote_debugger.set_breakpoint conn pc;%lwt
        Hashtbl.replace t.committed pc () |> Lwt.return
    | _ -> Lwt.return ()
  in
  t.commit_queue |> Hashtbl.to_seq_keys |> Lwt_util.iter_seq_s commit_one
