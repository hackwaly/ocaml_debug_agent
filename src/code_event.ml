open Remote_debugger

type t = { frag : int; event : Instruct.debug_event }

let stacksize t = t.event.ev_stacksize

let to_pc t = { frag = t.frag; pos = t.event.ev_pos }

let lexing_location t =
  t.event.ev_loc

let lexing_position t =
  Debug_event.lexing_position t.event

let defname t =
  t.event.ev_defname
