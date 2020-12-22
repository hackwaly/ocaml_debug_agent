open Remote_debugger

type t = { frag : int; event : Instruct.debug_event }

let stacksize t = t.event.ev_stacksize

let to_pc t = { frag = t.frag; pos = t.event.ev_pos }
