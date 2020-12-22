type t = { index : int; stack_pos : int; event : Code_event.t }

let stacksize t = t.event.event.ev_stacksize

let defname t = t.event.event.ev_defname

let loc t =
  if t.index = 0 then
    let pos = Debug_event.lexing_position t.event.event in
    Location.{ loc_start = pos; loc_end = pos; loc_ghost = false }
  else t.event.event.ev_loc
