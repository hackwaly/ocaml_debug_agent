open Types

type t = Breakpoints.breakpoint = {
  id : int;
  pc : pc;
  active_s : bool React.S.t;
  set_active : bool -> unit;
}

let id t = t.id

let make ~id ~pc () =
  let active_s, set_active = React.S.create false in
  { id; pc; active_s; set_active }

let active_signal b = b.active_s
