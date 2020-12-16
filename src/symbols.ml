open Types

type module_info = {
  frag : int;
  id : string;
  resolved_source : string option;
  events : Instruct.debug_event array;
}

type lexing_pos = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
[@@deriving show]

type eventlist = { evl : Instruct.debug_event list; dirs : string list }

type t = {
  event_by_pc : (pc, Instruct.debug_event) Hashtbl.t;
  commit_queue : (pc, unit) Hashtbl.t;
  committed : (pc, unit) Hashtbl.t;
  module_info_by_id : (string, module_info) Hashtbl.t;
  module_info_by_digest : (string, module_info) Hashtbl.t;
  change_e : unit React.E.t;
  emit_change : unit -> unit;
  derive_source_paths : string -> string list -> string list Lwt.t;
  get_digest : string -> string Lwt.t;
  load_source : string -> (string * int array) Lwt.t;
}

let default_derive_source_paths mid dirs =
  dirs |> List.to_seq
  |> Seq.flat_map (fun dir ->
         List.to_seq
           [
             dir ^ "/" ^ String.uncapitalize_ascii mid ^ ".ml";
             dir ^ "/" ^ String.uncapitalize_ascii mid ^ ".re";
             dir ^ "/" ^ mid ^ ".ml";
             dir ^ "/" ^ mid ^ ".re";
           ])
  |> List.of_seq |> Lwt.return

let make ?(derive_source_paths = default_derive_source_paths) () =
  let change_e, emit_change = React.E.create () in
  let get_digest =
    Lwt_util.memo ~weight:String.length ~cap:(64 * 1024) (fun _rec source ->
        Lwt_preemptive.detach (fun source -> Digest.file source) source)
  in
  let load_source =
    let weight (content, _bols) = String.length content in
    Lwt_util.memo ~weight
      ~cap:(32 * 1024 * 1024)
      (fun _rec source ->
        let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.input source in
        let%lwt code = Lwt_io.read ic in
        let bols = ref [ 0 ] in
        for i = 0 to String.length code - 1 do
          if code.[i] = '\n' then (
            bols := (i + 1) :: !bols
          )
        done;
        let bols = !bols |> List.rev |> Array.of_list in
        Lwt.return (code, bols))
  in
  {
    event_by_pc = Hashtbl.create 0;
    commit_queue = Hashtbl.create 0;
    committed = Hashtbl.create 0;
    module_info_by_id = Hashtbl.create 0;
    module_info_by_digest = Hashtbl.create 0;
    change_e;
    emit_change;
    derive_source_paths;
    get_digest;
    load_source;
  }

let commit t (module Rdbg : REMOTE_DEBUGGER) conn =
  let commit_one pc =
    let committed = Hashtbl.mem t.committed pc in
    if%lwt Lwt.return (not committed) then (
      Hashtbl.replace t.committed pc ();
      Rdbg.set_event conn pc )
  in
  t.commit_queue |> Hashtbl.to_seq_keys |> List.of_seq
  |> Lwt_list.iter_s commit_one;%lwt
  Hashtbl.reset t.commit_queue;
  Lwt.return ()

let read_toc ic =
  let%lwt len = Lwt_io.length ic in
  let pos_trailer = Int64.sub len (Int64.of_int 16) in
  Lwt_io.set_position ic pos_trailer;%lwt
  let%lwt num_sections = Lwt_io.BE.read_int ic in
  let%lwt magic =
    Lwt_util.read_to_string_exactly ic (String.length Config.exec_magic_number)
  in
  if%lwt Lwt.return (magic <> Config.exec_magic_number) then
    Lwt.fail_invalid_arg "Bad magic";%lwt
  let pos_toc = Int64.sub pos_trailer (Int64.of_int (8 * num_sections)) in
  Lwt_io.set_position ic pos_toc;%lwt
  let section_table = ref [] in
  for%lwt i = 1 to num_sections do
    let%lwt name = Lwt_util.read_to_string_exactly ic 4 in
    let%lwt len = Lwt_io.BE.read_int ic in
    section_table := (name, len) :: !section_table;
    Lwt.return_unit
  done;%lwt
  Lwt.return (pos_toc, !section_table)

let seek_section (pos, section_table) name =
  let rec seek_sec pos = function
    | [] -> raise Not_found
    | (name', len) :: rest ->
        let pos = Int64.sub pos (Int64.of_int len) in
        if name' = name then pos else seek_sec pos rest
  in
  seek_sec pos section_table

let relocate_event orig ev =
  ev.Instruct.ev_pos <- orig + ev.Instruct.ev_pos;
  match ev.ev_repr with Event_parent repr -> repr := ev.ev_pos | _ -> ()

let partition_modules evl =
  let rec partition_modules' ev evl =
    match evl with
    | [] -> ([ ev ], [])
    | ev' :: evl ->
        let evl, evll = partition_modules' ev' evl in
        if ev.Instruct.ev_module = ev'.ev_module then (ev :: evl, evll)
        else ([ ev ], evl :: evll)
  in
  match evl with
  | [] -> []
  | ev :: evl ->
      let evl, evll = partition_modules' ev evl in
      evl :: evll

let read_eventlists toc ic =
  let pos = seek_section toc "DBUG" in
  Lwt_io.set_position ic pos;%lwt
  let%lwt num_eventlists = Lwt_io.BE.read_int ic in
  let eventlists = ref [] in
  for%lwt i = 1 to num_eventlists do
    let%lwt orig = Lwt_io.BE.read_int ic in
    let%lwt evl = Lwt_io.read_value ic in
    let evl = (evl : Instruct.debug_event list) in
    List.iter (relocate_event orig) evl;
    let%lwt dirs = Lwt_io.read_value ic in
    let dirs = (dirs : string list) in
    eventlists := { evl; dirs } :: !eventlists;
    Lwt.return ()
  done;%lwt
  Lwt.return (List.rev !eventlists)

let lexing_pos_of_event ev =
  match ev.Instruct.ev_kind with
  | Event_before -> ev.ev_loc.Location.loc_start
  | Event_after _ -> ev.ev_loc.Location.loc_end
  | _ -> ev.ev_loc.Location.loc_start

let cnum_of_event ev = (lexing_pos_of_event ev).Lexing.pos_cnum

let change_event t = t.change_e

let load t frag path =
  let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.input path in
  (let%lwt toc = read_toc ic in
   let%lwt eventlists = read_eventlists toc ic in
   eventlists
   |> Lwt_list.iter_s (fun { evl; dirs } ->
          partition_modules evl
          |> Lwt_list.iter_s (fun evl ->
                 let id = (List.hd evl).Instruct.ev_module in
                 let%lwt source_paths = t.derive_source_paths id dirs in
                 let%lwt resolved_source =
                   try%lwt
                     let%lwt source_path =
                       source_paths |> Lwt_list.find_s Lwt_unix.file_exists
                     in
                     Lwt.return (Some source_path)
                   with Not_found -> Lwt.return None
                 in
                 let is_pseudo_event ev =
                   match ev.Instruct.ev_kind with
                   | Event_pseudo -> true
                   | _ -> false
                 in
                 evl
                 |> List.iter (fun ev ->
                        let pc = { frag; pos = ev.Instruct.ev_pos } in
                        Hashtbl.replace t.event_by_pc pc ev;
                        Hashtbl.replace t.commit_queue pc ());
                 let events =
                   evl
                   |> List.filter (fun ev -> not (is_pseudo_event ev))
                   |> Array.of_list
                 in
                 Array.fast_sort (Compare.by cnum_of_event) events;
                 let module_info = { frag; id; resolved_source; events } in
                 Hashtbl.replace t.module_info_by_id id module_info;
                 ( match resolved_source with
                 | Some source ->
                     let%lwt digest = t.get_digest source in
                     Hashtbl.replace t.module_info_by_digest digest module_info;
                     Lwt.return ()
                 | None -> Lwt.return () );%lwt
                 Lwt.return ())))
    [%finally Lwt_io.close ic]

let src_pos_to_cnum t src_pos =
  let%lwt _, bols = t.load_source src_pos.source in
  let bol = bols.(src_pos.line - 1) in
  Lwt.return (bol + src_pos.column)

let find_module_info t src_pos =
  let%lwt digest = t.get_digest src_pos.source in
  Hashtbl.find t.module_info_by_digest digest |> Lwt.return

let expand_to_equivalent_range code cnum =
  (* TODO: Support skip comments *)
  let is_whitespace_or_semicolon c =
    match c with ' ' | '\t' | '\r' | '\n' | ';' -> true | _ -> false
  in
  Log.debug (fun m -> m "cnum:%d, len:%d" cnum (String.length code));%lwt
  assert (cnum >= 0 && cnum < String.length code);
  let c = code.[cnum] in
  if is_whitespace_or_semicolon c then
    let rec aux f n =
      let n' = f n in
      Log.debug (fun m -> m "expand_to_equivalent_range.aux n:%d" n');%lwt
      let c = code.[n'] in
      if is_whitespace_or_semicolon c then aux f n' else Lwt.return n
    in
    let%lwt l = aux (fun x -> x - 1) cnum in
    let%lwt r = aux (fun x -> x + 1) cnum in
    Lwt.return (l, r + 1)
  else Lwt.return (cnum, cnum)

let find_event code events cnum =
  Log.debug (fun m ->
      m "expand_to_equivalent_range events:%s" ([%show: lexing_pos array] (events |> Array.map lexing_pos_of_event)));%lwt
  Log.debug (fun m ->
      m "expand_to_equivalent_range code:%s cnum:%d len:%d" code cnum
        (String.length code));%lwt
  let%lwt l, r = expand_to_equivalent_range code cnum in
  Log.debug (fun m ->
      m "expand_to_equivalent_range code:%s cnum:%d l:%d, r:%d" code cnum l r);%lwt
  assert (l <= r);
  let cmp ev () =
    let ev_cnum = cnum_of_event ev in
    if ev_cnum < l then -1 else if ev_cnum > r then 1 else 0
  in
  Lwt.return
    ( match events |> Array_util.bsearch ~cmp () with
    | `At i -> events.(i)
    | _ -> raise Not_found )

let resolve t src_pos =
  try%lwt
    let%lwt mi = find_module_info t src_pos in
    Log.debug (fun m -> m "resolve.find_module_info success");%lwt
    let%lwt code, _ = t.load_source src_pos.source in
    Log.debug (fun m -> m "resolve.load_source success");%lwt
    let%lwt cnum = src_pos_to_cnum t src_pos in
    Log.debug (fun m -> m "expand_to_equivalent_range.src_pos_to_cnum src_pos:%s, cnum:%d" (show_src_pos src_pos) cnum);%lwt
    let%lwt ev = find_event code mi.events cnum in
    Log.debug (fun m -> m "resolve.find_event success");%lwt
    let ev_pos = lexing_pos_of_event ev in
    let pc = { frag = mi.frag; pos = ev.Instruct.ev_pos } in
    let src_pos' =
      {
        source = mi.resolved_source |> Option.value ~default:src_pos.source;
        line = ev_pos.Lexing.pos_lnum + 1;
        column = ev_pos.Lexing.pos_cnum - ev_pos.Lexing.pos_bol;
      }
    in
    Lwt.return (Some (pc, src_pos'))
  with Not_found -> Lwt.return None

let sources t =
  t.module_info_by_id |> Hashtbl.to_seq_values
  |> Seq.filter_map (fun mi -> mi.resolved_source)
  |> List.of_seq |> Lwt.return
