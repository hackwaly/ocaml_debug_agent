open Types

type module_info =
  { frag: int
  ; id: string
  ; source_path: string
  ; source_lines: string array
  ; events: Instruct.debug_event array }

type eventlist = {orig: int; evl: Instruct.debug_event list; dirs: string list}

type t =
  { event_by_pc: (pc, Instruct.debug_event) Hashtbl.t
  ; module_info_tbl: (string, module_info) Hashtbl.t
  ; change_e: unit React.E.t
  ; emit_change: unit -> unit
  ; derive_source_paths: string -> string list -> string list Lwt.t }

let default_derive_source_paths mid dirs =
  dirs |> List.to_seq
  |> Seq.flat_map (fun dir ->
         List.to_seq
           [ dir ^ "/" ^ String.uncapitalize_ascii mid ^ ".ml"
           ; dir ^ "/" ^ String.uncapitalize_ascii mid ^ ".re"
           ; dir ^ "/" ^ mid ^ ".ml"
           ; dir ^ "/" ^ mid ^ ".re" ])
  |> List.of_seq |> Lwt.return

let make ?(derive_source_paths = default_derive_source_paths) () =
  let change_e, emit_change = React.E.create () in
  { event_by_pc= Hashtbl.create 0
  ; module_info_tbl= Hashtbl.create 0
  ; change_e
  ; emit_change
  ; derive_source_paths }

let read_toc ic =
  let%lwt len = Lwt_io.length ic in
  let pos_trailer = Int64.sub len (Int64.of_int 16) in
  Lwt_io.set_position ic pos_trailer ;%lwt
  let%lwt num_sections = Lwt_io.BE.read_int ic in
  let%lwt magic =
    Lwt_util.read_to_string_exactly ic (String.length Config.exec_magic_number)
  in
  if%lwt Lwt.return (magic <> Config.exec_magic_number) then
    Lwt.fail_invalid_arg "Bad magic" ;%lwt
  let pos_toc = Int64.sub pos_trailer (Int64.of_int (8 * num_sections)) in
  Lwt_io.set_position ic pos_toc ;%lwt
  let section_table = ref [] in
  for%lwt i = 1 to num_sections do
    let%lwt name = Lwt_util.read_to_string_exactly ic 4 in
    let%lwt len = Lwt_io.BE.read_int ic in
    section_table := (name, len) :: !section_table ;
    Lwt.return_unit
  done ;%lwt
  Lwt.return (pos_toc, !section_table)

let seek_section (pos, section_table) name =
  let rec seek_sec pos = function
    | [] ->
        raise Not_found
    | (name', len) :: rest ->
        let pos = Int64.sub pos (Int64.of_int len) in
        if name' = name then pos else seek_sec pos rest
  in
  seek_sec pos section_table

let relocate_event orig ev =
  ev.Instruct.ev_pos <- orig + ev.Instruct.ev_pos ;
  match ev.ev_repr with Event_parent repr -> repr := ev.ev_pos | _ -> ()

let partition_modules evl =
  let rec partition_modules' ev evl =
    match evl with
    | [] ->
        ([ev], [])
    | ev' :: evl ->
        let evl, evll = partition_modules' ev' evl in
        if ev.Instruct.ev_module = ev'.ev_module then (ev :: evl, evll)
        else ([ev], evl :: evll)
  in
  match evl with
  | [] ->
      []
  | ev :: evl ->
      let evl, evll = partition_modules' ev evl in
      evl :: evll

let read_eventlists toc ic =
  let pos = seek_section toc "DBUG" in
  Lwt_io.set_position ic pos ;%lwt
  let%lwt num_eventlists = Lwt_io.BE.read_int ic in
  let eventlists = ref [] in
  for%lwt i = 1 to num_eventlists do
    let%lwt orig = Lwt_io.BE.read_int ic in
    let%lwt evl = Lwt_io.read_value ic in
    let evl = (evl : Instruct.debug_event list) in
    List.iter (relocate_event orig) evl ;
    let%lwt dirs = Lwt_io.read_value ic in
    let dirs = (dirs : string list) in
    eventlists := {orig; evl; dirs} :: !eventlists ;
    Lwt.return ()
  done ;%lwt
  Lwt.return !eventlists

let pos_of_event ev =
  match ev.Instruct.ev_kind with
  | Event_before ->
      ev.ev_loc.Location.loc_start
  | Event_after _ ->
      ev.ev_loc.Location.loc_end
  | _ ->
      ev.ev_loc.Location.loc_start

let load t frag path =
  let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.input path in
  let%lwt toc = read_toc ic in
  let%lwt eventlists = read_eventlists toc ic in
  eventlists
  |> Lwt_list.iter_s (fun {orig; evl; dirs} ->
         List.iter (relocate_event orig) evl ;
         partition_modules evl
         |> Lwt_list.iter_s (fun evl ->
                let id = (List.hd evl).Instruct.ev_module in
                let%lwt source_paths = t.derive_source_paths id dirs in
                let%lwt source_path =
                  try%lwt
                    let%lwt source_path =
                      source_paths |> Lwt_list.find_s Lwt_unix.file_exists
                    in
                    Lwt.return (Some source_path)
                  with Not_found -> Lwt.return None
                in
                let%lwt source_lines =
                  match%lwt Lwt.return source_path with
                  | Some source_path ->
                      let%lwt source_ic =
                        Lwt_io.open_file ~mode:Lwt_io.input source_path
                      in
                      let%lwt lines =
                        source_ic |> Lwt_io.read_lines |> Lwt_stream.to_list
                      in
                      Lwt.return (Array.of_list lines)
                  | None ->
                      Lwt.return [||]
                in
                let events = evl |> Array.of_list in
                Array.fast_sort (Compare.by pos_of_event) events ;
                let module_info =
                  { frag
                  ; id
                  ; source_path= source_path |> Option.value ~default:"(none)"
                  ; source_lines
                  ; events }
                in
                Hashtbl.replace t.module_info_tbl id module_info ;
                Lwt.return ()))

let resolve t src_pos =
  ignore t ;
  ignore src_pos ;
  assert false
