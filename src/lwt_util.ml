let read_nativeint_be in_ =
  if Sys.word_size = 64 then
    let%lwt word = Lwt_io.BE.read_int64 in_ in
    Lwt.return (Int64.to_nativeint word)
  else
    let%lwt word = Lwt_io.BE.read_int32 in_ in
    Lwt.return (Nativeint.of_int32 word)

let write_nativeint_be out n =
  if Sys.word_size = 64 then
    let word = Int64.of_nativeint n in
    Lwt_io.BE.write_int64 out word
  else
    let word = Nativeint.to_int32 n in
    Lwt_io.BE.write_int32 out word

let read_to_string_exactly ic count =
  let buf = Bytes.create count in
  Lwt_io.read_into_exactly ic buf 0 count ;%lwt
  Lwt.return (Bytes.to_string buf)

let resolve_in_dirs fname dirs =
  let rec rec_resolve fname dirs =
    match dirs with
    | dir :: dirs ->
        let path = dir ^ "/" ^ fname in
        if%lwt Lwt_unix.file_exists path then Lwt.return (Some path)
        else rec_resolve fname dirs
    | [] ->
        Lwt.return None
  in
  rec_resolve fname dirs
