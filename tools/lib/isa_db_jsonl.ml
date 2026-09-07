type provenance = { extension : string option; group : string option }

type record = {
  record_id : string;
  source : string;
  kind : string;
  native_name : string;
  provenance : provenance;
}

let empty_provenance = { extension = None; group = None }

let provenance_jsont : provenance Jsont.t =
  Jsont.Object.map (fun extension group -> { extension; group })
  |> Jsont.Object.opt_mem "extension" Jsont.string
  |> Jsont.Object.opt_mem "group" Jsont.string
  |> Jsont.Object.finish

let record_jsont : record Jsont.t =
  Jsont.Object.map (fun record_id source kind native_name provenance ->
      { record_id; source; kind; native_name; provenance })
  |> Jsont.Object.mem "record_id" Jsont.string
  |> Jsont.Object.mem "source" Jsont.string
  |> Jsont.Object.mem "kind" Jsont.string
  |> Jsont.Object.mem "native_name" Jsont.string
  |> Jsont.Object.mem "provenance" provenance_jsont ~dec_absent:empty_provenance
  |> Jsont.Object.finish

let fail ?path detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path Tool_error.Parse detail)

(* export/writer.py always ends the file with a newline after its last line,
   same as every other line - so splitting on '\n' leaves one trailing empty
   element to drop, not a record to decode. *)
let lines_of_text text =
  match String.split_on_char '\n' text with
  | parts -> ( match List.rev parts with "" :: rest -> List.rev rest | _ -> parts)

let read_file path =
  let ( let* ) = Result.bind in
  let* text = Tool_fs.read path in
  let rec go acc lineno = function
    | [] -> Ok (List.rev acc)
    | line :: rest -> (
        match Jsont_bytesrw.decode_string record_jsont line with
        | Ok r -> go (r :: acc) (lineno + 1) rest
        | Error msg -> fail ~path (Printf.sprintf "line %d: %s" lineno msg))
  in
  go [] 1 (lines_of_text text)
