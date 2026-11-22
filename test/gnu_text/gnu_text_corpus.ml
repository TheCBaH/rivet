(* The RV32 cases of the isa-difficult corpus as GNU source printed from the typed module: each
   case's text is read into a normalized module, printed by [Gnu_module] with
   [Instruction.pp_gnu], and the printed text is what both assemblers are asked to read back. The
   reference bytes are the ones GNU as produced for the case's original text. *)

module P = Pipeline.Make (Riscv32)

type case = {
  id : string;
  source : string;
  argv : string list;
  features : Rivet_target_features.Config.spec option;
  bytes : string;
}

let render ds =
  String.concat "; "
    (List.map
       (fun d ->
         Printf.sprintf "%s: %s"
           (Rivet_foundation.Diagnostic.code d)
           (Rivet_foundation.Diagnostic.message d))
       (Rivet_foundation.Diag.diagnostics ds))

(* {1 The JSON lines, read just far enough} *)

let find_from line ~from s =
  let n = String.length s and len = String.length line in
  let rec go i =
    if i + n > len then None else if String.sub line i n = s then Some (i + n) else go (i + 1)
  in
  go from

(* The JSON string starting at [i] (just past its opening quote), unescaped, and the index past its
   closing quote. *)
let json_string line i =
  let b = Buffer.create 64 in
  let rec go i =
    match line.[i] with
    | '"' -> (Buffer.contents b, i + 1)
    | '\\' ->
        (match line.[i + 1] with
        | 'n' -> Buffer.add_char b '\n'
        | 't' -> Buffer.add_char b '\t'
        | c -> Buffer.add_char b c);
        go (i + 2)
    | c ->
        Buffer.add_char b c;
        go (i + 1)
  in
  go i

let string_field ?(from = 0) line key =
  Option.map
    (fun i -> fst (json_string line i))
    (find_from line ~from (Printf.sprintf "\"%s\":\"" key))

let strings_field line key =
  match find_from line ~from:0 (Printf.sprintf "\"%s\":[" key) with
  | None -> []
  | Some i ->
      let rec go i acc =
        match line.[i] with
        | ']' -> List.rev acc
        | '"' ->
            let s, j = json_string line (i + 1) in
            go j (s :: acc)
        | _ -> go (i + 1) acc
      in
      go i []

let bytes_of_hex hex =
  let digits = String.concat "" (String.split_on_char ' ' (String.trim hex)) in
  String.init
    (String.length digits / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub digits (2 * i) 2)))

let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

(* Rivet's default configuration has every component on except the ones that conflict with C
   (Zcmp and Zcmt reuse C.FSD's encodings); a case built for one of those names it in [-march]. *)
let features_of argv =
  let has s =
    List.exists
      (fun a -> String.starts_with ~prefix:"-march=" a && Option.is_some (find_from a ~from:0 s))
      argv
  in
  let spec text =
    match Rivet_target_features.Config.parse_spec text with Ok s -> s | Error m -> failwith m
  in
  if has "zcmp" then Some (spec "-c,-zcd,+zcmp")
  else if has "zcmt" then Some (spec "-c,-zcd,+zcmt")
  else None

let case_of_line ~target line =
  let gas = Option.value ~default:max_int (find_from line ~from:0 "\"gas\":") in
  match
    ( string_field line "target",
      find_from line ~from:0 "\"negative\":false",
      find_from line ~from:gas "\"exit_status\":{\"kind\":\"exited\",\"code\":0}",
      string_field line "case_id",
      string_field line "rendered_source",
      string_field ~from:gas line "bytes" )
  with
  | Some t, Some _, Some _, Some id, Some source, Some bytes when String.equal t target ->
      let argv = strings_field line "configuration" in
      Some { id; source; argv; features = features_of argv; bytes = bytes_of_hex bytes }
  | _ -> None

let load ?(target = "riscv32") path =
  In_channel.with_open_text path (fun ic ->
      let rec go acc =
        match In_channel.input_line ic with
        | None -> List.rev acc
        | Some l -> go (match case_of_line ~target l with Some c -> c :: acc | None -> acc)
      in
      go [])

(* {1 Printing and reading back} *)

let gnu_syntax = { Rivet_asm_core.Gnu_module.type_char = '@' }

let print ?features ~instruction source =
  let src = Rivet_foundation.Span.source ~name:"case.s" ~contents:source in
  match (P.initial_state features, P.parse ~unit_name:"case" ~source:src) with
  | Error ds, _ | _, Error ds -> Error (render ds)
  | Ok state, Ok m -> (
      match P.simplify ~state m with
      | Error ds -> Error (render ds)
      | Ok (n, _) -> Ok (Rivet_asm_core.Gnu_module.to_string gnu_syntax ~instruction n))

(* The bytes of a one-section source bound at 0, as the corpus's own assembler run binds it. *)
let assemble ?features (module D : Rivet_target_intf.Target.DRIVER) text =
  let source = Rivet_foundation.Span.source ~name:"gnu.s" ~contents:text in
  match D.assemble ?features ~unit_name:"gnu" ~source () with
  | Error ds -> Error (render ds)
  | Ok laid_out -> (
      let plan = Rivet_image.plan_of laid_out in
      let addresses =
        List.map
          (fun (s : Rivet_image.segment_plan) -> (s.Rivet_image.seg_name, 0L))
          plan.Rivet_image.segments
      in
      match Rivet_image.bind_image laid_out ~addresses with
      | Error ds -> Error (render ds)
      | Ok { Rivet_image.segments = [ s ]; _ } -> Ok s.Rivet_image.bytes
      | Ok { Rivet_image.segments = []; _ } -> Ok ""
      | Ok _ -> Error "more than one segment")
