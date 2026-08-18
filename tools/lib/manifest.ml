type key =
  | Generator
  | Source
  | Inputs
  | Ccomp_version of Target.t
  | Ccomp_target of Target.t
  | Ccomp_args of Target.t
  | Ccomp_configure_args of Target.t
  | Sha256 of string
  | Unknown of string

type record = { key : key; value : string option }
type t = record list

let records t = t
let of_records rs = rs

(* {1 Escaping}

   The shell's esc() is `s/\\/\\\\/g; s/\t/\\t/g; s/$/\\n/`, applied in that
   order. Backslash MUST come first: escaping the tab first would then have its
   introduced backslash escaped again. *)
let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '\t' -> Buffer.add_string b "\\t"
      | '\n' -> Buffer.add_string b "\\n"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let fail_parse detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v Tool_error.Parse detail)

(* D7: the exact inverse, and it REJECTS anything else. The shell's reader never
   unescapes at all, so a value containing "\q" round-trips through it
   unnoticed; here it is a parse error, because a reader that silently accepted
   an escape it could not have produced would mask a corrupted manifest. *)
let unescape s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then Ok (Buffer.contents b)
    else if s.[i] <> '\\' then (
      Buffer.add_char b s.[i];
      go (i + 1))
    else if i + 1 >= n then fail_parse (Printf.sprintf "value %S ends with a lone backslash" s)
    else
      match s.[i + 1] with
      | '\\' ->
          Buffer.add_char b '\\';
          go (i + 2)
      | 't' ->
          Buffer.add_char b '\t';
          go (i + 2)
      | 'n' ->
          Buffer.add_char b '\n';
          go (i + 2)
      | c -> fail_parse (Printf.sprintf "value %S contains an unknown escape \\%c" s c)
  in
  go 0

(* {2 Keys} *)

let key_prefix = function
  | Generator -> "generator"
  | Source -> "source"
  | Inputs -> "inputs"
  | Ccomp_version t -> "ccomp-version:" ^ Target.to_string t
  | Ccomp_target t -> "ccomp-target:" ^ Target.to_string t
  | Ccomp_args t -> "ccomp-args:" ^ Target.to_string t
  | Ccomp_configure_args t -> "ccomp-configure-args:" ^ Target.to_string t
  | Sha256 p -> "sha256:" ^ p
  | Unknown k -> k

let split_prefix s prefix =
  let n = String.length prefix in
  if String.length s > n && String.sub s 0 n = prefix then
    Some (String.sub s n (String.length s - n))
  else None

let parse_key raw =
  match raw with
  | "generator" -> Ok Generator
  | "source" -> Ok Source
  | "inputs" -> Ok Inputs
  | _ -> (
      let targeted prefix build =
        match split_prefix raw prefix with
        | None -> None
        | Some name -> (
            match Target.of_string name with
            | Ok t -> Some (Ok (build t))
            | Error _ ->
                Some (fail_parse (Printf.sprintf "record %S names an unknown target %S" raw name)))
      in
      match targeted "ccomp-version:" (fun t -> Ccomp_version t) with
      | Some r -> r
      | None -> (
          match targeted "ccomp-target:" (fun t -> Ccomp_target t) with
          | Some r -> r
          | None -> (
              match targeted "ccomp-configure-args:" (fun t -> Ccomp_configure_args t) with
              | Some r -> r
              | None -> (
                  match targeted "ccomp-args:" (fun t -> Ccomp_args t) with
                  | Some r -> r
                  | None -> (
                      match split_prefix raw "sha256:" with
                      | Some path -> (
                          (* D6: corpus paths are validated. The key side has no
                             escape layer, so a TAB or newline here could not
                             round-trip - it would silently become a different
                             record, or two. *)
                          match Identifier.relative_path path with
                          | Ok p -> Ok (Sha256 p)
                          | Error e ->
                              fail_parse
                                (Printf.sprintf "record %S has an unusable path: %s" raw
                                   (Err.Error.kind e).Tool_error.detail))
                      | None ->
                          (* P2: Unknown records parse, and are DROPPED on
                             rehash - write_manifest reconstructs known records
                             only, so preserving them here would invent a
                             behavior the shell does not have. *)
                          Ok (Unknown raw))))))

let is_hex_digit c = match c with '0' .. '9' | 'a' .. 'f' -> true | _ -> false

let check_hash raw v =
  if String.length v = 64 && String.for_all is_hex_digit v then Ok ()
  else fail_parse (Printf.sprintf "record %S has a malformed SHA-256 value %S" raw v)

(* {3 Parsing} *)

let parse text =
  if text = "" then Ok []
  else if text.[String.length text - 1] <> '\n' then
    (* D4: the shell's `while read` loop drops an unterminated final record
       silently, which is how a truncated manifest reads as a shorter one. *)
    Err.Accum.lift (fail_parse "manifest does not end with a newline")
  else begin
    let lines = String.split_on_char '\n' (String.sub text 0 (String.length text - 1)) in
    let numbered = List.mapi (fun i l -> (i + 1, l)) lines in
    let parse_line (lineno, line) =
      let at fmt =
        Printf.ksprintf (fun m -> fail_parse (Printf.sprintf "line %d: %s" lineno m)) fmt
      in
      if line = "" then at "empty record"
      else
        let raw_key, raw_value =
          match String.index_opt line '\t' with
          | None -> (line, None)
          | Some i ->
              (String.sub line 0 i, Some (String.sub line (i + 1) (String.length line - i - 1)))
        in
        if String.exists (fun c -> c = '\000') raw_key then at "key contains NUL"
        else
          match parse_key raw_key with
          | Error e -> at "%s" (Err.Error.kind e).Tool_error.detail
          | Ok key -> (
              match raw_value with
              | None -> Ok { key; value = None }
              | Some v -> (
                  match unescape v with
                  | Error e -> at "%s" (Err.Error.kind e).Tool_error.detail
                  | Ok decoded -> (
                      match key with
                      | Sha256 _ -> (
                          match check_hash raw_key decoded with
                          | Ok () -> Ok { key; value = Some decoded }
                          | Error e -> at "%s" (Err.Error.kind e).Tool_error.detail)
                      | _ -> Ok { key; value = Some decoded })))
    in
    match Err.Accum.map parse_line numbered with
    | Error _ as e -> e
    | Ok rs ->
        (* Duplicate detection is a whole-manifest property, so it runs after
           every line has had its say - a manifest with two malformed records
           AND a duplicate should report all three. *)
        let dups =
          let seen_hash = Hashtbl.create 64 and seen_version = Hashtbl.create 8 in
          List.filter_map
            (fun r ->
              match r.key with
              | Sha256 p ->
                  if Hashtbl.mem seen_hash p then
                    Some (fail_parse (Printf.sprintf "duplicate sha256 record for %S" p))
                  else (
                    Hashtbl.add seen_hash p ();
                    None)
              (* D12: the shell's carry-forward is `grep "^ccomp-version:$t\t"`,
                 which emits EVERY match, so today all duplicates are carried
                 forward verbatim and the manifest grows. *)
              | Ccomp_version t ->
                  if Hashtbl.mem seen_version t then
                    Some
                      (fail_parse
                         (Printf.sprintf "duplicate ccomp-version record for %s"
                            (Target.to_string t)))
                  else (
                    Hashtbl.add seen_version t ();
                    None)
              | _ -> None)
            rs
        in
        if dups = [] then Ok rs else Err.Accum.all dups |> Result.map (fun _ -> rs)
  end

(* {4 Rendering} *)

let render_record r =
  match r.value with None -> key_prefix r.key | Some v -> key_prefix r.key ^ "\t" ^ escape v

let serialize t =
  let lines = List.map render_record t in
  let sorted = List.sort String.compare lines in
  String.concat "" (List.map (fun l -> l ^ "\n") sorted)

let hashes t =
  List.filter_map
    (fun r -> match (r.key, r.value) with Sha256 p, Some v -> Some (p, v) | _ -> None)
    t

let find t key = List.find_map (fun r -> if r.key = key then Some r.value else None) t

(* P3: first wins, matching `awk -F'\t' '$1 == "source" { print $2; exit }'`. *)
let source_rel t =
  match List.find_opt (fun r -> r.key = Source) t with
  | None -> Ok None
  | Some { value = None; _ } ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Parse "the source record has no value")
  | Some { value = Some v; _ } -> Result.map Option.some (Identifier.relative_path v)
