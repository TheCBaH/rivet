type key =
  | Generator
  | Source
  | Source_unit of string
  | Inputs
  | Ccomp_version of Target.t
  | Ccomp_target of Target.t
  | Ccomp_args of Target.t
  | Ccomp_configure_args of Target.t
  | Sha256 of string
  | Abi_version
  | Supported_targets
  | Expected_value of int
  | Observation of int
  | Origin of string
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
  | Source_unit name -> "source-unit:" ^ name
  | Inputs -> "inputs"
  | Ccomp_version t -> "ccomp-version:" ^ Target.to_string t
  | Ccomp_target t -> "ccomp-target:" ^ Target.to_string t
  | Ccomp_args t -> "ccomp-args:" ^ Target.to_string t
  | Ccomp_configure_args t -> "ccomp-configure-args:" ^ Target.to_string t
  | Sha256 p -> "sha256:" ^ p
  | Abi_version -> "abi-version"
  | Supported_targets -> "supported-targets"
  | Expected_value i -> "expected-value:" ^ string_of_int i
  | Observation i -> "observation:" ^ string_of_int i
  | Origin stem -> "origin:" ^ stem
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
  | "abi-version" -> Ok Abi_version
  | "supported-targets" -> Ok Supported_targets
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
      (* Indexed by a plain positive integer rather than a [Target.t], for the
         M4 v3 result-vector families ([expected-value:]/[observation:]) -
         these are ordinal positions in a fixture's declared vector, not
         per-target facts. *)
      let targeted_int prefix build =
        match split_prefix raw prefix with
        | None -> None
        | Some suffix -> (
            match int_of_string_opt suffix with
            | Some i when i > 0 -> Some (Ok (build i))
            | _ ->
                Some
                  (fail_parse
                     (Printf.sprintf
                        "record %S has an invalid index %S (must be a positive integer)" raw suffix))
            )
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
                      match targeted_int "expected-value:" (fun i -> Expected_value i) with
                      | Some r -> r
                      | None -> (
                          match targeted_int "observation:" (fun i -> Observation i) with
                          | Some r -> r
                          | None -> (
                              match split_prefix raw "source-unit:" with
                              | Some name -> (
                                  (* Same D6 reasoning as sha256's path below: the key side
                                     has no escape layer, so a name is one path COMPONENT
                                     (no '/', no leading '-'), not the freer [source]
                                     value itself. *)
                                  match Identifier.parse name with
                                  | Ok id -> Ok (Source_unit (Identifier.to_string id))
                                  | Error e ->
                                      fail_parse
                                        (Printf.sprintf "record %S has an unusable unit name: %s"
                                           raw (Err.Error.kind e).Tool_error.detail))
                              | None -> (
                                  match split_prefix raw "origin:" with
                                  | Some stem -> (
                                      (* Exactly [source-unit:]'s pattern: the key
                                         carries the unit's own identity (one path
                                         component), and the value (parsed below,
                                         where the escape layer exists) carries the
                                         freer upstream path. *)
                                      match Identifier.parse stem with
                                      | Ok id -> Ok (Origin (Identifier.to_string id))
                                      | Error e ->
                                          fail_parse
                                            (Printf.sprintf
                                               "record %S has an unusable unit stem: %s" raw
                                               (Err.Error.kind e).Tool_error.detail))
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
                                                (Printf.sprintf "record %S has an unusable path: %s"
                                                   raw (Err.Error.kind e).Tool_error.detail))
                                      | None ->
                                          (* P2: Unknown records parse, and are DROPPED on
                                             rehash - write_manifest reconstructs known
                                             records only, so preserving them here would
                                             invent a behavior the shell does not have. *)
                                          Ok (Unknown raw))))))))))

let is_hex_digit c = match c with '0' .. '9' | 'a' .. 'f' -> true | _ -> false

let check_hash raw v =
  if String.length v = 64 && String.for_all is_hex_digit v then Ok ()
  else fail_parse (Printf.sprintf "record %S has a malformed SHA-256 value %S" raw v)

let check_abi_version raw v =
  match v with
  | "1" | "2" | "3" -> Ok ()
  | _ ->
      fail_parse
        (Printf.sprintf "record %S has an invalid abi-version value %S (must be 1, 2, or 3)" raw v)

(* Non-empty, no duplicates, and in [Target.all]'s canonical order - a
   validated set rather than a free string, so two manifests naming the same
   restricted target set are byte-identical rather than merely equivalent. *)
let check_supported_targets raw v =
  let names = String.split_on_char ' ' v |> List.filter (fun s -> s <> "") in
  if names = [] then
    fail_parse (Printf.sprintf "record %S has an empty supported-targets value" raw)
  else
    let rec parse_all acc = function
      | [] -> Ok (List.rev acc)
      | n :: rest -> (
          match Target.of_string n with
          | Ok t -> parse_all (t :: acc) rest
          | Error _ -> fail_parse (Printf.sprintf "record %S names an unknown target %S" raw n))
    in
    match parse_all [] names with
    | Error _ as e -> e
    | Ok ts ->
        let has_dup =
          let seen = Hashtbl.create 6 in
          List.exists
            (fun t ->
              if Hashtbl.mem seen t then true
              else (
                Hashtbl.add seen t ();
                false))
            ts
        in
        if has_dup then fail_parse (Printf.sprintf "record %S has a duplicate target" raw)
        else
          let canonical = List.filter (fun t -> List.mem t ts) Target.all in
          if canonical = ts then Ok ()
          else
            fail_parse
              (Printf.sprintf "record %S targets are not in canonical order (expected %S)" raw
                 (String.concat " " (List.map Target.to_string canonical)))

(* "<symbol> <width> <sign-extend:0|1>" - the host-side observation
   descriptor an M4 v3 fixture pairs with an [expected-value:] record.
   [width] is validated against the wire's own legal widths
   (docs/exec-abi-v3.md); resolving [symbol] to an address happens later,
   against a built [Image.exports], not here. *)
let check_observation raw v =
  match String.split_on_char ' ' v with
  | [ symbol; width_s; sign_s ] -> (
      if symbol = "" then
        fail_parse (Printf.sprintf "record %S has an empty observation symbol" raw)
      else
        match int_of_string_opt width_s with
        | Some (1 | 2 | 4 | 8) -> (
            match sign_s with
            | "0" | "1" -> Ok ()
            | _ ->
                fail_parse
                  (Printf.sprintf "record %S has an invalid sign-extend flag %S (must be 0 or 1)"
                     raw sign_s))
        | _ ->
            fail_parse
              (Printf.sprintf "record %S has an invalid width %S (must be 1, 2, 4, or 8)" raw
                 width_s))
  | _ ->
      fail_parse
        (Printf.sprintf "record %S must have the form \"<symbol> <width> <sign-extend:0|1>\"" raw)

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
                      | Abi_version -> (
                          match check_abi_version raw_key decoded with
                          | Ok () -> Ok { key; value = Some decoded }
                          | Error e -> at "%s" (Err.Error.kind e).Tool_error.detail)
                      | Supported_targets -> (
                          match check_supported_targets raw_key decoded with
                          | Ok () -> Ok { key; value = Some decoded }
                          | Error e -> at "%s" (Err.Error.kind e).Tool_error.detail)
                      | Expected_value _ -> (
                          match Int64.of_string_opt decoded with
                          | Some _ -> Ok { key; value = Some decoded }
                          | None ->
                              at "record %S has an invalid decimal integer value %S" raw_key decoded
                          )
                      | Observation _ -> (
                          match check_observation raw_key decoded with
                          | Ok () -> Ok { key; value = Some decoded }
                          | Error e -> at "%s" (Err.Error.kind e).Tool_error.detail)
                      | Origin _ -> (
                          match Identifier.relative_path decoded with
                          | Ok _ -> Ok { key; value = Some decoded }
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
          let seen_hash = Hashtbl.create 64
          and seen_version = Hashtbl.create 8
          and seen_source_unit = Hashtbl.create 4
          and seen_expected_value = Hashtbl.create 4
          and seen_observation = Hashtbl.create 4
          and seen_origin = Hashtbl.create 4 in
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
              (* Unlike Source's own P3 first-wins: two sources claiming the
                 same unit name is ambiguous rather than redundant - there is
                 no well-defined "which one wins" for which per-target .s file
                 a unit's own name should mean. *)
              | Source_unit name ->
                  if Hashtbl.mem seen_source_unit name then
                    Some (fail_parse (Printf.sprintf "duplicate source-unit record for %S" name))
                  else (
                    Hashtbl.add seen_source_unit name ();
                    None)
              | Expected_value i ->
                  if Hashtbl.mem seen_expected_value i then
                    Some
                      (fail_parse (Printf.sprintf "duplicate expected-value record for index %d" i))
                  else (
                    Hashtbl.add seen_expected_value i ();
                    None)
              | Observation i ->
                  if Hashtbl.mem seen_observation i then
                    Some (fail_parse (Printf.sprintf "duplicate observation record for index %d" i))
                  else (
                    Hashtbl.add seen_observation i ();
                    None)
              | Origin stem ->
                  if Hashtbl.mem seen_origin stem then
                    Some (fail_parse (Printf.sprintf "duplicate origin record for %S" stem))
                  else (
                    Hashtbl.add seen_origin stem ();
                    None)
              | _ -> None)
            rs
        in
        (* Whole-list invariants over the v3 result-vector families, beyond
           per-record duplication: element 0 of a fixture's result vector is
           always [expected-status.txt] (unaffected by any of this), so
           [expected-value:]/[observation:] declare elements 1..N with no
           gaps, exactly paired one-to-one, and only under [abi-version: 3].
           [Origin] vs. a legacy single-[source] case's implicit unit name is
           NOT checked here - this function has no case name to compare
           against, only [Source_unit] records; that check lives in
           [Corpus.sources]. *)
        let index_pairing_errors =
          let expected_indices =
            List.sort_uniq compare
              (List.filter_map (function { key = Expected_value i; _ } -> Some i | _ -> None) rs)
          in
          let observation_indices =
            List.sort_uniq compare
              (List.filter_map (function { key = Observation i; _ } -> Some i | _ -> None) rs)
          in
          let has_v3 = List.exists (fun r -> r.key = Abi_version && r.value = Some "3") rs in
          if expected_indices = [] && observation_indices = [] then
            if has_v3 then
              [ fail_parse "abi-version: 3 requires at least one expected-value/observation pair" ]
            else []
          else
            let n = List.length expected_indices in
            let contiguous_from_one = expected_indices = List.init n (fun i -> i + 1) in
            (if contiguous_from_one then []
             else [ fail_parse "expected-value indices must be exactly 1..N with no gaps" ])
            @ (if expected_indices = observation_indices then []
               else
                 [
                   fail_parse
                     "expected-value and observation indices must match exactly, one observation \
                      per expected value";
                 ])
            @
            if has_v3 then []
            else [ fail_parse "expected-value/observation records require abi-version: 3" ]
        in
        let all_errors = dups @ index_pairing_errors in
        if all_errors = [] then Ok rs else Err.Accum.all all_errors |> Result.map (fun _ -> rs)
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

let source_units t =
  let units =
    List.filter_map
      (function { key = Source_unit name; value } -> Some (name, value) | _ -> None)
      t
  in
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | (_, None) :: _ ->
        Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
          (Tool_error.v Tool_error.Parse "a source-unit record has no value")
    | (name, Some v) :: rest -> (
        match Identifier.relative_path v with
        | Error _ as e -> e
        | Ok p -> go ((name, p) :: acc) rest)
  in
  Result.map (List.sort compare) (go [] units)

let supported_targets t =
  match List.find_opt (fun r -> r.key = Supported_targets) t with
  | None -> Ok None
  | Some { value = None; _ } ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Parse "the supported-targets record has no value")
  | Some { value = Some v; _ } ->
      (* Already validated at [parse] time (check_supported_targets): every
         token is a known target, deduplicated, in canonical order - this
         just re-decodes, it does not re-validate. *)
      let names = String.split_on_char ' ' v |> List.filter (fun s -> s <> "") in
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | n :: rest -> (
            match Target.of_string n with Ok t -> go (t :: acc) rest | Error _ as e -> e)
      in
      Result.map Option.some (go [] names)

let origins t =
  let units =
    List.filter_map (function { key = Origin stem; value } -> Some (stem, value) | _ -> None) t
  in
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | (_, None) :: _ ->
        Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
          (Tool_error.v Tool_error.Parse "an origin record has no value")
    | (stem, Some v) :: rest -> (
        match Identifier.relative_path v with
        | Error _ as e -> e
        | Ok p -> go ((stem, p) :: acc) rest)
  in
  Result.map (List.sort compare) (go [] units)
