let fail op detail = Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v op detail)

(* Whitespace-separated fields, the awk default. Leading and trailing runs
   produce no empty fields, so field indices match awk's $1.. exactly. *)
let fields line =
  String.split_on_char '\t' line
  |> List.concat_map (String.split_on_char ' ')
  |> List.filter (fun s -> s <> "")

let lines text =
  let l = String.split_on_char '\n' text in
  (* A trailing newline yields a final "" that awk never sees as a record. *)
  match List.rev l with
  | "" :: rest -> List.rev rest
  | _ -> l

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* {1 objdump -h}

   Two-line records: a header line beginning with the index, then a flags line.
   Anything else - the blank line, the "file format" banner, the "Sections:" and
   column headings - matches neither and is skipped. *)

let is_index_line l =
  match fields l with
  | idx :: _ -> idx <> "" && String.for_all (fun c -> c >= '0' && c <= '9') idx
  | [] -> false

(* The flag is matched as a whole comma-or-space delimited word. A substring
   test would accept a hypothetical "NOALLOC" or "ALLOCATED"; this is the same
   word-boundary discipline the shell's regex used. *)
let has_flag flags name =
  String.split_on_char ',' flags
  |> List.concat_map (String.split_on_char ' ')
  |> List.exists (fun w -> w = name)

let section_headers text =
  let rec go acc = function
    | [] -> List.rev acc
    | l :: rest when is_index_line l -> (
        match fields l with
        | _idx :: name :: size :: _ -> (
            (* The flags are on the NEXT line; an index line that ends the input
               contributes no record rather than a record with no flags. *)
            match rest with
            | flags :: rest' -> go ((name, size, flags) :: acc) rest'
            | [] -> List.rev acc)
        | _ -> go acc rest)
    | _ :: rest -> go acc rest
  in
  go [] (lines text)

let alloc_sections text =
  match section_headers text with
  | [] -> fail Tool_error.Parse "objdump -h listed no sections"
  | hs -> Ok (List.filter_map (fun (n, _, f) -> if has_flag f "ALLOC" then Some n else None) hs)

let section_size text ~name =
  List.find_map (fun (n, s, _) -> if n = name then Some s else None) (section_headers text)

let section_is_nobits text ~name =
  List.exists (fun (n, _, f) -> n = name && not (has_flag f "CONTENTS")) (section_headers text)

(* {2 readelf -SW}

   Only the question "is there a .rela.* section". The bracketed index makes the
   name field's position stable even when the index is padded. *)
(* Structural and ANCHORED, transcribing the shell's
     ^[[:space:]]*\[[[:space:]]*[0-9]+\][[:space:]]+\.rela\.
   position by position.

   An earlier version of this scanned for "the first field ending in ']' and
   took the next one". That is strictly LOOSER than the grep - no line-start
   anchor, no digit check - so any line carrying a bracketed token followed by
   a ".rela." token would have satisfied it. This predicate decides REL versus
   RELA, i.e. whether the addend is 0 or lives in the instruction field, so a
   false positive silently corrupts reloc.txt for every relocating fixture. *)
let is_rela_section_line l =
  let n = String.length l in
  let space i = i < n && (l.[i] = ' ' || l.[i] = '\t') in
  let digit i = i < n && l.[i] >= '0' && l.[i] <= '9' in
  let rec skip_space i = if space i then skip_space (i + 1) else i in
  let i = skip_space 0 in
  if not (i < n && l.[i] = '[') then false
  else
    let i = skip_space (i + 1) in
    if not (digit i) then false
    else
      let rec skip_digits i = if digit i then skip_digits (i + 1) else i in
      let i = skip_digits i in
      if not (i < n && l.[i] = ']') then false
      else
        let j = i + 1 in
        (* At least one space between the bracket and the name. *)
        if not (space j) then false
        else
          let k = skip_space j in
          k + 6 <= n && String.sub l k 6 = ".rela."

let has_rela text = List.exists is_rela_section_line (lines text)

(* {3 objdump -r}

   Strip leading zeros, then pad to the requested width. [trim] is the same
   operation with no padding, and both map an all-zero string to "0" rather than
   to "". *)
let strip_zeros h =
  let n = String.length h in
  let rec first_sig i = if i < n && h.[i] = '0' then first_sig (i + 1) else i in
  let i = first_sig 0 in
  if i = n then "0" else String.sub h i (n - i)

let pad8 h =
  let d = strip_zeros h in
  let l = String.length d in
  if l >= 8 then d else String.make (8 - l) '0' ^ d

(* The addend is spelled into objdump's VALUE column as sym+0xN / sym-0xN.
   Matching from the END is what keeps a symbol whose own name contains "+0x"
   intact - the addend is always the final such group. *)
let split_addend value =
  let n = String.length value in
  let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
  let rec scan i =
    (* i indexes a candidate sign character. *)
    if i < 0 then None
    else if
      (value.[i] = '+' || value.[i] = '-')
      && i + 3 < n
      && value.[i + 1] = '0'
      && value.[i + 2] = 'x'
      &&
      let rec all j = j >= n || (is_hex value.[j] && all (j + 1)) in
      all (i + 3)
    then Some i
    else scan (i - 1)
  in
  match scan (n - 1) with
  | None -> None
  | Some i ->
      let sym = String.sub value 0 i in
      let sign = String.make 1 value.[i] in
      let digits = String.sub value (i + 3) (n - i - 3) in
      Some (sym, sign ^ "0x" ^ strip_zeros digits)

let recorded_sections = [ ".text"; ".rodata"; ".data" ]

let relocations ~objdump_r ~rela =
  let kind = if rela then "rela" else "rel" in
  let default_addend = if rela then "0x0" else "-" in
  let rec go section acc = function
    | [] -> Ok (List.rev acc)
    | l :: rest -> (
        if starts_with "RELOCATION RECORDS FOR" l then
          (* The banner names the section the records APPLY TO, between
             brackets: "RELOCATION RECORDS FOR [.text]:". *)
          let section =
            match (String.index_opt l '[', String.rindex_opt l ']') with
            | Some a, Some b when b > a + 1 -> String.sub l (a + 1) (b - a - 1)
            | _ -> ""
          in
          go section acc rest
        else if starts_with "OFFSET" l then go section acc rest
        else if (not (List.mem section recorded_sections)) || fields l = [] then go section acc rest
        else
          let contains_ff s = String.length s > 12 && starts_with "file format" s in
          if contains_ff l then go section acc rest
          else
            match fields l with
            | off :: typ :: value :: _ ->
                let sym, addend =
                  match split_addend value with
                  | Some (s, a) -> (s, a)
                  | None -> (value, default_addend)
                in
                let row = String.concat "\t" [ section; "0x" ^ pad8 off; kind; typ; sym; addend ] in
                go section (row :: acc) rest
            | _ ->
                Error
                  (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp
                     (Tool_error.v Tool_error.Parse
                        (Printf.sprintf "malformed objdump -r record: %S" l))))
  in
  match go "" [] (lines objdump_r) with
  | Error e -> Error e
  | Ok rows ->
      (* String.compare is bytewise, hence identical to LC_ALL=C sort. *)
      let sorted = List.sort String.compare rows in
      Ok (String.concat "" (List.map (fun r -> r ^ "\n") sorted))

(* {4 readelf -sW}

   Records are the lines whose first field is "<n>:". The index-0 entry has an
   EMPTY name, so it has one field fewer and is dropped by the arity check
   rather than by a special case. *)
let symbols ~readelf_s =
  let is_num_colon f =
    let n = String.length f in
    n >= 2
    && f.[n - 1] = ':'
    && String.for_all (fun c -> c >= '0' && c <= '9') (String.sub f 0 (n - 1))
  in
  let is_file_symbol name =
    let ends s = String.length name >= 2 && String.sub name (String.length name - 2) 2 = s in
    ends ".c" || ends ".s"
  in
  let rows =
    List.filter_map
      (fun l ->
        match fields l with
        | num :: _value :: _size :: _type_ :: bind :: vis :: ndx :: name :: _ when is_num_colon num
          ->
            if name = "" || is_file_symbol name then None
            else
              let defined = if ndx = "UND" then "undefined" else "defined" in
              Some
                (String.concat "\t"
                   [ name; String.lowercase_ascii bind; String.lowercase_ascii vis; ndx; defined ])
        | _ -> None)
      (lines readelf_s)
  in
  (* sort -u: bytewise order, then adjacent deduplication. *)
  let sorted = List.sort_uniq String.compare rows in
  Ok (String.concat "" (List.map (fun r -> r ^ "\n") sorted))
