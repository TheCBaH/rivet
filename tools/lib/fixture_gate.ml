type violation = Comm_or_local | Bss_or_nobits | Tail_converted_call | Missing_entry_label

(* Line-oriented, matching the shell's `grep -qE '^[[:space:]]*\.(comm|local)\b'`.
   Anchored at the start of a line with optional leading whitespace, so a
   mention inside a comment or a string does not trip the gate. *)
let lines asm = String.split_on_char '\n' asm
let is_space c = c = ' ' || c = '\t'

let strip_leading l =
  let n = String.length l in
  let rec go i = if i < n && is_space l.[i] then go (i + 1) else i in
  let i = go 0 in
  String.sub l i (n - i)

(* \b in the shell's ERE: the directive must not run into an identifier
   character. `.commfoo` is not `.comm`. *)
let word_boundary_after s prefix =
  let n = String.length prefix in
  String.length s >= n
  && String.sub s 0 n = prefix
  && (String.length s = n
     || match s.[n] with 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> false | _ -> true)

let any_line asm f = List.exists (fun l -> f (strip_leading l)) (lines asm)

let contains hay needle =
  let n = String.length needle and m = String.length hay in
  let rec go i = i + n <= m && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has_entry_label asm = List.exists (fun l -> word_boundary_after l "asm_test_entry:") (lines asm)

let check ~case ~asm =
  let v = [] in
  let v =
    if
      case <> "cross_bss"
      && any_line asm (fun l -> word_boundary_after l ".comm" || word_boundary_after l ".local")
    then Comm_or_local :: v
    else v
  in
  let v =
    if
      case <> "cross_bss"
      && (any_line asm (fun l -> word_boundary_after l ".bss")
         || contains asm "@nobits" || contains asm "%nobits")
    then Bss_or_nobits :: v
    else v
  in
  let v =
    (* The shell is `grep -qE '^[[:space:]]*(call|bl)[[:space:]]'`: a mnemonic at
       the start of a line, followed by whitespace. Only direct_call is checked -
       the other cases have no call site, so requiring one would reject them for
       a construct they never had. *)
    let starts_with_mnemonic l m =
      let n = String.length m in
      String.length l > n && String.sub l 0 n = m && is_space l.[n]
    in
    if
      case = "direct_call"
      && not (any_line asm (fun l -> starts_with_mnemonic l "call" || starts_with_mnemonic l "bl"))
    then Tail_converted_call :: v
    else v
  in
  List.rev v

let message ~case ~target violation =
  let where = Printf.sprintf "GATE %s/%s" case (Target.to_string target) in
  match violation with
  | Comm_or_local -> where ^ ": .comm/.local is M3 scope; give the global a non-zero initializer"
  | Bss_or_nobits -> where ^ ": .bss/nobits is M3 scope; give the global a non-zero initializer"
  | Tail_converted_call ->
      where ^ ": no call/bl - the call was tail-converted; make the call site non-tail"
  | Missing_entry_label ->
      where ^ ": no asm_test_entry label - every execution fixture needs the frozen entry"
