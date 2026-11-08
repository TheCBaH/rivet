(* The assembler dialect of the Macro Assembler AS for [cpu 8080], over the
   target's encoder.

   Sources are the Intel mnemonics the encoder already knows plus what real 8080
   sources need around them: macros ([macro]/[endm], [rept], [$$] locals),
   conditional assembly, [equ]/[set], several [org]s, [ds], [dup], [end], [$],
   and the literal spellings [NNh], [NNb], [0xNN], [$NN] and [NNd]. Expressions
   use AS's operators and priorities. Instructions are not parsed here: each is
   handed to {!I8080_encode} as soon as its operands have values, so the table
   stays the only list of opcodes.

   Anything the dialect does not cover is reported with its source position
   (["unsupported: ..."]) and assembly goes on, so one run lists every
   unsupported construct of a file. *)

open Foundation
module E = I8080_encode

type loc = { file : string; line : int; via : (string * int) list }
type diagnostic = { loc : loc; message : string }
type segment = { origin : int; bytes : string }
type entry = { loc : loc; address : int; bytes : string; text : string }
type image = { segments : segment list; symbols : (string * int) list; listing : entry list }

let pp_loc ppf { file; line; via } =
  Format.fprintf ppf "%s:%d" file line;
  List.iter (fun (m, l) -> Format.fprintf ppf " (in %s from line %d)" m l) via

let pp_diagnostic ppf (d : diagnostic) = Format.fprintf ppf "%a: %s" pp_loc d.loc d.message
let render ds = String.concat "\n" (List.map (Format.asprintf "%a" pp_diagnostic) ds)

exception Fail of string
exception Undefined of string

let fail fmt = Printf.ksprintf (fun m -> raise (Fail m)) fmt
let unsupported fmt = Printf.ksprintf (fun m -> raise (Fail ("unsupported: " ^ m))) fmt

(* {1 Lexing expressions} *)

type tok = Num of int | Str of string | Id of string | Op of string | Lp | Rp

let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_id_start c = is_alpha c || c = '_' || c = '@' || c = '?' || c = '.' || c = '$'
let is_id c = is_id_start c || is_digit c

let operators =
  [
    "<<";
    ">>";
    "<=";
    ">=";
    "<>";
    "==";
    "!=";
    "&&";
    "||";
    "+";
    "-";
    "*";
    "/";
    "#";
    "^";
    "&";
    "|";
    "!";
    "~";
    "=";
    "<";
    ">";
  ]

let number_of_word w =
  let n = String.length w in
  let digits ~base s =
    if s = "" then fail "malformed number %S" w
    else
      String.fold_left
        (fun acc c ->
          let d =
            if is_digit c then Char.code c - 48
            else if c >= 'a' && c <= 'f' then Char.code c - 87
            else if c >= 'A' && c <= 'F' then Char.code c - 55
            else fail "malformed number %S" w
          in
          if d >= base then fail "malformed number %S" w else (acc * base) + d)
        0 s
  in
  let lower = String.lowercase_ascii w in
  if n > 2 && lower.[0] = '0' && lower.[1] = 'x' then digits ~base:16 (String.sub w 2 (n - 2))
  else
    match lower.[n - 1] with
    | 'h' -> digits ~base:16 (String.sub w 0 (n - 1))
    | ('b' | 'o' | 'q' | 'd') when n > 1 && String.for_all is_hex (String.sub w 0 (n - 1)) -> (
        let body = String.sub w 0 (n - 1) in
        match lower.[n - 1] with
        | 'b' when String.for_all (fun c -> c = '0' || c = '1') body -> digits ~base:2 body
        | 'b' -> digits ~base:16 w
        | 'd' -> digits ~base:10 body
        | _ -> digits ~base:8 body)
    | _ -> digits ~base:10 w

let tokenize s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let c = s.[i] in
      if c = ' ' || c = '\t' then go (i + 1) acc
      else if c = '(' then go (i + 1) (Lp :: acc)
      else if c = ')' then go (i + 1) (Rp :: acc)
      else if is_digit c then (
        let j = ref i in
        while !j < n && is_id s.[!j] do
          incr j
        done;
        go !j (Num (number_of_word (String.sub s i (!j - i))) :: acc))
      else if c = '$' && i + 1 < n && is_hex s.[i + 1] && not (i + 1 < n && s.[i + 1] = '$') then (
        let j = ref (i + 1) in
        while !j < n && is_hex s.[!j] do
          incr j
        done;
        if !j < n && is_id s.[!j] then (
          (* an identifier such as [$foo]: not a hex literal *)
          let k = ref (i + 1) in
          while !k < n && is_id s.[!k] do
            incr k
          done;
          go !k (Id (String.lowercase_ascii (String.sub s i (!k - i))) :: acc))
        else go !j (Num (number_of_word ("0" ^ String.sub s (i + 1) (!j - i - 1) ^ "h")) :: acc))
      else if is_id_start c then (
        let j = ref i in
        while !j < n && is_id s.[!j] do
          incr j
        done;
        go !j (Id (String.lowercase_ascii (String.sub s i (!j - i))) :: acc))
      else if c = '\'' || c = '"' then
        let b = Buffer.create 8 in
        let rec str j =
          if j >= n then fail "unterminated string"
          else if s.[j] = c then
            if j + 1 < n && s.[j + 1] = c then (
              Buffer.add_char b c;
              str (j + 2))
            else j + 1
          else (
            Buffer.add_char b s.[j];
            str (j + 1))
        in
        let j = str (i + 1) in
        go j (Str (Buffer.contents b) :: acc)
      else
        match
          List.find_opt
            (fun o ->
              let l = String.length o in
              i + l <= n && String.sub s i l = o)
            operators
        with
        | Some o -> go (i + String.length o) (Op o :: acc)
        | None -> fail "unexpected character %C" c
  in
  go 0 []

(* {1 Expressions} *)

type expr =
  | Int of int
  | Sym of string
  | Pc
  | Neg of expr
  | Not of expr
  | Low of expr
  | High of expr
  | Bin of string * expr * expr

(* Binary levels from the loosest, as AS orders them (measured against asl):
   comparisons < [&&] [||] < [+] [-] < [*] [/] [#] < [^] (power) < [!] (xor)
   < [|] < [&] < shifts. *)
let levels =
  [|
    [ "="; "=="; "<>"; "!="; "<"; ">"; "<="; ">=" ];
    [ "&&"; "||" ];
    [ "+"; "-" ];
    [ "*"; "/"; "#" ];
    [ "^" ];
    [ "!" ];
    [ "|" ];
    [ "&" ];
    [ "<<"; ">>" ];
  |]

let parse_expr toks =
  let toks = ref toks in
  let peek () = match !toks with t :: _ -> Some t | [] -> None in
  let advance () = toks := List.tl !toks in
  let rec level k =
    if k >= Array.length levels then unary ()
    else
      let rec loop lhs =
        match peek () with
        | Some (Op o) when List.mem o levels.(k) ->
            advance ();
            loop (Bin (o, lhs, level (k + 1)))
        | _ -> lhs
      in
      loop (level (k + 1))
  and unary () =
    match peek () with
    | Some (Op "-") ->
        advance ();
        Neg (level 4)
    | Some (Op "+") ->
        advance ();
        level 4
    | Some (Op "~") ->
        advance ();
        Not (unary ())
    | Some (Id "high") ->
        advance ();
        High (unary ())
    | Some (Id "low") ->
        advance ();
        Low (unary ())
    | _ -> primary ()
  and primary () =
    match peek () with
    | Some (Num v) ->
        advance ();
        Int v
    | Some (Str s) ->
        advance ();
        if String.length s = 1 then Int (Char.code s.[0])
        else if String.length s = 2 then Int ((Char.code s.[0] lsl 8) lor Char.code s.[1])
        else fail "string %S is not a number" s
    | Some (Id "$") ->
        advance ();
        Pc
    | Some (Id name) ->
        advance ();
        Sym name
    | Some Lp ->
        advance ();
        let e = level 0 in
        (match peek () with Some Rp -> advance () | _ -> fail "missing closing parenthesis");
        e
    | Some (Op o) -> fail "unexpected operator %s" o
    | Some Rp -> fail "unexpected closing parenthesis"
    | None -> fail "expression expected"
  in
  let e = level 0 in
  if !toks <> [] then fail "unexpected text after expression";
  e

let pow a b =
  if b < 0 then fail "negative exponent"
  else
    let rec go acc b = if b = 0 then acc else go (acc * a) (b - 1) in
    go 1 b

let rec eval ~lookup ~pc = function
  | Int v -> v
  | Sym s -> lookup s
  | Pc -> pc
  | Neg e -> -eval ~lookup ~pc e
  | Not e -> lnot (eval ~lookup ~pc e)
  | Low e -> eval ~lookup ~pc e land 0xff
  | High e -> (eval ~lookup ~pc e lsr 8) land 0xff
  | Bin (op, a, b) -> (
      let a = eval ~lookup ~pc a and b = eval ~lookup ~pc b in
      let bool x = if x then 1 else 0 in
      match op with
      | "+" -> a + b
      | "-" -> a - b
      | "*" -> a * b
      | "/" -> if b = 0 then fail "division by zero" else a / b
      | "#" -> if b = 0 then fail "division by zero" else a mod b
      | "^" -> pow a b
      | "&" -> a land b
      | "|" -> a lor b
      | "!" -> a lxor b
      | "<<" -> a lsl b
      | ">>" -> a lsr b
      | "&&" -> bool (a <> 0 && b <> 0)
      | "||" -> bool (a <> 0 || b <> 0)
      | "=" | "==" -> bool (a = b)
      | "<>" | "!=" -> bool (a <> b)
      | "<" -> bool (a < b)
      | ">" -> bool (a > b)
      | "<=" -> bool (a <= b)
      | ">=" -> bool (a >= b)
      | _ -> fail "unknown operator %s" op)

(* {1 Splitting source text} *)

let strip_comment s =
  let n = String.length s in
  let rec go i q =
    if i >= n then s
    else
      let c = s.[i] in
      match q with
      | Some d -> go (i + 1) (if c = d then None else q)
      | None ->
          if c = ';' then String.sub s 0 i
          else go (i + 1) (if c = '\'' || c = '"' then Some c else None)
  in
  go 0 None

let rtrim s =
  let n = ref (String.length s) in
  while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t' || s.[!n - 1] = '\r') do
    decr n
  done;
  String.sub s 0 !n

let ltrim s =
  let i = ref 0 in
  while !i < String.length s && (s.[!i] = ' ' || s.[!i] = '\t') do
    incr i
  done;
  String.sub s !i (String.length s - !i)

let trim s = ltrim (rtrim s)

(* Split at commas outside quotes and parentheses. *)
let split_args s =
  if trim s = "" then []
  else
    let n = String.length s in
    let parts = ref [] and start = ref 0 and depth = ref 0 and q = ref None in
    for i = 0 to n - 1 do
      let c = s.[i] in
      match !q with
      | Some d -> if c = d then q := None
      | None ->
          if c = '\'' || c = '"' then q := Some c
          else if c = '(' then incr depth
          else if c = ')' then decr depth
          else if c = ',' && !depth = 0 then (
            parts := String.sub s !start (i - !start) :: !parts;
            start := i + 1)
    done;
    List.rev_map trim (String.sub s !start (n - !start) :: !parts)

(* The word at [i]: the characters an identifier may contain. *)
let word s i =
  let n = String.length s in
  let j = ref i in
  while !j < n && is_id s.[!j] do
    incr j
  done;
  if !j < n && s.[!j] = ':' && !j > i then incr j;
  (String.sub s i (!j - i), !j)

(* {1 Assembly} *)

type macro = { params : string list; body : (string * int) list }
type cond = { active : bool; taken : bool; outer : bool }
type frame = { mutable rest : (string * loc) list }

type st = {
  mutable pc : int;
  mutable stmt_pc : int;  (** [$]: where the statement being assembled began *)
  syms : (string, int) Hashtbl.t;  (** labels and [equ] of this pass *)
  prev : (string, int) Hashtbl.t;  (** the previous pass, for forward references *)
  sets : (string, int) Hashtbl.t;
  macros : (string, macro) Hashtbl.t;
  mutable conds : cond list;
  mutable frames : frame list;
  mutable cur : (int * Buffer.t) option;
  mutable segs : (int * Buffer.t) list;
  mutable errors : diagnostic list;
  mutable stop : bool;
  mutable uniq : int;
  mutable steps : int;
  mutable listing : entry list;
  mutable line_bytes : Buffer.t;
  first : bool;
}

let max_steps = 20_000_000
let registers = [ "a"; "b"; "c"; "d"; "e"; "h"; "l"; "m"; "sp"; "psw" ]

let directives =
  [
    "cpu";
    "org";
    "db";
    "dw";
    "ds";
    "end";
    "if";
    "ifdef";
    "ifndef";
    "else";
    "elseif";
    "endif";
    "macro";
    "endm";
    "rept";
    "error";
    "title";
    "page";
    "equ";
    "set";
    "=";
    "listing";
    "list";
    "nolist";
    "name";
    "include";
  ]

let active st = match st.conds with [] -> true | c :: _ -> c.active

let lookup st name =
  match Hashtbl.find_opt st.sets name with
  | Some v -> v
  | None -> (
      match Hashtbl.find_opt st.syms name with
      | Some v -> v
      | None -> (
          match Hashtbl.find_opt st.prev name with Some v -> v | None -> raise (Undefined name)))

(* [lenient]: an undefined symbol is 0 on a pass that is not the last, where
   only the sizes matter. *)
let value ?(lenient = false) st text =
  let e = parse_expr (tokenize text) in
  let lookup n = try lookup st n with Undefined _ when lenient && st.first -> 0 in
  try eval ~lookup ~pc:st.stmt_pc e with Undefined n -> fail "undefined symbol %s" n

let emit st bytes =
  if bytes <> "" then (
    (match st.cur with
    | Some (o, b) when o + Buffer.length b = st.pc -> Buffer.add_string b bytes
    | cur ->
        Option.iter (fun s -> st.segs <- s :: st.segs) cur;
        let b = Buffer.create 64 in
        Buffer.add_string b bytes;
        st.cur <- Some (st.pc, b));
    Buffer.add_string st.line_bytes bytes;
    st.pc <- st.pc + String.length bytes)

let define st name v =
  if Hashtbl.mem st.syms name || Hashtbl.mem st.sets name then
    fail "symbol %s is already defined" name;
  Hashtbl.replace st.syms name v

let range_check ~what ~lo ~hi v =
  if v < lo || v > hi then fail "%s %d out of range (%d..%d)" what v lo hi

let le bytes v = String.init bytes (fun i -> Char.chr ((v asr (8 * i)) land 0xff))

(* [dup]: a repeat count, the word [dup], a parenthesised list. *)
let find_dup s =
  let n = String.length s in
  let rec go i depth q =
    if i >= n then None
    else
      let c = s.[i] in
      match q with
      | Some d -> go (i + 1) depth (if c = d then None else q)
      | None ->
          if c = '\'' || c = '"' then go (i + 1) depth (Some c)
          else if c = '(' then go (i + 1) (depth + 1) None
          else if c = ')' then go (i + 1) (depth - 1) None
          else if
            depth = 0
            && i + 3 <= n
            && String.lowercase_ascii (String.sub s i 3) = "dup"
            && (i = 0 || not (is_id s.[i - 1]))
            && (i + 3 = n || not (is_id s.[i + 3]))
          then Some i
          else go (i + 1) depth None
  in
  go 0 0 None

let rec data st ~width arg =
  match find_dup arg with
  | Some i ->
      let count = value st (String.sub arg 0 i) in
      let rest = trim (String.sub arg (i + 3) (String.length arg - i - 3)) in
      let n = String.length rest in
      if n < 2 || rest.[0] <> '(' || rest.[n - 1] <> ')' then fail "dup needs a parenthesised list";
      if count < 0 then fail "negative dup count";
      let inner = split_args (String.sub rest 1 (n - 2)) in
      for _ = 1 to count do
        List.iter (data st ~width) inner
      done
  | None -> (
      let t = trim arg in
      match tokenize t with
      | [ Str str ] when String.length str <> 1 ->
          if width = 1 then emit st str
          else unsupported "dw with a string of %d characters" (String.length str)
      | _ ->
          let v = value ~lenient:true st t in
          if width = 1 then range_check ~what:"db value" ~lo:(-128) ~hi:255 v
          else range_check ~what:"dw value" ~lo:(-32768) ~hi:65535 v;
          emit st (le width v))

let operand st text =
  let t = String.lowercase_ascii (trim text) in
  if List.mem t registers then
    match t with
    | "sp" -> E.Operand.Sp
    | "psw" -> E.Operand.Psw
    | r -> E.Operand.Reg (Option.get (E.Reg.find r))
  else E.Operand.Imm (Bigint.of_int (value ~lenient:true st text))

let assemble_instruction st mnemonic args =
  let origin = Origin.synthesized ~pass:"i8080.as" () in
  let render_error e = Format.asprintf "%a" E.pp_error (Err.Error.kind e) in
  let ( let* ) r f = match r with Ok x -> f x | Error e -> fail "%s" (render_error e) in
  let ops = List.map (operand st) args in
  let* s = E.make_surface_instruction ~mnemonic ~origin ops in
  let* i = E.simplify_instruction () s in
  let* lowered = E.lower_instruction () i in
  List.iter
    (fun l ->
      let* enc = E.encode l in
      match enc with
      | `Fixed f -> emit st f.Asm_core.Lowered_ast.bytes
      | _ -> fail "unexpected encoding")
    lowered

(* Replace macro parameters and rename [$$] locals. *)
let substitute ~params ~args ~uniq line =
  let n = String.length line in
  let b = Buffer.create (n + 16) in
  let i = ref 0 in
  while !i < n do
    let c = line.[!i] in
    if c = '\'' || c = '"' then (
      let j = ref (!i + 1) in
      while !j < n && line.[!j] <> c do
        incr j
      done;
      let j = min (!j + 1) n in
      Buffer.add_string b (String.sub line !i (j - !i));
      i := j)
    else if is_id_start c then (
      let j = ref !i in
      while !j < n && is_id line.[!j] do
        incr j
      done;
      let w = String.sub line !i (!j - !i) in
      let lw = String.lowercase_ascii w in
      (if String.length w > 2 && w.[0] = '$' && w.[1] = '$' then
         Buffer.add_string b (Printf.sprintf "%s@%d" (String.sub w 2 (String.length w - 2)) uniq)
       else
         match List.assoc_opt lw (List.combine params args) with
         | Some a -> Buffer.add_string b a
         | None -> Buffer.add_string b w);
      i := !j)
    else (
      Buffer.add_char b c;
      incr i)
  done;
  Buffer.contents b

let next_line st =
  let rec go = function
    | [] -> None
    | f :: rest -> (
        match f.rest with
        | [] ->
            st.frames <- rest;
            go rest
        | l :: tl ->
            f.rest <- tl;
            Some l)
  in
  go st.frames

(* The body of a [macro] or [rept] up to its matching [endm]. *)
let collect_body st =
  let rec go depth acc =
    match next_line st with
    | None -> fail "missing endm"
    | Some (text, loc) ->
        let t = ltrim (strip_comment text) in
        let w1, e1 = if t = "" then ("", 0) else word t 0 in
        let w2 =
          if t = "" then "" else fst (word (ltrim (String.sub t e1 (String.length t - e1))) 0)
        in
        let key w = String.lowercase_ascii w in
        let opens = List.mem (key w1) [ "rept"; "macro" ] || key w2 = "macro" in
        let closes = key w1 = "endm" in
        if closes && depth = 0 then List.rev acc
        else
          go
            (if closes then depth - 1 else if opens then depth + 1 else depth)
            ((text, loc.line) :: acc)
  in
  go 0 []

let push_frame st lines = st.frames <- { rest = lines } :: st.frames

let rec process st (text, loc) =
  st.steps <- st.steps + 1;
  if st.steps > max_steps then fail "assembly does not terminate (too many lines)";
  st.stmt_pc <- st.pc;
  let raw = rtrim (strip_comment text) in
  if trim raw <> "" then (
    let col0 = raw.[0] <> ' ' && raw.[0] <> '\t' in
    let t = ltrim raw in
    let w1, e1 = word t 0 in
    let rest1 = ltrim (String.sub t e1 (String.length t - e1)) in
    let w2, e2 = if rest1 = "" then ("", 0) else word rest1 0 in
    let rest2 = ltrim (String.sub rest1 e2 (String.length rest1 - e2)) in
    let lw1 = String.lowercase_ascii w1 and lw2 = String.lowercase_ascii w2 in
    let colon = String.length w1 > 0 && w1.[String.length w1 - 1] = ':' in
    let name1 = if colon then String.sub w1 0 (String.length w1 - 1) else w1 in
    let lname1 = String.lowercase_ascii name1 in
    let is_op w =
      List.mem w directives || E.Opcode.of_mnemonic w <> None || Hashtbl.mem st.macros w
    in
    let eq = rest1 <> "" && rest1.[0] = '=' && not (String.length rest1 > 1 && rest1.[1] = '=') in
    let assign_kw = List.mem lw2 [ "equ"; "set"; "macro" ] || eq in
    if w1 = "" then fail "cannot parse the line";
    let label, op, args =
      if colon || (col0 && (assign_kw || not (is_op lw1))) then
        if eq then (Some lname1, "=", String.sub rest1 1 (String.length rest1 - 1))
        else if w2 = "" then (Some lname1, "", "")
        else (Some lname1, lw2, rest2)
      else (None, lw1, rest1)
    in
    conditional_or st (label, op, args) loc text)

and conditional_or st (label, op, args) loc text =
  let push c = st.conds <- c :: st.conds in
  let outer = active st in
  let pop () = match st.conds with [] -> fail "%s without if" op | c :: rest -> (c, rest) in
  let truth s = value st s <> 0 in
  match op with
  | "if" | "ifdef" | "ifndef" ->
      let take =
        outer
        &&
        match op with
        | "if" -> truth args
        | "ifdef" -> (
            try
              ignore (lookup st (String.lowercase_ascii (trim args)));
              true
            with Undefined _ -> false)
        | _ -> (
            try
              ignore (lookup st (String.lowercase_ascii (trim args)));
              false
            with Undefined _ -> true)
      in
      push { active = take; taken = take; outer }
  | "else" ->
      let c, rest = pop () in
      st.conds <- { c with active = c.outer && not c.taken; taken = true } :: rest
  | "elseif" ->
      let c, rest = pop () in
      let take = c.outer && (not c.taken) && truth args in
      st.conds <- { c with active = take; taken = c.taken || take } :: rest
  | "endif" ->
      let _, rest = pop () in
      st.conds <- rest
  | _ -> if outer then execute st (label, op, args) loc text

and execute st (label, op, args) loc text =
  Buffer.clear st.line_bytes;
  let address = st.pc in
  let record () =
    st.listing <-
      { loc; address; bytes = Buffer.contents st.line_bytes; text = trim text } :: st.listing
  in
  (match (label, op) with
  | Some name, "equ" ->
      let v = value st args in
      define st name v
  | Some name, ("set" | "=") ->
      let v = value st args in
      if Hashtbl.mem st.syms name then fail "symbol %s is already defined" name;
      Hashtbl.replace st.sets name v
  | Some name, "macro" ->
      let params = List.map String.lowercase_ascii (split_args args) in
      let body = collect_body st in
      Hashtbl.replace st.macros (String.lowercase_ascii name) { params; body }
  | Some name, _ -> define st name st.pc
  | None, _ -> ());
  (match (label, op) with
  | Some _, ("equ" | "set" | "=" | "macro") | _, "" -> ()
  | _ -> statement st op args loc);
  record ()

and statement st op args loc =
  match op with
  | "cpu" -> if String.lowercase_ascii (trim args) <> "8080" then unsupported "cpu %s" (trim args)
  | "org" ->
      let v = value st args in
      range_check ~what:"origin" ~lo:0 ~hi:0xffff v;
      st.pc <- v
  | "db" -> List.iter (data st ~width:1) (split_args args)
  | "dw" -> List.iter (data st ~width:2) (split_args args)
  | "ds" -> (
      match split_args args with
      | [ n ] ->
          let v = value st n in
          if v < 0 then fail "negative ds count";
          st.pc <- st.pc + v
      | _ -> unsupported "ds with a fill value")
  | "end" -> st.stop <- true
  | "error" -> fail "%s" (match tokenize args with [ Str s ] -> s | _ -> trim args)
  | "title" | "page" | "listing" | "list" | "nolist" | "name" -> ()
  | "rept" ->
      let n = value st args in
      let body = collect_body st in
      if n < 0 then fail "negative rept count";
      let lines = List.init n (fun _ -> List.map (fun (t, l) -> (t, { loc with line = l })) body) in
      push_frame st (List.concat lines)
  | "endm" -> fail "endm without macro or rept"
  | "include" -> unsupported "include"
  | m when Hashtbl.mem st.macros m ->
      let mac = Hashtbl.find st.macros m in
      let args = split_args args in
      if List.length args > List.length mac.params then fail "macro %s: too many arguments" m;
      let args = args @ List.init (List.length mac.params - List.length args) (fun _ -> "") in
      st.uniq <- st.uniq + 1;
      if List.length loc.via > 50 then fail "macro nesting too deep";
      let via = (m, loc.line) :: loc.via in
      push_frame st
        (List.map
           (fun (t, l) ->
             (substitute ~params:mac.params ~args ~uniq:st.uniq t, { loc with line = l; via }))
           mac.body)
  | m when E.Opcode.of_mnemonic m <> None -> assemble_instruction st m (split_args args)
  | m -> unsupported "directive or instruction %s" m

let run_pass ~first ~prev ~file text =
  let st =
    {
      pc = 0;
      stmt_pc = 0;
      syms = Hashtbl.create 64;
      prev;
      sets = Hashtbl.create 16;
      macros = Hashtbl.create 16;
      conds = [];
      frames = [];
      cur = None;
      segs = [];
      errors = [];
      stop = false;
      uniq = 0;
      steps = 0;
      listing = [];
      line_bytes = Buffer.create 16;
      first;
    }
  in
  let lines = String.split_on_char '\n' text in
  push_frame st (List.mapi (fun i t -> (t, { file; line = i + 1; via = [] })) lines);
  let rec loop () =
    if not st.stop then
      match next_line st with
      | None -> ()
      | Some ((_, loc) as l) ->
          (try process st l with
          | Fail message -> st.errors <- { loc; message } :: st.errors
          | Undefined n -> st.errors <- { loc; message = "undefined symbol " ^ n } :: st.errors);
          loop ()
  in
  loop ();
  (if st.conds <> [] then
     let loc = { file; line = List.length lines; via = [] } in
     st.errors <- { loc; message = "missing endif" } :: st.errors);
  st

let finish st =
  Option.iter (fun s -> st.segs <- s :: st.segs) st.cur;
  st.cur <- None;
  List.sort compare (List.map (fun (o, b) -> (o, Buffer.contents b)) st.segs)

(** Assemble [text]. Passes repeat until the symbols stop changing (forward
    references through [equ] need more than two). *)
let assemble ?(file = "input") text =
  let rec go k prev =
    let st = run_pass ~first:(k = 1) ~prev ~file text in
    let same =
      Hashtbl.length prev = Hashtbl.length st.syms
      && Hashtbl.fold (fun n v ok -> ok && Hashtbl.find_opt prev n = Some v) st.syms true
    in
    if k > 1 && same then st
    else if k >= 8 then (
      st.errors <-
        { loc = { file; line = 0; via = [] }; message = "symbols do not converge (phase error)" }
        :: st.errors;
      st)
    else go (k + 1) st.syms
  in
  let st = go 1 (Hashtbl.create 1) in
  if st.errors <> [] then Error (List.rev st.errors)
  else
    let segs = finish st in
    let rec overlap = function
      | (o1, b1) :: ((o2, _) :: _ as rest) ->
          if o1 + String.length b1 > o2 then Some o2 else overlap rest
      | _ -> None
    in
    match overlap segs with
    | Some o ->
        Error
          [
            {
              loc = { file; line = 0; via = [] };
              message = Printf.sprintf "segments overlap at 0x%04x" o;
            };
          ]
    | None ->
        let segments = List.map (fun (origin, bytes) -> { origin; bytes }) segs in
        let symbols = List.sort compare (Hashtbl.fold (fun n v acc -> (n, v) :: acc) st.syms []) in
        Ok { segments; symbols; listing = List.rev st.listing }

(** The bytes of [range] (inclusive, default: lowest to highest emitted address),
    gaps filled with [fill]: the image [p2bin -l <fill> -r <range>] cuts. *)
let flatten ?(fill = '\000') ?range (img : image) =
  let lo, hi =
    match range with
    | Some (lo, hi) -> (lo, hi + 1)
    | None -> (
        match img.segments with
        | [] -> (0, 0)
        | (first : segment) :: _ ->
            let last : segment = List.fold_left (fun _ s -> s) first img.segments in
            (first.origin, last.origin + String.length last.bytes))
  in
  let out = Bytes.make (hi - lo) fill in
  List.iter
    (fun (s : segment) ->
      let from = max s.origin lo and upto = min (s.origin + String.length s.bytes) hi in
      if upto > from then Bytes.blit_string s.bytes (from - s.origin) out (from - lo) (upto - from))
    img.segments;
  (lo, Bytes.to_string out)

let hex_of s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

(** The listing map: address, bytes, source position and text of every line
    assembled, macro expansions included. *)
let listing_to_string (img : image) =
  String.concat "\n"
    (List.map
       (fun e ->
         let b = hex_of e.bytes in
         let b = if String.length b > 12 then String.sub b 0 12 ^ "+" else b in
         Format.asprintf "%04x %-13s %a  %s" e.address b pp_loc e.loc e.text)
       img.listing)
