(* The AArch64 front end: how A64 source text becomes operands.

   {!Aarch64_encode} is the rest of the target - the instruction types and
   everything that turns one into bytes - and this file is what {!Target.TARGET}
   adds on top of {!Target_encode.ENCODE}: a lexical profile, an operand parser,
   and a directive handler. The two are separate libraries so that a consumer
   which builds A64 instructions as values rather than reading them links the
   encoder alone, with no lexer and no Menhir table behind it.

   [include] rather than functor application: this module *is* the aarch64
   target, so its type identities have to be the encoder's own. Everything
   {!Aarch64_encode} exports is re-exported here, which is also what lets the
   parser below use [diag], [Reg], [Operand] and the rest without qualification. *)

open Foundation
include Aarch64_encode

(* {1 Lexical profile}

   [//] introduces a comment, so the profile's longest-first matching matters:
   a dialect that also used [/] would otherwise take the first character and
   leave the second as a divide. *)
let lexical_profile =
  Asm_syntax.Lexical_profile.make ~name:"aarch64" ~comment_introducers:[ "//" ]
    ~statement_separator:';' ~immediate_sigil:'#' ~modifier_syntax:[ "lo12" ] ()
(* {1 Operand parsing}

   Two things the common parser cannot do, and the reason §5.1's one-operand-
   at-a-time signature was replaced. First, [stp x15, x30, [sp, #-16]!] has a
   comma inside the bracketed operand, so the slices arrive split and are
   rejoined here by bracket depth. Second, the [!] belongs to the whole memory
   operand rather than to the token before it. *)

let regroup (slices : Asm_syntax.Token.slice list) =
  let depth_of slice =
    List.fold_left
      (fun d t ->
        match Asm_syntax.Token.kind t with
        | Asm_syntax.Token.Lbracket | Asm_syntax.Token.Lbrace -> d + 1
        | Asm_syntax.Token.Rbracket | Asm_syntax.Token.Rbrace -> d - 1
        | _ -> d)
      0 slice
  in
  (* The separator itself is gone: the grammar consumed every top-level comma
     as a slice boundary, so rejoining is concatenation and the rejoined operand
     reads [[ sp # 0 ]]. Re-inserting a comma token would be inventing source
     text, and the spans would then not be the lexer's. *)
  let rec go acc pending depth = function
    | [] -> if pending = [] then Ok (List.rev acc) else Error "unbalanced brackets in operand"
    | s :: rest ->
        let d = depth + depth_of s in
        let pending = pending @ s in
        if d = 0 then go (pending :: acc) [] 0 rest
        else if d < 0 then Error "unbalanced brackets in operand"
        else go acc pending d rest
  in
  go [] [] 0 slices

let slice_origin (s : Asm_syntax.Token.slice) =
  match Asm_syntax.Token.slice_span s with
  | Some sp -> Origin.text sp
  | None -> Origin.synthesized ~pass:"aarch64" ()

let parse_one (slice : Asm_syntax.Token.slice) =
  let origin = slice_origin slice in
  let bad msg = Error (diag ~origin "aarch64.operand" msg) in
  let open Asm_core in
  let open Asm_syntax in
  let mem ~base ~offset ~writeback =
    match Reg.find base with
    | Some r -> Ok (Operand.Mem { Mem.base = r; offset; writeback; pre = true })
    | None -> bad ("unknown register " ^ base)
  in
  let const_mem ~base ~offset ~writeback = mem ~base ~offset:(Disp.Const offset) ~writeback in
  let int64_of v = match Bigint.to_int64_opt v with Some x -> Some x | None -> None in
  match List.map Token.kind slice with
  | [ Token.Ident n ] -> (
      (* A bare identifier is a register if the table knows it and a symbol
         otherwise. There is no sigil to tell them apart on this dialect, so
         the table is the only thing that can. *)
      match Reg.find n with
      | Some r -> Ok (Operand.Reg r)
      | None -> Ok (Operand.Sym (Expr.Symbol n)))
  | [ Token.Immediate_sigil; Token.Int v ] -> Ok (Operand.Imm v)
  | [ Token.Immediate_sigil; Token.Minus; Token.Int v ] -> Ok (Operand.Imm (Bigint.neg v))
  | [
   Token.Ident (("lsl" | "lsr" | "asr" | "ror" | "uxtw" | "sxtw") as k);
   Token.Immediate_sigil;
   Token.Int v;
  ] -> (
      match Bigint.to_int_opt v with
      | Some n -> Ok (Operand.Shift { Shift.kind = k; amount = n })
      | None -> bad "shift amount does not fit")
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket ] ->
      const_mem ~base:b ~offset:0L ~writeback:false
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket; Token.Bang ] ->
      const_mem ~base:b ~offset:0L ~writeback:true
  | [ Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:o ~writeback:false
      | None -> bad "offset does not fit")
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket; Token.Bang;
  ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:o ~writeback:true
      | None -> bad "offset does not fit")
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Minus; Token.Int v; Token.Rbracket;
  ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:(Int64.neg o) ~writeback:false
      | None -> bad "offset does not fit")
  | [
   Token.Lbracket;
   Token.Ident b;
   Token.Immediate_sigil;
   Token.Minus;
   Token.Int v;
   Token.Rbracket;
   Token.Bang;
  ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:(Int64.neg o) ~writeback:true
      | None -> bad "offset does not fit")
  (* [[x16, #:lo12:g]] - a memory operand whose offset is an expression rather
     than a number (§D2). The bracket and sigil are stripped here and the middle
     handed to the common expression parser, which is what knows [:lo12:g+4]
     means the same thing in an operand as in a directive argument. *)
  | Token.Lbracket :: Token.Ident b :: Token.Immediate_sigil :: Token.Modifier _ :: _
    when match List.rev (List.map Token.kind slice) with Token.Rbracket :: _ -> true | _ -> false
    -> (
      let inner =
        match slice with
        | _ :: _ :: _ :: rest -> List.filteri (fun i _ -> i < List.length rest - 1) rest
        | _ -> []
      in
      match Asm_syntax.Parse_lines.parse_expression inner with
      | Ok e -> mem ~base:b ~offset:(Disp.Sym e) ~writeback:false
      | Error _ -> bad ("cannot parse operand " ^ Asm_syntax.Token.slice_text slice))
  | _ -> (
      (* A branch or call target: not a register, an immediate or an address, but
         still an expression. The common parser decides what it means. *)
      match Asm_syntax.Parse_lines.parse_expression slice with
      | Ok e -> Ok (Operand.Sym e)
      | Error _ -> bad ("cannot parse operand " ^ Asm_syntax.Token.slice_text slice))

let parse_operands ~mnemonic slices =
  ignore mnemonic;
  match regroup slices with
  | Error m -> Error (diag "aarch64.operand" m)
  | Ok groups ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | s :: rest -> ( match parse_one s with Ok o -> go (o :: acc) rest | Error e -> Error e)
      in
      go [] groups

let handle_directive ~name ~argument state =
  ignore name;
  ignore argument;
  ignore state;
  (* A64 needs no target-state directive for the M1 fixtures: unlike ARM there
     is no [.syntax], no [.arch] and no [.fpu] in them. *)
  Target_intf.Target.Unhandled
