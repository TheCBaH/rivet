(* The common directive table (asm/docs/contracts.md §3).

   Architecture-independent by construction: it names no target and branches on
   no target. What varies between dialects is which *spelling* appears - [.align]
   on x86, [.balign] on ARM and AArch64, [@function] where [@] is not a comment
   introducer and [%function] where it is - and all three differences are
   absorbed here, in one table, rather than by four copies of a directive
   handler.

   A directive not in this table and not claimed by the target is a diagnostic.
   That is §2.2, and it is the rule that stops a plausible image being produced
   from a file the assembler did not understand. *)

open Foundation
open Asm_core
open Asm_syntax

let text_of (slice : Token.slice) = Token.slice_text slice

(* [.section name,"flags",@type]. The flag string decides allocation: a section
   without [a] is never given an address, which is why [.note.GNU-stack] becomes
   a [Declared_section] and why M1.4's restriction can honestly be "reject a
   second *allocatable* section" while every fixture contains two [.section]-ish
   things. *)
(* The section *type* argument, in both spellings for [sym_kind_of]'s reason:
   [@] introduces a comment on ARM, so GAS spells it [%nobits] there. Only
   [nobits] is distinguished, because it is the one value that changes whether
   the section has real contents at all (M3 §6) - every other type spelling
   ([@progbits] and friends) means the ordinary case and is not tracked. *)
let is_nobits s = match s with "@nobits" | "%nobits" | "nobits" -> true | _ -> false

let section_of_args args =
  match args with
  | [] -> None
  | name :: rest ->
      let name = text_of name in
      let flags =
        match rest with
        | f :: _ -> ( match List.map Token.kind f with [ Token.String s ] -> Some s | _ -> None)
        | [] -> None
      in
      let allocatable = match flags with None -> true | Some s -> String.contains s 'a' in
      let nobits = match rest with _ :: t :: _ -> is_nobits (text_of t) | _ -> false in
      Some (name, allocatable, nobits)

let perms_of_section name =
  if name = ".text" then Perms.rx
  else if name = ".rodata" || (String.length name >= 8 && String.sub name 0 8 = ".rodata.") then
    Perms.ro
  else Perms.rw

let sym_kind_of s =
  (* [@function] and [%function] are the same thing: [@] introduces a comment on
     ARM, so GAS spells the type marker with [%] there. Accepting both here is
     what lets one table serve four dialects. *)
  match s with
  | "@function" | "%function" | "function" -> Some Directive.Function
  | "@object" | "%object" | "object" -> Some Directive.Object
  | "@notype" | "%notype" | "notype" -> Some Directive.Notype
  | _ -> None

let is_metadata name = String.length name >= 5 && String.sub name 0 5 = ".cfi_"

type outcome =
  | Normalized of Directive.t
  | Dropped  (** metadata: consumed, understood, and deliberately discarded *)
  | Unknown

(* A rejection may name its own diagnostic code. Almost none do - [.size needs a
   symbol] is an ordinary malformed directive and [simplify.directive] says
   everything about it - but a deferral is different: it is a scope statement
   that a test has to be able to name, and a caller grepping for "which
   directives does this milestone refuse on purpose" should find one code rather
   than a message substring. [reasons] retains typed parser causes where they
   exist; [pp_rejection] owns their eventual presentation. *)
type rejection = {
  code : string option;
  message : string;
  reasons : Parse_lines.error_kind Err.Error.t list;
}

let pp_rejection ppf r =
  match r.reasons with
  | [] -> Fmt.string ppf r.message
  | reasons ->
      Fmt.pf ppf "%s: %a" r.message
        Fmt.(list ~sep:(any "; ") (Err.Error.pp_kind Parse_lines.pp_error))
        reasons

let reject ?code ~pos message = Err.fail ~pos ~pp_error:pp_rejection { code; message; reasons = [] }
let reject_reasons ?code message reasons = { code; message; reasons }

let normalize ~data_widths ~name ~(arguments : Token.slice list) =
  let one () = match arguments with [ a ] -> Some (text_of a) | _ -> None in
  let int_arg () =
    match arguments with
    | [ a ] -> (
        match List.map Token.kind a with [ Token.Int v ] -> Bigint.to_int_opt v | _ -> None)
    | _ -> None
  in
  match name with
  | _ when is_metadata name -> Ok Dropped
  (* Asked before this table rather than after it, and by width rather than by
     spelling: [.word] is two bytes in GNU x86 syntax and four on ARM, so the
     dialect owns the name and the common code owns everything that follows
     from it. Each argument stays an expression - one naming a symbol becomes a
     fixup - so nothing is evaluated here. *)
  | _ when List.mem_assoc name data_widths -> (
      let width = List.assoc name data_widths in
      match arguments with
      | [] -> reject ~pos:__POS__ (name ^ " needs at least one value")
      | args ->
          Err.Accum.map ~pos:__POS__ Parse_lines.parse_expression args
          |> Err.Accum.fold_errors (reject_reasons (name ^ ": every value must be an expression"))
          |> Err.map (fun values -> Normalized (Directive.Data { width; values })))
  (* [.ascii]/[.asciz]/[.string]: one or more string-literal arguments,
     concatenated into raw byte data. [.ascii] emits exactly the decoded
     string bytes; [.asciz] and [.string] (a GAS synonym) each append a
     trailing NUL. The lexer has already resolved escapes (including octal)
     into [Token.String]'s payload, so no further decoding happens here.
     Reused as [Directive.Data { width = 1; ... }] - one [Const] byte value
     per character - rather than a new constructor, since every downstream
     consumer (layout, encoding, fixups) already knows how to turn that into
     bytes and nothing here needs a symbol reference or fixup. *)
  | ".ascii" | ".asciz" | ".string" -> (
      let string_of arg =
        match List.map Token.kind arg with [ Token.String s ] -> Some s | _ -> None
      in
      match arguments with
      | [] -> reject ~pos:__POS__ (name ^ " needs at least one string literal")
      | args -> (
          match List.map string_of args with
          | opts when List.for_all Option.is_some opts ->
              let nul_terminated = name <> ".ascii" in
              let chars =
                List.concat_map
                  (fun s ->
                    let cs = List.init (String.length s) (String.get s) in
                    if nul_terminated then cs @ [ '\000' ] else cs)
                  (List.map Option.get opts)
              in
              let values = List.map (fun c -> Expr.Const (Bigint.of_int (Char.code c))) chars in
              Ok (Normalized (Directive.Data { width = 1; values }))
          | _ -> reject ~pos:__POS__ (name ^ ": every argument must be a string literal")))
  | ".bss" ->
      Ok
        (Normalized
           (Directive.Section { name = ".bss"; perms = perms_of_section ".bss"; nobits = true }))
  | ".text" | ".data" ->
      let n = name in
      Ok (Normalized (Directive.Section { name = n; perms = perms_of_section n; nobits = false }))
  | ".section" -> (
      match section_of_args arguments with
      | None -> reject ~pos:__POS__ "a .section directive needs a name"
      | Some (n, true, nobits) ->
          Ok (Normalized (Directive.Section { name = n; perms = perms_of_section n; nobits }))
      | Some (n, false, _) -> Ok (Normalized (Directive.Declared_section { name = n })))
  (* M3 §6: reservation-only, single-argument form. A fill byte
     ([.space size,fill]) is rejected explicitly below rather than silently
     ignored, since ignoring it would make [.space 4,1] mean something
     different from what it says. *)
  | ".zero" | ".space" -> (
      match int_arg () with
      | Some n when n >= 0 -> Ok (Normalized (Directive.Zero { length = n }))
      | Some _ -> reject ~pos:__POS__ (name ^ " needs a non-negative length")
      | None -> (
          match arguments with
          | [ _; _ ] -> reject ~pos:__POS__ (name ^ ": a fill-byte argument is not supported")
          | _ -> reject ~pos:__POS__ (name ^ " needs exactly one integer argument")))
  (* [.align] and [.balign] normalize to the same constructor with different
     values. On the four targets here both denote a byte count; a target where
     [.align] means a power of two states that in its own directive handler,
     and the table above is the checked claim rather than a general one. *)
  | ".align" | ".balign" -> (
      match int_arg () with
      | Some n when n > 0 -> Ok (Normalized (Directive.Align { boundary = n }))
      | _ -> reject ~pos:__POS__ (name ^ " needs a positive integer argument"))
  | ".p2align" ->
      (* Deliberately rejected rather than accepted as a synonym. Its argument
         is an exponent, not a byte count, so treating it as one would silently
         align [.p2align 4] to four bytes instead of sixteen - a difference that
         produces a valid image and wrong addresses. It is absent from
         asm/docs/contracts.md §3's table, and nothing outside that table
         assembles. *)
      reject ~pos:__POS__
        ".p2align takes a power-of-two exponent and is not in M2 scope; use .balign"
  | ".globl" | ".global" -> (
      match one () with
      | Some n -> Ok (Normalized (Directive.Global { name = n }))
      | None -> reject ~pos:__POS__ (name ^ " needs exactly one symbol"))
  | ".weak" -> (
      match one () with
      | Some n -> Ok (Normalized (Directive.Weak { name = n }))
      | None -> reject ~pos:__POS__ (name ^ " needs exactly one symbol"))
  | ".local" -> (
      match one () with
      | Some n -> Ok (Normalized (Directive.Local { name = n }))
      | None -> reject ~pos:__POS__ (name ^ " needs exactly one symbol"))
  | ".comm" -> (
      let int_of_slice slice =
        match List.map Token.kind slice with [ Token.Int v ] -> Bigint.to_int_opt v | _ -> None
      in
      match arguments with
      | [ n; s ] -> (
          match int_of_slice s with
          | Some size when size >= 0 ->
              Ok (Normalized (Directive.Common { name = text_of n; size; align = 1 }))
          | _ -> reject ~pos:__POS__ ".comm needs a non-negative integer size")
      | [ n; s; a ] -> (
          match (int_of_slice s, int_of_slice a) with
          | Some size, Some align when size >= 0 && align > 0 ->
              Ok (Normalized (Directive.Common { name = text_of n; size; align }))
          | _ -> reject ~pos:__POS__ ".comm needs a non-negative size and a positive alignment")
      | _ -> reject ~pos:__POS__ ".comm needs a symbol and a size, and optionally an alignment")
  | ".type" -> (
      match arguments with
      | [ n; k ] -> (
          match sym_kind_of (text_of k) with
          | Some kind -> Ok (Normalized (Directive.Sym_type { name = text_of n; kind }))
          | None -> reject ~pos:__POS__ ("unknown symbol type " ^ text_of k))
      | _ -> reject ~pos:__POS__ ".type needs a symbol and a type")
  | ".size" -> (
      match arguments with
      | [ n; e ] ->
          (* [lift] keeps the parser's wrapper and [fold_errors] changes only
             the published payload domain. Rendering here would flatten the
             expression domain at exactly the boundary where
             asm/docs/errors.md says to wrap it instead. *)
          Parse_lines.parse_expression e |> Err.Accum.lift
          |> Err.Accum.fold_errors (reject_reasons ".size")
          |> Err.map (fun expr -> Normalized (Directive.Sym_size { name = text_of n; size = expr }))
      | _ -> reject ~pos:__POS__ ".size needs a symbol and an expression")
  | _ -> Ok Unknown
