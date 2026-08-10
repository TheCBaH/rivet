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

let text_of (slice : Token.slice) = Token.slice_text slice

(* [.section name,"flags",@type]. The flag string decides allocation: a section
   without [a] is never given an address, which is why [.note.GNU-stack] becomes
   a [Declared_section] and why M1.4's restriction can honestly be "reject a
   second *allocatable* section" while every fixture contains two [.section]-ish
   things. *)
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
      Some (name, allocatable)

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
      | [] -> Error (name ^ " needs at least one value")
      | args ->
          let parsed = List.map Parse_lines.parse_expression args in
          if List.exists Result.is_error parsed then
            Error (name ^ ": every value must be an expression")
          else
            Ok
              (Normalized
                 (Directive.Data { width; values = List.map (fun r -> Result.get_ok r) parsed })))
  | ".text" | ".data" | ".bss" ->
      let n = name in
      Ok (Normalized (Directive.Section { name = n; perms = perms_of_section n }))
  | ".section" -> (
      match section_of_args arguments with
      | None -> Error "a .section directive needs a name"
      | Some (n, true) ->
          Ok (Normalized (Directive.Section { name = n; perms = perms_of_section n }))
      | Some (n, false) -> Ok (Normalized (Directive.Declared_section { name = n })))
  (* [.align] and [.balign] normalize to the same constructor with different
     values. On the four targets here both denote a byte count; a target where
     [.align] means a power of two states that in its own directive handler,
     and the table above is the checked claim rather than a general one. *)
  | ".align" | ".balign" -> (
      match int_arg () with
      | Some n when n > 0 -> Ok (Normalized (Directive.Align { boundary = n }))
      | _ -> Error (name ^ " needs a positive integer argument"))
  | ".p2align" ->
      (* Deliberately rejected rather than accepted as a synonym. Its argument
         is an exponent, not a byte count, so treating it as one would silently
         align [.p2align 4] to four bytes instead of sixteen - a difference that
         produces a valid image and wrong addresses. It is absent from
         asm/docs/contracts.md §3's table, and nothing outside that table
         assembles. *)
      Error ".p2align takes a power-of-two exponent and is not in M1 scope; use .balign"
  | ".globl" | ".global" -> (
      match one () with
      | Some n -> Ok (Normalized (Directive.Global { name = n }))
      | None -> Error (name ^ " needs exactly one symbol"))
  | ".type" -> (
      match arguments with
      | [ n; k ] -> (
          match sym_kind_of (text_of k) with
          | Some kind -> Ok (Normalized (Directive.Sym_type { name = text_of n; kind }))
          | None -> Error ("unknown symbol type " ^ text_of k))
      | _ -> Error ".type needs a symbol and a type")
  | ".size" -> (
      match arguments with
      | [ n; e ] -> (
          match Parse_lines.parse_expression e with
          | Ok expr -> Ok (Normalized (Directive.Sym_size { name = text_of n; size = expr }))
          | Error m -> Error (".size: " ^ m))
      | _ -> Error ".size needs a symbol and an expression")
  | _ -> Ok Unknown
