(* The common directive table (docs/contracts.md §3).

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

let has_prefix name prefix =
  String.length name >= String.length prefix && String.sub name 0 (String.length prefix) = prefix

let perms_of_section name =
  if name = ".text" then Perms.rx
  else if name = ".rodata" || has_prefix name ".rodata." then Perms.ro
  else Perms.rw

(* With a flag string the flags decide ([a] readable, [w], [x]); the name table
   answers only when there is none. *)
let perms_of_flags name = function
  | None -> perms_of_section name
  | Some f -> { Perms.read = true; write = String.contains f 'w'; execute = String.contains f 'x' }

(* GNU as gives a section named like [.bss] or [.tbss] the NOBITS type when none is
   written. *)
let nobits_by_name name =
  List.exists
    (fun base -> String.equal name base || has_prefix name (base ^ "."))
    [ ".bss"; ".tbss" ]

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
      let nobits =
        match rest with _ :: t :: _ -> is_nobits (text_of t) | _ -> nobits_by_name name
      in
      Some (name, allocatable, nobits, perms_of_flags name flags)

let sym_kind_of s =
  (* [@function] and [%function] are the same thing: [@] introduces a comment on
     ARM, so GAS spells the type marker with [%] there. Accepting both here is
     what lets one table serve four dialects. *)
  match s with
  | "@function" | "%function" | "function" -> Some Directive.Function
  | "@object" | "%object" | "object" -> Some Directive.Object
  | "@notype" | "%notype" | "notype" -> Some Directive.Notype
  | _ -> None

(* [.file]/[.ident] are GAS's own compiler-provenance metadata ([.file "x.c"],
   [.ident "GCC: ..."]) - like [.cfi_*], they change nothing this project
   assembles and are dropped rather than rejected, the same "understood and
   discarded, not silently ignored as unknown" distinction §2.2 draws (M5,
   the corpus notes: gcc's own freestanding riscv.c helper output carries
   both on every profile). Real gcc emits both on every target, not just
   RISC-V, but classify-c-gcc's own check stops at --dump-source-ast - parse
   only, never reaching simplify, the phase that rejected these - so this is
   the first time gcc-compiled output has been run through the full pipeline
   far enough to see it. *)
let is_metadata name =
  (String.length name >= 5 && String.sub name 0 5 = ".cfi_")
  || String.equal name ".file" || String.equal name ".ident"

type outcome =
  | Normalized of Directive.t
  | Normalized_many of Directive.t list  (** one directive naming several symbols *)
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

(* A constant integer argument: any expression that folds without a symbol. *)
let constant (slice : Token.slice) =
  match Parse_lines.parse_expression slice with
  | Error _ -> None
  | Ok e -> ( match Expr.absolute Expr.no_env e with Ok v -> Some v | Error _ -> None)

let constant_int slice = Option.bind (constant slice) Bigint.to_int_opt

(* The symbols a binding or visibility directive names: one identifier per argument. *)
let symbol_names arguments =
  let name = function
    | [ t ] -> ( match Token.kind t with Token.Ident n | Token.Directive n -> Some n | _ -> None)
    | _ -> None
  in
  let names = List.map name arguments in
  if arguments <> [] && List.for_all Option.is_some names then Some (List.map Option.get names)
  else None

let uleb128 v =
  let b = Buffer.create 10 in
  let rec go v =
    let byte = Int64.to_int (Int64.logand v 0x7fL) in
    let rest = Int64.shift_right_logical v 7 in
    if rest = 0L then Buffer.add_char b (Char.chr byte)
    else (
      Buffer.add_char b (Char.chr (byte lor 0x80));
      go rest)
  in
  go v;
  Buffer.contents b

let sleb128 v =
  let b = Buffer.create 10 in
  let rec go v =
    let byte = Int64.to_int (Int64.logand v 0x7fL) in
    let rest = Int64.shift_right v 7 in
    if (rest = 0L && byte land 0x40 = 0) || (rest = -1L && byte land 0x40 <> 0) then
      Buffer.add_char b (Char.chr byte)
    else (
      Buffer.add_char b (Char.chr (byte lor 0x80));
      go rest)
  in
  go v;
  Buffer.contents b

(* The most a filled [.space] may reserve as real bytes. *)
let max_filled_space = 1 lsl 24

(* A trailing comma leaves an empty last slot, which GNU as ignores everywhere but in the
   alignment directives, where [.p2align 4,] (a zero fill) and [.p2align 4,,] (the default
   fill) differ. *)
let rec drop_trailing_empty = function
  | [] -> []
  | l -> ( match List.rev l with [] :: rest -> drop_trailing_empty (List.rev rest) | _ -> l)

let arguments_of ~name arguments =
  match name with
  | ".align" | ".balign" | ".p2align" -> arguments
  | _ -> drop_trailing_empty arguments

let normalize ~data_widths ~align_unit ~name ~(arguments : Token.slice list) =
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
     One [Directive.Ascii] string: nothing here needs a symbol reference or
     fixup, and a [Const] value per character costs a Bigint each. *)
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
              let bytes =
                String.concat ""
                  (List.map
                     (fun s -> if nul_terminated then Option.get s ^ "\000" else Option.get s)
                     opts)
              in
              Ok (Normalized (Directive.Ascii { bytes }))
          | _ -> reject ~pos:__POS__ (name ^ ": every argument must be a string literal")))
  (* GNU as takes a subsection number here; there are no subsections, so an argument is
     rejected rather than ignored. *)
  | (".bss" | ".text" | ".data") when arguments <> [] ->
      reject ~pos:__POS__ (name ^ ": a subsection argument is not supported")
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
      | Some (n, true, nobits, perms) ->
          Ok (Normalized (Directive.Section { name = n; perms; nobits }))
      | Some (n, false, _, _) -> Ok (Normalized (Directive.Declared_section { name = n })))
  (* [.zero n] and [.space n[, fill]] ([.skip] is GNU's third spelling): [n] is a constant
     expression. A zero or absent fill is a reservation, which the image layer makes real
     bytes or pure extent according to the section's kind; any other fill is real bytes. *)
  | ".zero" | ".space" | ".skip" -> (
      let size = match arguments with s :: _ -> constant_int s | [] -> None in
      let fill =
        match arguments with
        | [ _ ] | [ _; [] ] -> Some 0
        | [ _; f ] -> Option.map (fun v -> v land 0xff) (constant_int f)
        | _ -> None
      in
      match (arguments, size, fill) with
      | ([ _ ] | [ _; _ ]), Some n, Some f when n >= 0 ->
          if f = 0 then Ok (Normalized (Directive.Zero { length = n }))
          else if n > max_filled_space then
            reject ~pos:__POS__ (name ^ ": a filled reservation is limited to 16 MiB")
          else Ok (Normalized (Directive.Ascii { bytes = String.make n (Char.chr f) }))
      | ([ _ ] | [ _; _ ]), Some _, Some _ ->
          reject ~pos:__POS__ (name ^ " needs a non-negative length")
      | _ -> reject ~pos:__POS__ (name ^ " needs a constant length and an optional constant fill"))
  (* [.balign n] is a byte count; [.align n] is a byte count or an exponent according to the
     dialect; [.p2align n] is always an exponent, capped at 29 (the largest whose power of two is
     a positive [int] on every build, 31-bit ones included), so read as a byte count
     [.p2align 4] would be a valid image at wrong addresses. Either way the boundary is a power
     of two, as GNU as requires, and 0 is accepted: [.balign 0] and a byte-count [.align 0] pad
     nothing, and an exponent-unit [.align 0] stands for [zero_exponent].

     The optional second operand is the fill byte and the third a limit: when more bytes than
     that would be needed the padding is skipped (0 means no limit, as in GNU as). Either may
     be left empty ([.p2align 4,,10]). *)
  | ".align" | ".balign" | ".p2align" -> (
      let power_of_two n = n land (n - 1) = 0 in
      let operand = function [] -> Some None | s -> Option.map Option.some (constant_int s) in
      let fill, max_skip, shape_ok =
        match arguments with
        | [ _ ] -> (Some None, Some None, true)
        (* GNU as: a comma with nothing after it asks for a zero fill, unlike an empty fill
           followed by a maximum, which leaves the default. *)
        | [ _; [] ] -> (Some (Some 0), Some None, true)
        | [ _; f ] -> (operand f, Some None, true)
        | [ _; f; m ] -> (operand f, operand m, true)
        | _ -> (None, None, false)
      in
      match (shape_ok, match arguments with n :: _ -> constant_int n | [] -> None) with
      | true, Some n when n >= 0 -> (
          let boundary =
            match (name, align_unit) with
            | ".p2align", _ -> if n > 29 then None else Some (1 lsl n)
            | ".align", Directive.Exponent { zero_exponent } ->
                if n > 29 then None else Some (1 lsl if n = 0 then zero_exponent else n)
            | _ -> if n = 0 then Some 1 else if power_of_two n then Some n else None
          in
          match (boundary, fill, max_skip) with
          | Some boundary, Some fill, Some max_skip -> (
              match max_skip with
              | Some m when m < 0 -> reject ~pos:__POS__ (name ^ ": the maximum skip is negative")
              | _ ->
                  let fill = Option.map (fun f -> f land 0xff) fill in
                  Ok (Normalized (Directive.Align { boundary; fill; max_skip })))
          | None, _, _ ->
              reject ~pos:__POS__
                (if name = ".p2align" then ".p2align needs an exponent from 0 to 29"
                 else name ^ ": the alignment is not a power of two")
          | _ -> reject ~pos:__POS__ (name ^ ": the fill and maximum skip must be constants"))
      | false, _ ->
          reject ~pos:__POS__
            (name ^ " takes an alignment, an optional fill and an optional maximum skip")
      | _ -> reject ~pos:__POS__ (name ^ " needs a non-negative constant alignment"))
  | ".globl" | ".global" | ".weak" | ".local" -> (
      match symbol_names arguments with
      | Some names ->
          let mk n =
            match name with
            | ".weak" -> Directive.Weak { name = n }
            | ".local" -> Directive.Local { name = n }
            | _ -> Directive.Global { name = n }
          in
          Ok (Normalized_many (List.map mk names))
      | None -> reject ~pos:__POS__ (name ^ " needs one or more symbols"))
  | ".hidden" | ".protected" | ".internal" -> (
      let visibility =
        match name with
        | ".hidden" -> Directive.Hidden
        | ".protected" -> Directive.Protected
        | _ -> Directive.Internal
      in
      match symbol_names arguments with
      | Some names ->
          Ok
            (Normalized_many
               (List.map (fun n -> Directive.Visibility { name = n; visibility }) names))
      | None -> reject ~pos:__POS__ (name ^ " needs one or more symbols"))
  | ".uleb128" | ".sleb128" -> (
      let signed = name = ".sleb128" in
      let encode slice =
        match constant slice with
        | None -> None
        | Some v ->
            if signed then Option.map sleb128 (Bigint.to_int64_opt v)
            else if Bigint.sign v < 0 then None
            else Option.map uleb128 (Bigint.to_uint64_opt v)
      in
      match arguments with
      | [] -> reject ~pos:__POS__ (name ^ " needs at least one value")
      | args ->
          let encoded = List.map encode args in
          if List.for_all Option.is_some encoded then
            Ok
              (Normalized
                 (Directive.Ascii { bytes = String.concat "" (List.map Option.get encoded) }))
          else
            reject ~pos:__POS__
              (name ^ ": every value must be a constant"
              ^ if signed then " that fits 64 bits" else " from 0 to 2^64-1"))
  | ".pushsection" -> (
      match section_of_args arguments with
      | None -> reject ~pos:__POS__ "a .pushsection directive needs a name"
      | Some (n, true, nobits, perms) ->
          Ok (Normalized (Directive.Push_section (Directive.Section { name = n; perms; nobits })))
      | Some (n, false, _, _) ->
          Ok (Normalized (Directive.Push_section (Directive.Declared_section { name = n }))))
  | ".popsection" when arguments = [] -> Ok (Normalized Directive.Pop_section)
  | ".previous" when arguments = [] -> Ok (Normalized Directive.Previous_section)
  | ".popsection" | ".previous" -> reject ~pos:__POS__ (name ^ " takes no arguments")
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
          | Some size, Some align when size >= 0 && align > 0 && align land (align - 1) = 0 ->
              Ok (Normalized (Directive.Common { name = text_of n; size; align }))
          | _ -> reject ~pos:__POS__ ".comm needs a non-negative size and a power-of-two alignment")
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
             docs/errors.md says to wrap it instead. *)
          Parse_lines.parse_expression e |> Err.Accum.lift
          |> Err.Accum.fold_errors (reject_reasons ".size")
          |> Err.map (fun expr -> Normalized (Directive.Sym_size { name = text_of n; size = expr }))
      | _ -> reject ~pos:__POS__ ".size needs a symbol and an expression")
  | _ -> Ok Unknown
