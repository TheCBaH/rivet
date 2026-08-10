(* The lowered module AST (.ai/asm_plan.md §4.4, §7).

   One input unit, whether parsed or constructed directly, in the form the image
   builder consumes: logical sections with permissions, ordered fragments,
   symbol declarations, and fixups. It has the same information an object file
   would carry because the internal linker needs it, and is never serialized as
   one.

   No addresses. A fragment knows its bytes and its alignment demand; where it
   lands is layout's answer, and a lowered module that already contained
   addresses could not be laid out twice.

   Parameterized over the target's fixup-kind type. M1 emits no fixups at all -
   .text in all four fixtures is relocation-free, the only relocation being the
   .eh_frame PC-relative entries from .cfi_*, which are discarded - so every
   [fixups] list here is empty today. The parameter is present because the
   alternative is a string-typed kind that M2 would have to replace everywhere. *)

open Foundation

(* A fixup as the *linker* needs it: a byte offset into the fragment, a bit
   range within that, the target kind, and the expression to evaluate. It is
   deliberately not Codec.placement - asm_core sits below codec in the
   dependency DAG (§5.1) and must stay there - and the shapes differ anyway,
   because a codec places bits inside one instruction while a linker patches
   bytes inside a section. The target converts. *)
type 'k fixup = {
  kind : 'k;
  name : string;
  byte_offset : int;
  bit_offset : int;
  bit_width : int;
  value : Expr.t;
  origin : Origin.t;
}

type 'k fragment =
  | Bytes of { bytes : string; form : string option; fixups : 'k fixup list; origin : Origin.t }
      (** [form] is the codec's form identifier: the path of alternatives its encoding took. It is
          what the diagnostic disassembler prints and what a differential test names when an
          encoding is right by accident. *)
  | Align of { boundary : int; fill : string; origin : Origin.t }
      (** [fill] is the target's padding, not zero by default: x86 pads code with multi-byte no-ops
          and a zero-filled gap in .text would be a decodable instruction. *)
  | Zero of { length : int; origin : Origin.t }
  | Label_def of { name : string; origin : Origin.t }
  | Set_size of { name : string; size : Expr.t; origin : Origin.t }
      (** [.size s, . - s] as a zero-length fragment rather than a field of the symbol. The
          expression mentions the current location, so its value depends on *where the directive
          appeared*; a symbol-level field would have lost that position and had to guess. *)

type section = { sec_name : string; perms : Perms.t; alignment : int }
type 'k section_content = { sec : section; fragments : 'k fragment list }

type symbol = {
  name : string;
  global : bool;
  kind : Directive.sym_kind;
  size : Expr.t option;
  section : string;
}

type 'k module_ = {
  unit_name : string;
  sections : 'k section_content list;
  symbols : symbol list;
  declared_sections : string list;
      (** sections named by the input that are never allocated - [.note.GNU-stack]. Recorded so a
          later pass can report them, and deliberately not section objects. *)
}

let pp_fragment ppf = function
  | Bytes { bytes; form; _ } ->
      Fmt.pf ppf "bytes %-24s %s" (Byte_cursor.to_hex bytes)
        (match form with None -> "" | Some f -> "[" ^ f ^ "]")
  | Align { boundary; fill; _ } -> Fmt.pf ppf "align %d fill=%s" boundary (Byte_cursor.to_hex fill)
  | Zero { length; _ } -> Fmt.pf ppf "zero %d" length
  | Label_def { name; _ } -> Fmt.pf ppf "label %s" name
  | Set_size { name; size; _ } -> Fmt.pf ppf "size %s = %a" name Expr.pp size

let pp_symbol ppf s =
  Fmt.pf ppf "%s %s %s in %s%a"
    (if s.global then "global" else "local")
    s.name (Directive.sym_kind_name s.kind) s.section
    Fmt.(option (any " size=" ++ using Expr.to_string string))
    s.size

let pp ppf m =
  Fmt.pf ppf "@[<v>lowered %s@,%a@,%a%a@]" m.unit_name
    Fmt.(
      list ~sep:cut (fun ppf sc ->
          Fmt.pf ppf "@[<v2>section %s %a align=%d@,%a@]" sc.sec.sec_name Perms.pp sc.sec.perms
            sc.sec.alignment
            Fmt.(list ~sep:cut pp_fragment)
            sc.fragments))
    m.sections
    Fmt.(list ~sep:cut pp_symbol)
    m.symbols
    Fmt.(list ~sep:nop (fun ppf n -> Fmt.pf ppf "@,declared %s (not allocated)" n))
    m.declared_sections
