(* The Intel 8080 encoder: instruction types, lowering, the codec, decoding and
   the canonical printer.

   Everything instruction-specific is read from {!I8080_ops}. This library
   satisfies {!Target_encode.ENCODE} and reaches no lexer; {!I8080} adds the
   text front end. The 8080 has no relative branches, no prefixes and one
   instruction length per opcode, so every encoding is [`Fixed]. *)

open Foundation
module C = Codec
module Ops = I8080_ops
module L = Asm_core.Lowered_ast

let name = "i8080"
let triple = "i8080-unknown-none"

(* {1 Types} *)

module Reg = struct
  (* The [R8] field order: B C D E H L M A = 0..7. [M] is the memory operand
     at (HL), spelled as a register. *)
  type t = B | C | D | E | H | L | M | A

  let all = [ B; C; D; E; H; L; M; A ]
  let to_int = function B -> 0 | C -> 1 | D -> 2 | E -> 3 | H -> 4 | L -> 5 | M -> 6 | A -> 7
  let of_int n = List.nth_opt all n
  let name r = Ops.r8_names.(to_int r)
  let find s = List.find_opt (fun r -> String.equal (name r) (String.lowercase_ascii s)) all
end

module Operand = struct
  type t = Reg of Reg.t | Sp | Psw | Imm of Bigint.t | Sym of Asm_core.Expr.t | Str of string

  let pp ppf = function
    | Reg r -> Fmt.string ppf (Reg.name r)
    | Sp -> Fmt.string ppf "sp"
    | Psw -> Fmt.string ppf "psw"
    | Imm v -> Bigint.pp ppf v
    | Sym e -> Fmt.string ppf (Asm_core.Expr.to_string e)
    | Str s -> Fmt.pf ppf "%S" s
end

module Opcode = struct
  type t = Ops.entry

  let of_mnemonic s = Ops.find_entry (String.lowercase_ascii s)
end

type error_kind =
  [ Target_error.shared
  | `Bad_operand of bad_operand
  | `Out_of_range of out_of_range
  | `Codec of Codec.error
  | `Decode_short
  | `Decode_no_match
  | `Fixup_out_of_range of int64 * int
  | `No_data_relocation of int
  | `Org_misplaced
  | `Address_space of int * int
  | `Many_sections
  | `No_padding ]

and bad_operand = { mnemonic : string; index : int; expected : string }
and out_of_range = { what : string; value : Bigint.t; low : int; high : int }

type error = error_kind Target_error.t

let pp_error_kind ppf : error_kind -> unit = function
  | #Target_error.shared as e -> Target_error.pp_shared ppf e
  | `Bad_operand { mnemonic; index; expected } ->
      Fmt.pf ppf "%s: operand %d must be %s" mnemonic (index + 1) expected
  | `Out_of_range { what; value; low; high } ->
      Fmt.pf ppf "%s %a does not fit %d..%d" what Bigint.pp value low high
  | `Codec e -> Codec.pp_error ppf e
  | `Decode_short -> Fmt.string ppf "instruction is cut short"
  | `Decode_no_match -> Fmt.string ppf "no instruction matches these bytes"
  | `Fixup_out_of_range (v, bits) -> Fmt.pf ppf "value %Ld does not fit %d bits" v bits
  | `No_data_relocation w -> Fmt.pf ppf "no absolute relocation for %d-byte data" w
  | `Org_misplaced -> Fmt.string ppf "org must precede every instruction and datum, and appear once"
  | `Address_space (size, origin) ->
      Fmt.pf ppf "image of %d bytes at 0x%04x leaves the 64 KiB address space" size origin
  | `Many_sections -> Fmt.string ppf "only one section is supported"
  | `No_padding -> Fmt.string ppf "the 8080 has no alignment padding: .align is not supported"

let error_kind_code : error_kind -> string = function
  | `Unknown_instruction _ | `No_form _ | `Bad_operand _ | `Feature_disabled _ -> "i8080.simplify"
  | `Immediate_too_wide | `Out_of_range _ | `Org_misplaced -> "i8080.lower"
  | `Address_space _ | `Many_sections -> "i8080.image"
  | `No_padding -> "i8080.lower"
  | `Codec _ -> "i8080.encode"
  | `Decode_short | `Decode_no_match | `Decode_no_normalized -> "i8080.decode"
  | `Fixup_out_of_range _ | `No_data_relocation _ -> "i8080.fixup"

let pp_error ppf e = pp_error_kind ppf (Target_error.kind e)
let error_code e = error_kind_code (Target_error.kind e)
let error_diagnostic e = Target_error.to_diagnostic ~code:error_kind_code ~pp:pp_error_kind e

let diag ?pos ?origin kind =
  Err.Error.make ?pos ~pp_error
    (Target_error.make
       ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:"i8080" ())
       kind)

(* {2 Staged ASTs} *)

module Surface = struct
  type t = { mnemonic : string; ops : Operand.t list; origin : Origin.t }

  let pp ppf s =
    Fmt.pf ppf "%s%a" s.mnemonic
      (fun ppf -> function
        | [] -> () | ops -> Fmt.pf ppf " %a" Fmt.(list ~sep:(any ", ") Operand.pp) ops)
      s.ops
end

module Instruction = struct
  type t =
    | Insn of { entry : Ops.entry; args : Operand.t list }
    | Data of { width : int; items : Operand.t list }
    | Org of int

  let hex ~digits v = Printf.sprintf "0x%0*x" digits v

  let pp_imm ~digits ppf v =
    match Bigint.to_int_opt v with
    | Some n when n >= 0 -> Fmt.string ppf (hex ~digits n)
    | _ -> Bigint.pp ppf v

  let pp_arg operand ppf (arg : Operand.t) =
    match (arg, operand) with
    | Operand.Imm v, (Ops.D8 | Ops.P8) -> pp_imm ~digits:2 ppf v
    | Operand.Imm v, (Ops.D16 | Ops.A16) -> pp_imm ~digits:4 ppf v
    | _ -> Operand.pp ppf arg

  let pp ppf = function
    | Insn { entry; args } -> (
        Fmt.string ppf entry.Ops.mnemonic;
        match List.combine entry.Ops.operands args with
        | [] -> ()
        | pairs ->
            Fmt.pf ppf " %a" Fmt.(list ~sep:(any ", ") (fun ppf (o, a) -> pp_arg o ppf a)) pairs)
    | Data { width; items } ->
        let digits = 2 * width in
        Fmt.pf ppf "%s %a"
          (if width = 1 then "db" else "dw")
          Fmt.(
            list ~sep:(any ", ") (fun ppf (a : Operand.t) ->
                match a with Operand.Imm v -> pp_imm ~digits ppf v | a -> Operand.pp ppf a))
          items
    | Org n -> Fmt.pf ppf "org %s" (hex ~digits:4 n)
end

type tail_value = Tconst of int64 | Tsym of Asm_core.Expr.t

module Lowered = struct
  type t =
    | Insn of { op : Ops.entry; a : int; b : int; tail : tail_value }
    | Alias of int * tail_value
        (** an undocumented byte, with the operand bytes its documented twin takes *)
    | Data of { width : int; value : tail_value }

  let pp_tail ppf = function
    | Tconst v -> Fmt.pf ppf "%Ld" v
    | Tsym e -> Fmt.string ppf (Asm_core.Expr.to_string e)

  let pp ppf = function
    | Insn { op; a; b; tail } -> Fmt.pf ppf "%s %d %d %a" op.Ops.mnemonic a b pp_tail tail
    | Alias (b, tail) -> Fmt.pf ppf "alias %02x %a" b pp_tail tail
    | Data { width; value } -> Fmt.pf ppf "data%d %a" width pp_tail value

  let equal_tail a b =
    match (a, b) with
    | Tconst x, Tconst y -> Int64.equal x y
    | Tsym x, Tsym y -> String.equal (Asm_core.Expr.to_string x) (Asm_core.Expr.to_string y)
    | _ -> false

  let equal x y =
    match (x, y) with
    | Insn a, Insn b ->
        String.equal a.op.Ops.mnemonic b.op.Ops.mnemonic
        && a.a = b.a && a.b = b.b && equal_tail a.tail b.tail
    | Alias (a, x), Alias (b, y) -> a = b && equal_tail x y
    | Data a, Data b -> a.width = b.width && equal_tail a.value b.value
    | _ -> false
end

(* {1 Fixups, state, configuration} *)

type fixup_kind = Abs8 | Abs16

let fixup_kind_name = function Abs8 -> "abs8" | Abs16 -> "abs16"
let equal_fixup_kind a b = a = b
let fixup_family = fixup_kind_name
let fixup_role (_ : fixup_kind) = L.Data_address

type target_state = unit

let default_state = ()
let components : Target_component.t list = []
let default_config = Target_config.default components
let initial_state (_ : Target_config.t) = default_state
let state_config (_ : target_state) = default_config
let required (_ : Instruction.t) : Target_component.Req.t option = None
let make_surface_instruction ~mnemonic ~origin ops = Ok { Surface.mnemonic; ops; origin }

(* {1 Simplify} *)

let expected_of = function
  | Ops.R8 -> "a register (b c d e h l m a)"
  | Ops.Rp -> "a register pair (b d h sp)"
  | Ops.Rp_psw -> "a register pair (b d h psw)"
  | Ops.Rp_bd -> "b or d"
  | Ops.Rst -> "0..7"
  | Ops.D8 -> "an 8-bit value"
  | Ops.P8 -> "a port number"
  | Ops.D16 -> "a 16-bit value"
  | Ops.A16 -> "an address"

(* The field value an operand takes for a field-valued operand kind. *)
let field_value kind (a : Operand.t) =
  let half r = Reg.to_int r / 2 in
  match (kind, a) with
  | Ops.R8, Operand.Reg r -> Some (Reg.to_int r)
  | Ops.Rp, Operand.Reg ((Reg.B | Reg.D | Reg.H) as r) -> Some (half r)
  | Ops.Rp, Operand.Sp -> Some 3
  | Ops.Rp_psw, Operand.Reg ((Reg.B | Reg.D | Reg.H) as r) -> Some (half r)
  | Ops.Rp_psw, Operand.Psw -> Some 3
  | Ops.Rp_bd, Operand.Reg ((Reg.B | Reg.D) as r) -> Some (half r)
  | Ops.Rst, Operand.Imm v -> (
      match Bigint.to_int_opt v with Some n when n >= 0 && n < 8 -> Some n | _ -> None)
  | _ -> None

let is_value = function Operand.Imm _ | Operand.Sym _ -> true | _ -> false

let operand_ok kind a =
  match kind with Ops.D8 | Ops.P8 | Ops.D16 | Ops.A16 -> is_value a | k -> field_value k a <> None

let simplify_instruction (_ : target_state) (s : Surface.t) =
  let bad kind = Error (diag ~pos:__POS__ ~origin:s.Surface.origin kind) in
  let m = String.lowercase_ascii s.Surface.mnemonic in
  match m with
  | "db" | "dw" -> (
      let width = if m = "db" then 1 else 2 in
      let ok = function
        | Operand.Imm _ | Operand.Sym _ -> true
        | Operand.Str _ -> width = 1
        | _ -> false
      in
      if s.Surface.ops = [] then bad (`No_form m)
      else
        match
          List.find_opt (fun (_, a) -> not (ok a)) (List.mapi (fun i a -> (i, a)) s.Surface.ops)
        with
        | Some (index, _) ->
            bad
              (`Bad_operand
                 {
                   mnemonic = m;
                   index;
                   expected = ("a value" ^ if width = 1 then " or string" else "");
                 })
        | None -> Ok (Instruction.Data { width; items = s.Surface.ops }))
  | "org" -> (
      match s.Surface.ops with
      | [ Operand.Imm v ] -> (
          match Bigint.to_int_opt v with
          | Some n when n >= 0 && n < 0x10000 -> Ok (Instruction.Org n)
          | _ -> bad (`Out_of_range { what = "origin"; value = v; low = 0; high = 0xffff }))
      | _ -> bad (`Bad_operand { mnemonic = m; index = 0; expected = "a constant address" }))
  | _ -> (
      match Opcode.of_mnemonic m with
      | None -> bad (`Unknown_instruction s.Surface.mnemonic)
      | Some entry -> (
          let kinds = entry.Ops.operands and args = s.Surface.ops in
          if List.length kinds <> List.length args then bad (`No_form m)
          else
            let rec check i = function
              | [], [] -> None
              | k :: ks, a :: rest ->
                  if operand_ok k a then check (i + 1) (ks, rest) else Some (i, expected_of k)
              | _ -> Some (0, "")
            in
            match check 0 (kinds, args) with
            | Some (index, expected) -> bad (`Bad_operand { mnemonic = m; index; expected })
            | None ->
                (* mov m,m is the slot of hlt *)
                if m = "mov" && args = [ Operand.Reg Reg.M; Operand.Reg Reg.M ] then
                  bad (`No_form m)
                else Ok (Instruction.Insn { entry; args })))

(* {1 Lower} *)

let int64_of_big v = Bigint.to_int64_opt v

(* 8-bit values are accepted as -128..255 and 16-bit as -32768..65535 (the
   assembler convention); the stored pattern is the low bits. *)
let value_of ~bytes ~what ~origin a =
  let low, high = if bytes = 1 then (-128, 255) else (-32768, 65535) in
  match a with
  | Operand.Sym e -> Ok (Tsym e)
  | Operand.Imm v -> (
      match int64_of_big v with
      | Some n
        when Int64.compare n (Int64.of_int low) >= 0 && Int64.compare n (Int64.of_int high) <= 0 ->
          let mask = if bytes = 1 then 0xffL else 0xffffL in
          Ok (Tconst (Int64.logand n mask))
      | _ -> Error (diag ~pos:__POS__ ~origin (`Out_of_range { what; value = v; low; high })))
  | _ ->
      Error (diag ~pos:__POS__ ~origin (`Out_of_range { what; value = Bigint.of_int 0; low; high }))

let lower_instruction (_ : target_state) (i : Instruction.t) =
  let origin = Origin.synthesized ~pass:"i8080.lower" () in
  match i with
  | Instruction.Org _ -> Ok []
  | Instruction.Data { width; items } ->
      let one acc (a : Operand.t) =
        match acc with
        | Error _ -> acc
        | Ok xs -> (
            match a with
            | Operand.Str s ->
                Ok
                  (List.rev_append
                     (List.init (String.length s) (fun k ->
                          Lowered.Data
                            { width = 1; value = Tconst (Int64.of_int (Char.code s.[k])) }))
                     xs)
            | a -> (
                match
                  value_of ~bytes:width
                    ~what:(if width = 1 then "db value" else "dw value")
                    ~origin a
                with
                | Ok value -> Ok (Lowered.Data { width; value } :: xs)
                | Error e -> Error e))
      in
      Result.map List.rev (List.fold_left one (Ok []) items)
  | Instruction.Insn { entry; args } -> (
      let fields = ref [] and tail = ref (Ok (Tconst 0L)) in
      List.iter2
        (fun kind a ->
          match kind with
          | Ops.D8 | Ops.P8 ->
              tail := value_of ~bytes:1 ~what:(entry.Ops.mnemonic ^ " operand") ~origin a
          | Ops.D16 | Ops.A16 ->
              tail := value_of ~bytes:2 ~what:(entry.Ops.mnemonic ^ " operand") ~origin a
          | k -> fields := Option.get (field_value k a) :: !fields)
        entry.Ops.operands args;
      match (!tail, List.rev !fields) with
      | Error e, _ -> Error e
      | Ok tail, [] -> Ok [ Lowered.Insn { op = entry; a = 0; b = 0; tail } ]
      | Ok tail, [ a ] -> Ok [ Lowered.Insn { op = entry; a; b = 0; tail } ]
      | Ok tail, [ a; b ] -> Ok [ Lowered.Insn { op = entry; a; b; tail } ]
      | Ok _, _ -> Error (diag ~pos:__POS__ ~origin (`No_form entry.Ops.mnemonic)))

(* {1 The codec}

   One alternative per table entry, built from the entry's field positions, then
   one per alias byte. A 16-bit value is a byte-swapped field, the same
   treatment as x86's little-endian displacements; the linker patches the
   container little-endian itself. *)

let k_none : (tail_value, fixup_kind) C.t =
  C.iso_fun ~name:"no-tail" ~encode:(fun _ -> Some ()) ~decode:(fun () -> Some (Tconst 0L)) C.empty

let placeholder = function Tconst v -> v | Tsym _ -> 0L

let swap16 v =
  Int64.logor
    (Int64.shift_left (Int64.logand v 0xffL) 8)
    (Int64.shift_right_logical (Int64.logand v 0xff00L) 8)

let tail8 : (tail_value, fixup_kind) C.t =
  C.iso_fun ~name:"imm8"
    ~encode:(fun t -> Some (placeholder t))
    ~decode:(fun v -> Some (Tconst v))
    (C.fixup ~width:8 ~kind:Abs8 "imm")

let tail16 : (tail_value, fixup_kind) C.t =
  C.iso_fun ~name:"imm16"
    ~encode:(fun t -> Some (swap16 (placeholder t)))
    ~decode:(fun v -> Some (Tconst (swap16 v)))
    (C.fixup ~width:16 ~kind:Abs16 "imm")

let tail_part (e : Ops.entry) =
  match Ops.tail_of e with Ops.Tail_none -> k_none | Ops.Tail_8 -> tail8 | Ops.Tail_16 -> tail16

let fld w name = C.field ~width:w name

(* The opcode byte of an entry: fixed bits around one or two register fields,
   carried as the pair [(a, b)] of field values. *)
let opcode_part (e : Ops.entry) : (int64 * int64, fixup_kind) C.t =
  let bits shift width = (e.Ops.base lsr shift) land ((1 lsl width) - 1) |> Int64.of_int in
  match e.Ops.fields with
  | [] ->
      C.iso_fun ~name:"fixed"
        ~encode:(fun (a, b) -> if a = 0L && b = 0L then Some () else None)
        ~decode:(fun () -> Some (0L, 0L))
        (C.const ~width:8 (Int64.of_int e.Ops.base))
  | [ (shift, width) ] -> (
      let hi_w = 8 - shift - width and lo_w = shift in
      let hi () = C.const ~width:hi_w (bits (shift + width) hi_w)
      and lo () = C.const ~width:lo_w (bits 0 lo_w)
      and f = fld width "r" in
      let put = fun (a, b) -> if b = 0L then Some a else None in
      match (hi_w > 0, lo_w > 0) with
      | true, true ->
          C.iso_fun ~name:"opcode"
            ~encode:(fun v -> Option.map (fun a -> ((), (a, ()))) (put v))
            ~decode:(fun ((), (a, ())) -> Some (a, 0L))
            C.(hi () ** f ** lo ())
      | true, false ->
          C.iso_fun ~name:"opcode"
            ~encode:(fun v -> Option.map (fun a -> ((), a)) (put v))
            ~decode:(fun ((), a) -> Some (a, 0L))
            C.(hi () ** f)
      | false, true ->
          C.iso_fun ~name:"opcode"
            ~encode:(fun v -> Option.map (fun a -> (a, ())) (put v))
            ~decode:(fun (a, ()) -> Some (a, 0L))
            C.(f ** lo ())
      | false, false -> invalid_arg "i8080: field fills the opcode")
  | _ -> invalid_arg "i8080: unsupported field layout"

let entry_alt priority (e : Ops.entry) =
  let mine = function
    | Lowered.Insn { op; _ } -> String.equal op.Ops.mnemonic e.Ops.mnemonic
    | _ -> false
  in
  let body =
    C.iso_fun ~name:e.Ops.mnemonic
      ~encode:(function
        | Lowered.Insn { a; b; tail; _ } as l when mine l ->
            Some ((Int64.of_int a, Int64.of_int b), tail)
        | _ -> None)
      ~decode:(fun ((a, b), tail) ->
        Some (Lowered.Insn { op = e; a = Int64.to_int a; b = Int64.to_int b; tail }))
      C.(opcode_part e ** tail_part e)
  in
  C.alt ~label:e.Ops.mnemonic ~priority ~guard:mine body

let alias_tail target =
  match Ops.form_of_byte target with
  | Some (Ops.Documented (e, _)) -> tail_part e
  | _ -> invalid_arg "i8080: alias of an undocumented byte"

let alias_alt priority (byte, target) =
  C.alt
    ~label:(Printf.sprintf "alias.%02x" byte)
    ~priority
    ~guard:(function Lowered.Alias (b, _) -> b = byte | _ -> false)
    (C.iso_fun ~name:"alias"
       ~encode:(function Lowered.Alias (b, t) when b = byte -> Some ((), t) | _ -> None)
       ~decode:(fun ((), t) -> Some (Lowered.Alias (byte, t)))
       C.(C.const ~width:8 (Int64.of_int byte) ** alias_tail target))

(* mov is [01 ddd sss] with [01 110 110] taken by hlt. Rather than let two
   forms overlap and priority decide, it is fourteen disjoint forms: a constant
   destination with a free source (not M), and destination M with a constant
   source (not M). *)
let mov_alts first_priority (e : Ops.entry) =
  let is_mov = function
    | Lowered.Insn { op; _ } -> String.equal op.Ops.mnemonic e.Ops.mnemonic
    | _ -> false
  in
  let alt_of i ~guard body =
    C.alt ~label:e.Ops.mnemonic ~priority:(first_priority + i) ~guard body
  in
  let free_src d =
    alt_of d
      ~guard:(function Lowered.Insn { a; _ } as l -> is_mov l && a = d | _ -> false)
      (C.iso_fun ~name:"mov"
         ~encode:(function
           | Lowered.Insn { a; b; _ } when a = d -> Some ((), ((), Int64.of_int b)) | _ -> None)
         ~decode:(fun ((), ((), b)) ->
           Some (Lowered.Insn { op = e; a = d; b = Int64.to_int b; tail = Tconst 0L }))
         C.(C.const ~width:2 1L ** C.const ~width:3 (Int64.of_int d) ** fld 3 "src"))
  in
  let free_src_dsts = List.filter (( <> ) 6) (List.init 8 Fun.id) in
  let m_dst s =
    alt_of (10 + s)
      ~guard:(function Lowered.Insn { a; b; _ } as l -> is_mov l && a = 6 && b = s | _ -> false)
      (C.iso_fun ~name:"mov"
         ~encode:(function Lowered.Insn { a = 6; b; _ } when b = s -> Some () | _ -> None)
         ~decode:(fun () -> Some (Lowered.Insn { op = e; a = 6; b = s; tail = Tconst 0L }))
         (C.const ~width:8 (Int64.of_int (0x70 lor s))))
  in
  List.map free_src free_src_dsts @ List.map m_dst (List.filter (( <> ) 6) (List.init 8 Fun.id))

let codec : (Lowered.t, fixup_kind) C.t =
  let alts, next =
    List.fold_left
      (fun (acc, p) (e : Ops.entry) ->
        if e.Ops.mnemonic = "mov" then (acc @ mov_alts p e, p + 20)
        else (acc @ [ entry_alt p e ], p + 1))
      ([], 0) Ops.entries
  in
  C.choice ~name:"i8080" (alts @ List.mapi (fun i a -> alias_alt (next + i) a) Ops.aliases)

(* {1 Encode} *)

let range_of = function Abs8 -> L.Bitpattern 8 | Abs16 -> L.Bitpattern 16

let fixup_of ~kind ~name ~byte_offset ~container ~slices ~value =
  {
    L.kind;
    kind_name = fixup_kind_name kind;
    family = fixup_family kind;
    role = fixup_role kind;
    name;
    slices;
    byte_offset;
    container;
    pc_bias = 0;
    range = range_of kind;
    value;
    pairing = L.Unpaired;
    origin = Origin.synthesized ~pass:"i8080.encode" ();
  }

let expr_of_lowered = function Lowered.Insn { tail = Tsym e; _ } -> [ ("imm", e) ] | _ -> []

let form_of l (enc : fixup_kind C.encoded) =
  let exprs = expr_of_lowered l in
  {
    L.bytes = C.Bits.to_bytes enc.C.bits;
    form = C.form_id enc;
    fixups =
      List.filter_map
        (fun (p : fixup_kind C.placement) ->
          match List.assoc_opt p.C.name exprs with
          | None -> None
          | Some value ->
              let first =
                List.fold_left (fun m (s : C.slice) -> min m s.C.bit_offset) max_int p.C.slices
              in
              let width = List.fold_left (fun a (s : C.slice) -> a + s.C.bit_width) 0 p.C.slices in
              Some
                (fixup_of ~kind:p.C.kind ~name:p.C.name ~byte_offset:(first / 8)
                   ~container:((width + 7) / 8)
                   ~slices:
                     (List.map
                        (fun (s : C.slice) ->
                          {
                            L.bit_offset = s.C.bit_offset - first;
                            bit_width = s.C.bit_width;
                            value_lsb = s.C.value_lsb;
                          })
                        p.C.slices)
                   ~value))
        enc.C.placements;
  }

let le_bytes n v =
  String.init n (fun i ->
      Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xffL)))

let encode = function
  | Lowered.Data { width; value } ->
      let kind = if width = 1 then Abs8 else Abs16 in
      let fixups =
        match value with
        | Tconst _ -> []
        | Tsym e ->
            [
              fixup_of ~kind ~name:"data" ~byte_offset:0 ~container:width
                ~slices:[ { L.bit_offset = 0; bit_width = 8 * width; value_lsb = 0 } ]
                ~value:e;
            ]
      in
      Ok
        (`Fixed
           {
             L.bytes = le_bytes width (placeholder value);
             form = (if width = 1 then "db" else "dw");
             fixups;
           })
  | l -> (
      match C.encode codec l with
      | Error (e : C.error) -> Error (diag ~pos:__POS__ (`Codec e))
      | Ok enc -> Ok (`Fixed (form_of l enc)))

let encode_in (_ : target_state) l = encode l

(* {1 Decode} *)

type decode_context = { state : target_state; address : int64 }

let instruction_of_lowered : Lowered.t -> Instruction.t option = function
  | Lowered.Alias (b, tail) ->
      let has_operand =
        match List.assoc_opt b Ops.aliases with
        | Some t -> (
            match Ops.form_of_byte t with
            | Some (Ops.Documented (e, _)) -> Ops.tail_of e = Ops.Tail_16
            | _ -> false)
        | None -> false
      in
      let imm n = Operand.Imm (Bigint.of_int n) in
      let operand =
        match tail with
        | Tconst v when has_operand ->
            let v = Int64.to_int v in
            [ imm (v land 0xff); imm ((v lsr 8) land 0xff) ]
        | _ -> []
      in
      Some (Instruction.Data { width = 1; items = imm b :: operand })
  | Lowered.Data _ -> None
  | Lowered.Insn { op; a; b; tail } ->
      let values =
        ref (match op.Ops.fields with [ _; _ ] -> [ a; b ] | [ _ ] -> [ a ] | _ -> [])
      in
      let arg kind =
        match kind with
        | Ops.D8 | Ops.P8 | Ops.D16 | Ops.A16 -> (
            match tail with
            | Tconst v -> Some (Operand.Imm (Bigint.of_int64 v))
            | Tsym e -> Some (Operand.Sym e))
        | k -> (
            match !values with
            | [] -> None
            | v :: rest -> (
                values := rest;
                match k with
                | Ops.R8 -> Option.map (fun r -> Operand.Reg r) (Reg.of_int v)
                | Ops.Rp | Ops.Rp_psw ->
                    if v = 3 then Some (if k = Ops.Rp then Operand.Sp else Operand.Psw)
                    else Option.map (fun r -> Operand.Reg r) (Reg.of_int (2 * v))
                | Ops.Rp_bd -> Option.map (fun r -> Operand.Reg r) (Reg.of_int (2 * v))
                | Ops.Rst -> Some (Operand.Imm (Bigint.of_int v))
                | _ -> None))
      in
      let args = List.map arg op.Ops.operands in
      if List.exists Option.is_none args then None
      else Some (Instruction.Insn { entry = op; args = List.map Option.get args })

let decode (_ : decode_context) bytes ~pos =
  let remaining = String.length bytes - pos in
  if remaining <= 0 then Error (diag ~pos:__POS__ `Decode_short)
  else
    let window = String.sub bytes pos (min 3 remaining) in
    match C.decode_bits codec (C.Bits.of_bytes window) with
    | None -> Error (diag ~pos:__POS__ (if remaining < 3 then `Decode_short else `Decode_no_match))
    | Some d -> (
        match instruction_of_lowered d.C.value with
        | None -> Error (diag ~pos:__POS__ `Decode_no_normalized)
        | Some i -> Ok (i, String.concat "." d.C.dform, d.C.consumed / 8))

let decode_candidates ctx bytes ~pos =
  match decode ctx bytes ~pos with Ok r -> [ r ] | Error _ -> []

(* {1 Fixups, padding, data} *)

let evaluate_fixup kind ~place:_ ~target =
  match kind with
  | Abs8 ->
      if Int64.compare target (-128L) >= 0 && Int64.compare target 255L <= 0 then
        Ok (Int64.logand target 0xffL)
      else Error (diag ~pos:__POS__ (`Fixup_out_of_range (target, 8)))
  | Abs16 ->
      if Int64.compare target (-32768L) >= 0 && Int64.compare target 65535L <= 0 then
        Ok (Int64.logand target 0xffffL)
      else Error (diag ~pos:__POS__ (`Fixup_out_of_range (target, 16)))

let data_widths = [ (".byte", 1); (".word", 2); (".2byte", 2) ]

let data_fixup ~width =
  match width with
  | 1 -> Ok Abs8
  | 2 -> Ok Abs16
  | w -> Error (diag ~pos:__POS__ (`No_data_relocation w))

let nop_bytes ~length:_ = Error (diag ~pos:__POS__ `No_padding)
let merge_fill = None
let pad_section_to_alignment = false
