(* Shared x86 encoding machinery: registers, addressing, REX, ModR/M, SIB,
   displacements and immediates (.ai/asm_plan.md §5.1).

   The only family package. Everything here is built from the generic
   Seq/Iso/Alt combinators, and the x86 vocabulary lives here rather than in
   lib/codec - which is what keeps the codec EDSL target-agnostic and what
   tools/asm-check-layers.sh enforces.

   The family parameter [MODE] exposes real differences, per §5.1: register
   availability, default operand size, and whether a REX byte may exist at all.
   It is not a name: x86_32 and x86_64 differ in what they can encode, and a
   mode that only supplied a string would let a 32-bit assembly acquire a REX
   prefix from a shared code path.

   Byte order. The codec lays bits down most-significant first, so an x86 form
   is authored as a sequence of 8-bit chunks in memory order and the packing is
   memory order. Multi-byte immediates and displacements are little-endian,
   which is a real part of the encoding rather than an output-stage detail, so
   it appears in the tree as an [Iso_fun] ({!le}) - [inspect] then shows it and
   [decode] inverts it for free. *)

open Foundation
module C = Codec

(* {1 Registers}

   [rnum] is the architectural number 0-15, so [rnum land 7] is the ModR/M or
   SIB field and [rnum >= 8] is the REX extension bit. Keeping one number
   rather than a pair is what makes "did this operand need REX.B" a property of
   the register instead of a fact the caller has to remember to carry. *)

type reg = { rname : string; rnum : int; rwidth : int }

let reg_equal a b = a.rnum = b.rnum && a.rwidth = b.rwidth
let pp_reg ppf r = Fmt.pf ppf "%%%s" r.rname
let names_32 = [| "eax"; "ecx"; "edx"; "ebx"; "esp"; "ebp"; "esi"; "edi" |]
let names_64 = [| "rax"; "rcx"; "rdx"; "rbx"; "rsp"; "rbp"; "rsi"; "rdi" |]
let names_16 = [| "ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di" |]
let names_8l = [| "al"; "cl"; "dl"; "bl"; "spl"; "bpl"; "sil"; "dil" |]

let base_regs width names =
  Array.to_list (Array.mapi (fun i n -> { rname = n; rnum = i; rwidth = width }) names)

let extended_regs width suffix =
  List.init 8 (fun i ->
      { rname = Printf.sprintf "r%d%s" (i + 8) suffix; rnum = i + 8; rwidth = width })

(* {1 Memory operands}

   [scale] is the multiplier (1, 2, 4 or 8), not its logarithm, because that is
   what the surface syntax writes and converting once at the encoder is one
   place to be wrong instead of every place that builds a [mem]. *)

type mem = { base : reg option; index : reg option; scale : int; disp : int64 }

let mem_equal a b =
  Option.equal reg_equal a.base b.base
  && Option.equal reg_equal a.index b.index
  && a.scale = b.scale && Int64.equal a.disp b.disp

let pp_mem ppf m =
  let disp = if Int64.equal m.disp 0L then "" else Printf.sprintf "%Ld" m.disp in
  match (m.base, m.index) with
  | None, None -> Fmt.pf ppf "%Ld" m.disp
  | Some b, None -> Fmt.pf ppf "%s(%a)" disp pp_reg b
  | Some b, Some i -> Fmt.pf ppf "%s(%a,%a,%d)" disp pp_reg b pp_reg i m.scale
  | None, Some i -> Fmt.pf ppf "%s(,%a,%d)" disp pp_reg i m.scale

type operand = Op_reg of reg | Op_mem of mem | Op_imm of Bigint.t

let pp_operand ppf = function
  | Op_reg r -> pp_reg ppf r
  | Op_mem m -> pp_mem ppf m
  | Op_imm v -> Fmt.pf ppf "$%a" Bigint.pp v

(* {1 The three staged instruction types} *)

type surface_instruction = { s_mnemonic : string; s_ops : operand list; s_origin : Origin.t }

(* AT&T operand order throughout: source first, destination last. Canonicalizing
   to Intel order here would make every diagnostic disagree with the source the
   user wrote. *)
let pp_surface ppf s =
  match s.s_ops with
  | [] -> Fmt.string ppf s.s_mnemonic
  | ops -> Fmt.pf ppf "%s %a" s.s_mnemonic Fmt.(list ~sep:(any ", ") pp_operand) ops

type opcode = Op_add | Op_sub | Op_mov | Op_lea | Op_ret

let opcode_name = function
  | Op_add -> "add"
  | Op_sub -> "sub"
  | Op_mov -> "mov"
  | Op_lea -> "lea"
  | Op_ret -> "ret"

(* The normalized instruction carries an explicit operand *width in bits*: the
   suffix that spelled it (`l`, `q`) is surface syntax, and by this stage the
   only thing that matters is 32 or 64. *)
type instruction = { i_op : opcode; i_width : int; i_ops : operand list }

let suffix_of_width = function 8 -> "b" | 16 -> "w" | 32 -> "l" | 64 -> "q" | _ -> "?"

let pp_instruction ppf i =
  match i.i_ops with
  | [] -> Fmt.string ppf (opcode_name i.i_op)
  | ops ->
      Fmt.pf ppf "%s%s %a" (opcode_name i.i_op) (suffix_of_width i.i_width)
        Fmt.(list ~sep:(any ", ") pp_operand)
        ops

type rm = Rm_reg of reg | Rm_mem of mem

let rm_equal a b =
  match (a, b) with
  | Rm_reg x, Rm_reg y -> reg_equal x y
  | Rm_mem x, Rm_mem y -> mem_equal x y
  | _ -> false

let pp_rm ppf = function Rm_reg r -> pp_reg ppf r | Rm_mem m -> pp_mem ppf m

(* The lowered instruction names an *encoding shape*, not a mnemonic: [L_alu_rm_imm]
   with [ext = 5] is `sub` and with [ext = 0] is `add`, because the machine
   encodes both as opcode 0x83 with the operation in the ModR/M reg field. A
   lowered type that still said "sub" would have to say it twice - once as the
   constructor and once as the extension - and the two could disagree. *)
type lowered_instruction =
  | L_alu_rm_imm of { ext : int; width : int; rm : rm; imm : int64 }
  | L_mov_r_imm of { width : int; reg : reg; imm : int64 }
  | L_mov_rm_r of { width : int; rm : rm; reg : reg }
  | L_mov_r_rm of { width : int; reg : reg; rm : rm }
  | L_lea of { width : int; reg : reg; mem : mem }
  | L_ret

let alu_ext = function Op_add -> 0 | Op_sub -> 5 | _ -> -1
let alu_of_ext = function 0 -> Some Op_add | 5 -> Some Op_sub | _ -> None

let pp_lowered ppf = function
  | L_alu_rm_imm { ext; width; rm; imm } ->
      Fmt.pf ppf "%s%s $%Ld, %a"
        (match alu_of_ext ext with Some o -> opcode_name o | None -> "alu?")
        (suffix_of_width width) imm pp_rm rm
  | L_mov_r_imm { width; reg; imm } ->
      Fmt.pf ppf "mov%s $%Ld, %a" (suffix_of_width width) imm pp_reg reg
  | L_mov_rm_r { width; rm; reg } ->
      Fmt.pf ppf "mov%s %a, %a" (suffix_of_width width) pp_reg reg pp_rm rm
  | L_mov_r_rm { width; reg; rm } ->
      Fmt.pf ppf "mov%s %a, %a" (suffix_of_width width) pp_rm rm pp_reg reg
  | L_lea { width; reg; mem } ->
      Fmt.pf ppf "lea%s %a, %a" (suffix_of_width width) pp_mem mem pp_reg reg
  | L_ret -> Fmt.string ppf "ret"

let lowered_equal a b =
  match (a, b) with
  | L_alu_rm_imm x, L_alu_rm_imm y ->
      x.ext = y.ext && x.width = y.width && rm_equal x.rm y.rm && Int64.equal x.imm y.imm
  | L_mov_r_imm x, L_mov_r_imm y ->
      x.width = y.width && reg_equal x.reg y.reg && Int64.equal x.imm y.imm
  | L_mov_rm_r x, L_mov_rm_r y -> x.width = y.width && rm_equal x.rm y.rm && reg_equal x.reg y.reg
  | L_mov_r_rm x, L_mov_r_rm y -> x.width = y.width && reg_equal x.reg y.reg && rm_equal x.rm y.rm
  | L_lea x, L_lea y -> x.width = y.width && reg_equal x.reg y.reg && mem_equal x.mem y.mem
  | L_ret, L_ret -> true
  | _ -> false

(* {1 Fixups}

   Declared, and emitted by nothing in M1: every fixture's .text is
   relocation-free once the .cfi_* directives (and therefore .eh_frame) are
   discarded. The type exists so that M2 adds cases rather than replacing a
   string. *)
type fixup_kind = Abs32 | Pcrel32

let fixup_kind_name = function Abs32 -> "abs32" | Pcrel32 -> "pcrel32"

(* {1 Little-endian fields} *)

let mask ~width v =
  if width >= 64 then v else Int64.logand v (Int64.sub (Int64.shift_left 1L width) 1L)

let sign_extend ~width v =
  if width >= 64 then v
  else
    let s = 64 - width in
    Int64.shift_right (Int64.shift_left v s) s

(* Reversing the bytes of a [width]-bit value. An involution at each width, so
   the same function serves both directions of the isomorphism. *)
let bswap ~width v =
  let n = width / 8 in
  let v = mask ~width v in
  let r = ref 0L in
  for i = 0 to n - 1 do
    r :=
      Int64.logor (Int64.shift_left !r 8) (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xFFL)
  done;
  !r

let le ~signedness ~width name =
  if width = 8 then C.field ~signedness ~width name
  else
    C.iso_fun ~name:(Printf.sprintf "le%d" width)
      ~encode:(fun v -> Some (bswap ~width v))
      ~decode:(fun v ->
        let raw = bswap ~width v in
        Some (match signedness with C.Signed -> sign_extend ~width raw | C.Unsigned -> raw))
      (C.field ~width name)

(* The absent displacement, typed as a displacement rather than as [unit], so
   that all six memory alternatives have the same tuple shape and are built by
   the same two functions. Reading it as "the displacement is not stored, which
   means zero" is exactly what mod=00 says. *)
let no_disp : (int64, fixup_kind) C.t =
  C.iso_fun ~name:"no-disp"
    ~encode:(fun v -> if Int64.equal v 0L then Some () else None)
    ~decode:(fun () -> Some 0L)
    C.empty

let fits_s8 v = Int64.compare v (-128L) >= 0 && Int64.compare v 127L <= 0
let fits_s32 v = Int64.compare v (-2147483648L) >= 0 && Int64.compare v 2147483647L <= 0

(* {1 ModR/M, SIB and displacement}

   One codec over [rm_enc], covering the whole group of bytes that the ModR/M
   byte governs. Splitting it - a ModR/M codec, then an optional SIB codec, then
   an optional displacement codec - is not possible without a shared decision,
   because [mod] and [rm] jointly decide whether the other two bytes exist. *)

type rm_enc = { re_reg : int; re_rm : rm }
(** [re_reg] is the ModR/M reg field, which for the group-1 opcodes is the
    *operation* rather than a register - the /digit in Intel's tables. *)

type sib = { sc : int; ix : int; bs : int }

let sib_codec : (sib, fixup_kind) C.t =
  C.iso_fun ~name:"sib"
    ~encode:(fun s -> Some (Int64.of_int s.sc, (Int64.of_int s.ix, Int64.of_int s.bs)))
    ~decode:(fun (sc, (ix, bs)) ->
      Some { sc = Int64.to_int sc; ix = Int64.to_int ix; bs = Int64.to_int bs })
    C.(field ~width:2 "scale" ** field ~width:3 "index" ** field ~width:3 "base")

let log2_scale = function 1 -> Some 0 | 2 -> Some 1 | 4 -> Some 2 | 8 -> Some 3 | _ -> None
let scale_of_log2 = function 0 -> 1 | 1 -> 2 | 2 -> 4 | 3 -> 8 | _ -> 1

(* SIB is required when there is an index, and also when the base's low three
   bits are 100 - the value that in the rm field *means* "a SIB byte follows",
   so rsp and r12 have no non-SIB encoding. *)
let needs_sib m = m.index <> None || match m.base with Some b -> b.rnum land 7 = 4 | None -> true

(* Which displacement form GAS picks, and the one rule that is not "the smallest
   that fits": a base whose low three bits are 101 (rbp, r13) cannot use mod=00,
   because that encoding means "no base, disp32 follows". Such an operand gets
   an explicit zero disp8. *)
type disp_form = D_none | D_8 | D_32

let disp_form_of m =
  let ebp_like = match m.base with Some b -> b.rnum land 7 = 5 | None -> false in
  if Int64.equal m.disp 0L && not ebp_like then D_none else if fits_s8 m.disp then D_8 else D_32

let sib_of m =
  match log2_scale (if m.index = None then 1 else m.scale) with
  | None -> None
  | Some sc ->
      Some
        {
          sc;
          ix = (match m.index with Some i -> i.rnum land 7 | None -> 4);
          bs = (match m.base with Some b -> b.rnum land 7 | None -> 5);
        }

(* Decoding a memory operand needs the register table back, so the two
   directions are parameterized over it. [reg_of_num] is the mode's 32- or
   64-bit address register set. *)
let make_rm_codec ~(reg_of_num : int -> reg) : (rm_enc, fixup_kind) C.t =
  let mem_of_sib ~sib ~disp =
    {
      base = Some (reg_of_num sib.bs);
      index = (if sib.ix = 4 then None else Some (reg_of_num sib.ix));
      scale = scale_of_log2 sib.sc;
      disp;
    }
  in
  let sib_alt ~label ~priority ~modbits ~form ~disp_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:("modrm-" ^ label)
         ~encode:(fun e ->
           match e.re_rm with
           | Rm_mem m when needs_sib m && disp_form_of m = form -> (
               match sib_of m with
               | None -> None
               | Some s -> Some ((), (Int64.of_int (e.re_reg land 7), ((), (s, m.disp)))))
           | _ -> None)
         ~decode:(fun ((), (reg, ((), (s, disp)))) ->
           Some { re_reg = Int64.to_int reg; re_rm = Rm_mem (mem_of_sib ~sib:s ~disp) })
         C.(
           const ~width:2 (Int64.of_int modbits)
           ** field ~width:3 "reg" ** const ~width:3 4L ** sib_codec ** disp_codec))
  in
  let base_alt ~label ~priority ~modbits ~form ~disp_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:("modrm-" ^ label)
         ~encode:(fun e ->
           match e.re_rm with
           | Rm_mem m when (not (needs_sib m)) && disp_form_of m = form -> (
               match m.base with
               | None -> None
               | Some b ->
                   Some
                     ((), (Int64.of_int (e.re_reg land 7), (Int64.of_int (b.rnum land 7), m.disp))))
           | _ -> None)
         ~decode:(fun ((), (reg, (rm, disp))) ->
           Some
             {
               re_reg = Int64.to_int reg;
               re_rm =
                 Rm_mem
                   { base = Some (reg_of_num (Int64.to_int rm)); index = None; scale = 1; disp };
             })
         C.(
           const ~width:2 (Int64.of_int modbits)
           ** field ~width:3 "reg" ** field ~width:3 "rm" ** disp_codec))
  in
  C.choice ~name:"modrm"
    [
      (* Priority is decode order, and it has to put the SIB forms first: their
         rm field is the constant 100 while the non-SIB forms have a free rm
         field that can also hold 100, so the two overlap and only order
         separates them. [check] reports the overlap, correctly - it is real,
         and priority is what resolves it. *)
      C.alt ~label:"reg" ~priority:0
        (C.iso_fun ~name:"modrm-reg"
           ~encode:(fun e ->
             match e.re_rm with
             | Rm_reg r -> Some ((), (Int64.of_int (e.re_reg land 7), Int64.of_int (r.rnum land 7)))
             | Rm_mem _ -> None)
           ~decode:(fun ((), (reg, rm)) ->
             Some { re_reg = Int64.to_int reg; re_rm = Rm_reg (reg_of_num (Int64.to_int rm)) })
           C.(const ~width:2 3L ** field ~width:3 "reg" ** field ~width:3 "rm"));
      sib_alt ~label:"sib-disp0" ~priority:1 ~modbits:0 ~form:D_none ~disp_codec:no_disp;
      sib_alt ~label:"sib-disp8" ~priority:2 ~modbits:1 ~form:D_8
        ~disp_codec:(le ~signedness:C.Signed ~width:8 "disp8");
      sib_alt ~label:"sib-disp32" ~priority:3 ~modbits:2 ~form:D_32
        ~disp_codec:(le ~signedness:C.Signed ~width:32 "disp32");
      base_alt ~label:"base-disp0" ~priority:4 ~modbits:0 ~form:D_none ~disp_codec:no_disp;
      base_alt ~label:"base-disp8" ~priority:5 ~modbits:1 ~form:D_8
        ~disp_codec:(le ~signedness:C.Signed ~width:8 "disp8");
      base_alt ~label:"base-disp32" ~priority:6 ~modbits:2 ~form:D_32
        ~disp_codec:(le ~signedness:C.Signed ~width:32 "disp32");
    ]

(* An [Empty] displacement makes the disp0 alternatives' tuple shape
   [(unit * ...) ] rather than a special case, so all six memory alternatives
   are built by the same two functions above. *)

(* {1 The mode parameter} *)

module type MODE = sig
  val name : string
  val triple : string

  val address_width : int
  (** the width of a register usable as a base or index: 32 or 64. Not the same as the default
      operand size, which is 32 in both modes - [movl $42, %eax] is the identical encoding on
      x86-64. *)

  val rex_allowed : bool
  (** whether a REX byte may exist at all. In 32-bit mode the encodings 0x40-0x4f are inc/dec, so a
      REX-shaped alternative in the codec would not merely be unused - it would decode those
      instructions as a prefix on whatever followed. *)

  val registers : reg list
end

module Make (M : MODE) = struct
  let name = M.name
  let triple = M.triple

  type register = reg
  type nonrec reg = reg
  type nonrec mem = mem
  type surface_operand = operand
  type nonrec surface_instruction = surface_instruction
  type nonrec opcode = opcode
  type nonrec instruction = instruction
  type nonrec lowered_instruction = lowered_instruction
  type nonrec fixup_kind = fixup_kind
  type feature = No_features
  type target_state = { unused : unit }

  let default_state = { unused = () }
  let default_features = []
  let pp_surface = pp_surface
  let pp_instruction = pp_instruction
  let pp_lowered = pp_lowered
  let lowered_equal = lowered_equal

  (* {2 Lexical profile}

     x86's `#` is a comment introducer and its immediates carry `$`; registers
     carry `%`. That last one is why [.section ...,%progbits] lexes as a
     register token whose name is not a register - the sigil is lexical, and
     what the name means is decided here. *)
  let lexical_profile =
    Asm_core.Lexical_profile.make ~name:M.name ~comment_introducers:[ "#" ] ~statement_separator:';'
      ~immediate_sigil:'$' ~register_sigil:'%' ()

  let address_regs = List.filter (fun r -> r.rwidth = M.address_width) M.registers

  let reg_at ~width n =
    match List.find_opt (fun r -> r.rwidth = width && r.rnum = n) M.registers with
    | Some r -> r
    | None -> { rname = Printf.sprintf "r%d?" n; rnum = n; rwidth = width }

  (* The register a ModR/M or SIB field denotes when it is being used to form an
     *address*. Always the mode's address width, whatever the operand size is:
     the base register in [movl %eax, 0(%rsp)] is 64-bit on x86-64 even though
     the operand moved is 32-bit. *)
  let reg_of_num n = reg_at ~width:M.address_width (n land 7)

  (* And the register the same field denotes when it is an *operand*. Decoding
     cannot know which until the form says so, which is why this is applied by
     each form's decode rather than inside [rm_codec]: with no REX the mov below
     writes [%eax], and printing [%rax] there would be a disassembly that does
     not re-assemble to the bytes it came from. *)
  let retype ~width (r : reg) = reg_at ~width r.rnum
  let retype_rm ~width = function Rm_reg r -> Rm_reg (retype ~width r) | Rm_mem _ as m -> m
  let find_reg n = List.find_opt (fun r -> String.equal r.rname n) M.registers

  (* {2 Operand parsing (§4.7)}

     A hand-written token cursor, because AT&T addressing is not a grammar the
     common parser can own: `16(%esp)` and `(1+2)*3` start identically and only
     the target knows that the first is a memory operand. *)

  let diag ?origin code message =
    Diagnostic.error ~code ~message
      ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:M.name ())
      ()

  let slice_origin (s : Asm_core.Token.slice) =
    match Asm_core.Token.slice_span s with
    | Some sp -> Origin.text sp
    | None -> Origin.synthesized ~pass:M.name ()

  let parse_one_operand (slice : Asm_core.Token.slice) =
    let origin = slice_origin slice in
    let bad msg = Error (diag ~origin "x86.operand" msg) in
    let reg_named n =
      match find_reg n with
      | Some r -> Ok r
      | None -> bad (Printf.sprintf "unknown register %%%s" n)
    in
    let open Asm_core in
    match List.map Token.kind slice with
    (* $imm *)
    | [ Token.Immediate_sigil; Token.Int v ] -> Ok (Op_imm v)
    | Token.Immediate_sigil :: Token.Minus :: [ Token.Int v ] -> Ok (Op_imm (Bigint.neg v))
    (* %reg *)
    | [ Token.Register n ] -> Result.map (fun r -> Op_reg r) (reg_named n)
    (* disp(%base) and (%base) *)
    | [ Token.Lparen; Token.Register b; Token.Rparen ] ->
        Result.map
          (fun b -> Op_mem { base = Some b; index = None; scale = 1; disp = 0L })
          (reg_named b)
    | [ Token.Int d; Token.Lparen; Token.Register b; Token.Rparen ] -> (
        match (Bigint.to_int64_opt d, reg_named b) with
        | Some d, Ok b -> Ok (Op_mem { base = Some b; index = None; scale = 1; disp = d })
        | None, _ -> bad "displacement does not fit 64 bits"
        | _, Error e -> Error e)
    | Token.Minus :: Token.Int d :: [ Token.Lparen; Token.Register b; Token.Rparen ] -> (
        match (Bigint.to_int64_opt d, reg_named b) with
        | Some d, Ok b -> Ok (Op_mem { base = Some b; index = None; scale = 1; disp = Int64.neg d })
        | None, _ -> bad "displacement does not fit 64 bits"
        | _, Error e -> Error e)
    (* disp(%base,%index,scale) *)
    | [
     Token.Int d;
     Token.Lparen;
     Token.Register b;
     Token.Comma;
     Token.Register i;
     Token.Comma;
     Token.Int s;
     Token.Rparen;
    ] -> (
        match (Bigint.to_int64_opt d, Bigint.to_int_opt s, reg_named b, reg_named i) with
        | Some d, Some s, Ok b, Ok i ->
            if log2_scale s = None then bad "scale must be 1, 2, 4 or 8"
            else Ok (Op_mem { base = Some b; index = Some i; scale = s; disp = d })
        | _ -> bad "malformed scaled memory operand")
    | _ -> bad (Printf.sprintf "cannot parse operand %s" (Asm_core.Token.slice_text slice))

  let parse_operands ~mnemonic slices =
    ignore mnemonic;
    (* x86 needs no mnemonic-directed operand parsing: unlike ARM's [ldm sp!, {r0}]
       there is no operand shape whose meaning depends on which instruction it
       belongs to. The parameter is in the signature because ARM and AArch64 do
       need it, and a signature with a per-target shape would not be one
       signature. *)
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | s :: rest -> (
          match parse_one_operand s with Ok o -> go (o :: acc) rest | Error e -> Error e)
    in
    go [] slices

  let make_surface_instruction ~mnemonic ~origin ops =
    Ok { s_mnemonic = mnemonic; s_ops = ops; s_origin = origin }

  (* {2 Simplify: surface -> normalized}

     The AT&T suffix decides the operand width, and dropping it is the whole of
     x86 normalization at M1. An unsuffixed mnemonic that needs a width is
     rejected rather than defaulted: GAS infers one from the register operand,
     and inferring differently from GAS is worse than refusing. *)

  let split_suffix m =
    let n = String.length m in
    if n < 2 then (m, None)
    else
      let stem = String.sub m 0 (n - 1) in
      match m.[n - 1] with
      | 'b' -> (stem, Some 8)
      | 'w' -> (stem, Some 16)
      | 'l' -> (stem, Some 32)
      | 'q' -> (stem, Some 64)
      | _ -> (m, None)

  let simplify_instruction ~features s =
    ignore features;
    let bad msg = Error (diag ~origin:s.s_origin "x86.simplify" msg) in
    let stem, suffix = split_suffix s.s_mnemonic in
    let widthed op =
      match suffix with
      | None -> bad (Printf.sprintf "%s needs an operand-size suffix (b, w, l or q)" s.s_mnemonic)
      | Some 16 -> bad "16-bit operands need the 0x66 prefix, which is not in M1 scope"
      | Some 8 -> bad "8-bit operands are not in M1 scope"
      | Some w when w = 64 && not M.rex_allowed ->
          bad (Printf.sprintf "%s has no 64-bit operand size" M.name)
      | Some w -> Ok { i_op = op; i_width = w; i_ops = s.s_ops }
    in
    match (s.s_mnemonic, stem) with
    | "ret", _ ->
        if s.s_ops = [] then Ok { i_op = Op_ret; i_width = M.address_width; i_ops = [] }
        else bad "ret takes no operands in M1"
    | _, "add" -> widthed Op_add
    | _, "sub" -> widthed Op_sub
    | _, "mov" -> widthed Op_mov
    | _, "lea" -> widthed Op_lea
    | _ -> bad (Printf.sprintf "unknown instruction %s" s.s_mnemonic)

  (* {2 Lower: normalized -> lowered}

     One to one for every M1 form - x86 has no pseudo-instruction in the
     fixtures - but the signature returns a list because [lower_instruction] is
     where a pseudo would expand and a function that returned a single value
     could not be extended without changing every caller. *)

  let lower_instruction state i =
    ignore state;
    let bad msg = Error (diag "x86.lower" msg) in
    let imm_of v =
      match Bigint.to_int64_opt v with
      | Some x -> Ok x
      | None -> Error (diag "x86.lower" "immediate does not fit 64 bits")
    in
    let width_ok r =
      if r.rwidth = i.i_width then Ok ()
      else
        bad (Printf.sprintf "%s is %d-bit but the instruction is %d-bit" r.rname r.rwidth i.i_width)
    in
    match (i.i_op, i.i_ops) with
    | (Op_add | Op_sub), [ Op_imm v; dst ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            let ext = alu_ext i.i_op in
            match dst with
            | Op_reg r -> (
                match width_ok r with
                | Error e -> Error e
                | Ok () -> Ok [ L_alu_rm_imm { ext; width = i.i_width; rm = Rm_reg r; imm } ])
            | Op_mem m -> Ok [ L_alu_rm_imm { ext; width = i.i_width; rm = Rm_mem m; imm } ]
            | Op_imm _ -> bad "an immediate cannot be a destination"))
    | Op_mov, [ Op_imm v; Op_reg r ] -> (
        match (imm_of v, width_ok r) with
        | Ok imm, Ok () -> Ok [ L_mov_r_imm { width = i.i_width; reg = r; imm } ]
        | Error e, _ | _, Error e -> Error e)
    | Op_mov, [ Op_reg r; Op_mem m ] -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () -> Ok [ L_mov_rm_r { width = i.i_width; rm = Rm_mem m; reg = r } ])
    | Op_mov, [ Op_reg a; Op_reg b ] -> (
        match (width_ok a, width_ok b) with
        | Ok (), Ok () -> Ok [ L_mov_rm_r { width = i.i_width; rm = Rm_reg b; reg = a } ]
        | Error e, _ | _, Error e -> Error e)
    | Op_mov, [ Op_mem m; Op_reg r ] -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () -> Ok [ L_mov_r_rm { width = i.i_width; reg = r; rm = Rm_mem m } ])
    | Op_lea, [ Op_mem m; Op_reg r ] -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () -> Ok [ L_lea { width = i.i_width; reg = r; mem = m } ])
    | Op_ret, [] -> Ok [ L_ret ]
    | _ -> bad (Printf.sprintf "no %s form takes these operands" (opcode_name i.i_op))

  (* {2 The codec}

     [fixup_kind] must be declared before the first codec value, or the weak
     type variable in [rm_codec] is created before the type exists and OCaml
     reports it escaping its scope. *)

  let rm_codec = make_rm_codec ~reg_of_num

  (* REX is *derived*, never chosen: W comes from the operand width and R/X/B
     from whether a register number needs a fourth bit. So the alternative is
     between a byte and nothing, and which one applies is settled before the
     codec runs. What the codec still owns is the byte's layout and the decode
     direction. *)
  let rex_of ~width ~reg ~rm =
    if not M.rex_allowed then None
    else
      let r = reg >= 8 in
      let x, b =
        match rm with
        | Rm_reg g -> (false, g.rnum >= 8)
        | Rm_mem m ->
            ( (match m.index with Some i -> i.rnum >= 8 | None -> false),
              match m.base with Some bb -> bb.rnum >= 8 | None -> false )
      in
      let w = width = 64 in
      if w || r || x || b then
        Some
          ((if w then 8 else 0)
          lor (if r then 4 else 0)
          lor (if x then 2 else 0)
          lor if b then 1 else 0)
      else None

  let rex_codec : (int option, fixup_kind) C.t =
    if M.rex_allowed then
      C.choice ~name:"rex"
        [
          (* Present is tried first on decode, where its 0100 constant is what
             discriminates; on encode the two projections are disjoint, so order
             does not matter there. *)
          C.alt ~label:"rex-present" ~priority:0
            (C.iso_fun ~name:"rex-present"
               ~encode:(function Some v -> Some ((), Int64.of_int (v land 0xf)) | None -> None)
               ~decode:(fun ((), v) -> Some (Some (0x40 lor Int64.to_int v)))
               C.(const ~width:4 4L ** field ~width:4 "wrxb"));
          C.alt ~label:"rex-absent" ~priority:1
            (C.iso_fun ~name:"rex-absent"
               ~encode:(function None -> Some () | Some _ -> None)
               ~decode:(fun () -> Some None)
               C.empty);
        ]
    else
      (* Not an [Alt] with one branch: in 32-bit mode there is no choice to
         record, and a one-branch alternative would put a meaningless component
         in every form id. *)
      C.iso_fun ~name:"no-rex"
        ~encode:(function None -> Some () | Some _ -> None)
        ~decode:(fun () -> Some None)
        C.empty

  let width_of_rex rex = match rex with Some v when v land 8 <> 0 -> 64 | _ -> 32

  let alu_form ~label ~priority ~opcode_byte ~imm_width =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | L_alu_rm_imm { ext; width; rm; imm } ->
               let fits = if imm_width = 8 then fits_s8 imm else fits_s32 imm in
               if not fits then None
               else Some (rex_of ~width ~reg:ext ~rm, ((), ({ re_reg = ext; re_rm = rm }, imm)))
           | _ -> None)
         ~decode:(fun (rex, ((), (e, imm))) ->
           match alu_of_ext e.re_reg with
           | None -> None
           | Some _ ->
               let width = width_of_rex rex in
               Some (L_alu_rm_imm { ext = e.re_reg; width; rm = retype_rm ~width e.re_rm; imm }))
         C.(
           rex_codec
           ** const ~width:8 (Int64.of_int opcode_byte)
           ** rm_codec
           ** le ~signedness:C.Signed ~width:imm_width "imm"))

  let codec : (lowered_instruction, fixup_kind) C.t =
    C.choice ~name:M.name
      [
        (* The short immediate first: GAS encodes [subl $12, %esp] as [83 ec 0c]
           rather than [81 ec 0c 00 00 00], and the selection rule is priority
           plus the sign-extended-imm8 range check inside the form. Getting the
           order wrong here produces working code that differs from every other
           assembler by three bytes per instruction. *)
        alu_form ~label:"alu-rm-imm8" ~priority:0 ~opcode_byte:0x83 ~imm_width:8;
        alu_form ~label:"alu-rm-imm32" ~priority:1 ~opcode_byte:0x81 ~imm_width:32;
        C.alt ~label:"mov-r-imm" ~priority:2
          (C.iso_fun ~name:"mov-r-imm"
             ~encode:(function
               | L_mov_r_imm { width; reg; imm } ->
                   Some
                     ( rex_of ~width ~reg:0 ~rm:(Rm_reg reg),
                       (((), Int64.of_int (reg.rnum land 7)), imm) )
               | _ -> None)
             ~decode:(fun (rex, (((), r), imm)) ->
               let width = width_of_rex rex in
               Some
                 (L_mov_r_imm
                    { width; reg = reg_at ~width (Int64.to_int r); imm = mask ~width:32 imm }))
             C.(
               rex_codec
               ** (const ~width:5 0b10111L ** field ~width:3 "reg")
               ** le ~signedness:C.Unsigned ~width:32 "imm32"));
        C.alt ~label:"mov-rm-r" ~priority:3
          (C.iso_fun ~name:"mov-rm-r"
             ~encode:(function
               | L_mov_rm_r { width; rm; reg } ->
                   Some (rex_of ~width ~reg:reg.rnum ~rm, ((), { re_reg = reg.rnum; re_rm = rm }))
               | _ -> None)
             ~decode:(fun (rex, ((), e)) ->
               let width = width_of_rex rex in
               Some
                 (L_mov_rm_r { width; rm = retype_rm ~width e.re_rm; reg = reg_at ~width e.re_reg }))
             C.(rex_codec ** const ~width:8 0x89L ** rm_codec));
        C.alt ~label:"mov-r-rm" ~priority:4
          (C.iso_fun ~name:"mov-r-rm"
             ~encode:(function
               | L_mov_r_rm { width; reg; rm } ->
                   Some (rex_of ~width ~reg:reg.rnum ~rm, ((), { re_reg = reg.rnum; re_rm = rm }))
               | _ -> None)
             ~decode:(fun (rex, ((), e)) ->
               let width = width_of_rex rex in
               Some
                 (L_mov_r_rm { width; reg = reg_at ~width e.re_reg; rm = retype_rm ~width e.re_rm }))
             C.(rex_codec ** const ~width:8 0x8bL ** rm_codec));
        C.alt ~label:"lea" ~priority:5
          (C.iso_fun ~name:"lea"
             ~encode:(function
               | L_lea { width; reg; mem } ->
                   Some
                     ( rex_of ~width ~reg:reg.rnum ~rm:(Rm_mem mem),
                       ((), { re_reg = reg.rnum; re_rm = Rm_mem mem }) )
               | _ -> None)
             ~decode:(fun (rex, ((), e)) ->
               match e.re_rm with
               | Rm_mem m ->
                   let width = width_of_rex rex in
                   Some (L_lea { width; reg = reg_at ~width e.re_reg; mem = m })
               | Rm_reg _ -> None)
             C.(rex_codec ** const ~width:8 0x8dL ** rm_codec));
        C.alt ~label:"ret" ~priority:6
          (C.iso_fun ~name:"ret"
             ~encode:(function L_ret -> Some () | _ -> None)
             ~decode:(fun () -> Some L_ret)
             C.(const ~width:8 0xc3L));
      ]

  (* {2 Encode and decode} *)

  let encode l =
    match C.encode codec l with
    | Error e -> Error (diag "x86.encode" (Fmt.to_to_string C.pp_error e))
    | Ok enc -> Ok (C.Bits.to_bytes enc.C.bits, C.form_id enc, enc.C.placements)

  type decode_context = { state : target_state; address : int64 }

  (* The normalized instruction a lowered one came from. For M1 this is total
     and information-preserving, because no x86 form in the fixtures is a
     pseudo; the moment one is, this stops being an inverse and the disassembler
     will need the lowered form directly. *)
  let instruction_of_lowered = function
    | L_alu_rm_imm { ext; width; rm; imm } -> (
        match alu_of_ext ext with
        | None -> None
        | Some op ->
            Some
              {
                i_op = op;
                i_width = width;
                i_ops =
                  [
                    Op_imm (Bigint.of_int64 imm);
                    (match rm with Rm_reg r -> Op_reg r | Rm_mem m -> Op_mem m);
                  ];
              })
    | L_mov_r_imm { width; reg; imm } ->
        Some
          { i_op = Op_mov; i_width = width; i_ops = [ Op_imm (Bigint.of_int64 imm); Op_reg reg ] }
    | L_mov_rm_r { width; rm; reg } ->
        Some
          {
            i_op = Op_mov;
            i_width = width;
            i_ops = [ Op_reg reg; (match rm with Rm_reg r -> Op_reg r | Rm_mem m -> Op_mem m) ];
          }
    | L_mov_r_rm { width; reg; rm } ->
        Some
          {
            i_op = Op_mov;
            i_width = width;
            i_ops = [ (match rm with Rm_reg r -> Op_reg r | Rm_mem m -> Op_mem m); Op_reg reg ];
          }
    | L_lea { width; reg; mem } ->
        Some { i_op = Op_lea; i_width = width; i_ops = [ Op_mem mem; Op_reg reg ] }
    | L_ret -> Some { i_op = Op_ret; i_width = M.address_width; i_ops = [] }

  let decode ctx bytes ~pos =
    ignore ctx;
    let bits = C.Bits.of_bytes (String.sub bytes pos (String.length bytes - pos)) in
    match C.decode_bits codec bits with
    | None -> Error (diag "x86.decode" "no form matches these bytes")
    | Some d -> (
        if d.C.consumed mod 8 <> 0 then
          Error (diag "x86.decode" "a form consumed a non-whole number of bytes")
        else
          match instruction_of_lowered d.C.value with
          | None -> Error (diag "x86.decode" "decoded a form with no normalized instruction")
          | Some i -> Ok (i, String.concat "." d.C.dform, d.C.consumed / 8))

  (* {2 Fixups, padding, directives} *)

  let evaluate_fixup kind ~place ~target =
    match kind with
    | Abs32 ->
        if fits_s32 target then Ok target
        else Error (diag "x86.fixup" "absolute target does not fit 32 bits")
    | Pcrel32 ->
        let d = Int64.sub target place in
        if fits_s32 d then Ok d else Error (diag "x86.fixup" "displacement does not fit 32 bits")

  (* The Intel-recommended multi-byte no-op table. M1's fixtures never reach it
     - both x86 sections have their [.align] at offset 0, where the padding is
     empty - so this is stated as following the published table rather than as
     verified against GAS, and the first fixture that pads is what turns the
     claim into evidence. *)
  let nop_table =
    [|
      "\x90";
      "\x66\x90";
      "\x0f\x1f\x00";
      "\x0f\x1f\x40\x00";
      "\x0f\x1f\x44\x00\x00";
      "\x66\x0f\x1f\x44\x00\x00";
      "\x0f\x1f\x80\x00\x00\x00\x00";
      "\x0f\x1f\x84\x00\x00\x00\x00\x00";
      "\x66\x0f\x1f\x84\x00\x00\x00\x00\x00";
    |]

  let nop_bytes ~length =
    if length < 0 then Error (diag "x86.nop" "negative padding length")
    else
      let buf = Buffer.create length in
      let rec go n =
        if n = 0 then ()
        else
          let take = min n (Array.length nop_table) in
          Buffer.add_string buf nop_table.(take - 1);
          go (n - take)
      in
      go length;
      Ok (Buffer.contents buf)

  let handle_directive ~name ~argument state =
    ignore argument;
    ignore name;
    ignore state;
    (* x86 has no target-state directive in M1: [.syntax], [.arch] and [.fpu]
       are ARM's, and the common table owns everything the x86 fixtures use. *)
    Target_intf.Target.Unhandled
end
