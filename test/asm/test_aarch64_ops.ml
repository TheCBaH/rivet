(* Aarch64_ops agrees with the encoder on every opcode: spelling and the operand lists lowering
   takes; the encoder reads its encoding fields from the table, and the pinned constants below
   hold them to what they were. *)

open Aarch64_encode
module Ops = Aarch64_encode.Aarch64_ops
module H = Harness.Make (Aarch64_encode)

(* Every constructor, so a missing table entry is a compile error here rather than a gap. *)
let constructors =
  let open Opcode in
  [
    Add;
    Mov;
    Movz;
    Movn;
    Movk;
    Cbz;
    Cbnz;
    Sxtw;
    Uxtw;
    Udiv;
    Sdiv;
    Cset;
    Lsl;
    Lsr;
    Asr;
    Ror;
    Ubfx;
    Ubfiz;
    Sbfx;
    Sbfiz;
    Rbit;
    Rev16;
    Rev32;
    Rev;
    Clz;
    Cls;
    Stp;
    Ldp;
    Ldr;
    Ldrb;
    Ldrh;
    Ldrsb;
    Ldrsh;
    Ldrsw;
    Str;
    Strb;
    Strh;
    Ret;
    Br;
    Mrs;
    Msr;
    Sub;
    Cmp;
    Cmn;
    And;
    Orr;
    Eor;
    Bic;
    Madd;
    Msub;
    Mul;
    Csel;
    Adrp;
    Adr;
    B;
    Udf;
    Bl;
    Fmov;
    Fcmp;
    Fadd;
    Fsub;
    Fmul;
    Fdiv;
    Fcsel;
    Scvtf;
    Ucvtf;
    Fneg;
    Fmax;
    Fmla;
    Fcmeq;
    Fcmge;
    Fcmgt;
    Bsl;
    Bit;
    Not;
    Fmadd;
    Dup;
    Ins;
    Ld1;
    Ld1r;
    St1;
    Fcvtl;
    Fcvtn;
    Fsqrt;
    Frintz;
    Fcvt;
    Fcvtzs;
    Fcvtzu;
    Tst;
  ]
  @ List.map (fun c -> Bcond c) Cond.all

(* a match with no wildcard: a new constructor stops this file compiling until it is listed *)
let _complete : Opcode.t -> unit = function
  | Opcode.(
      ( Add | Mov | Movz | Movn | Movk | Cbz | Cbnz | Sxtw | Uxtw | Udiv | Sdiv | Cset | Lsl | Lsr
      | Asr | Ror | Ubfx | Ubfiz | Sbfx | Sbfiz | Rbit | Rev16 | Rev32 | Rev | Clz | Cls | Stp | Ldp
      | Ldr | Ldrb | Ldrh | Ldrsb | Ldrsh | Ldrsw | Str | Strb | Strh | Ret | Br | Mrs | Msr | Sub
      | Cmp | Cmn | And | Orr | Eor | Bic | Madd | Msub | Mul | Csel | Adrp | Adr | B | Bcond _
      | Udf | Bl | Fmov | Fcmp | Fadd | Fsub | Fmul | Fdiv | Fcsel | Scvtf | Ucvtf | Fneg | Fmax
      | Fmla | Fcmeq | Fcmge | Fcmgt | Bsl | Bit | Not | Fmadd | Dup | Ins | Ld1 | Ld1r | St1
      | Fcvtl | Fcvtn | Fsqrt | Frintz | Fcvt | Fcvtzs | Fcvtzu | Tst )) ->
      ()

let%expect_test "every opcode has one entry" =
  let missing = List.filter (fun op -> Ops.find_opt op = None) constructors in
  let extra = List.filter (fun op -> not (List.mem op constructors)) Ops.opcodes in
  let dup = List.length Ops.opcodes <> List.length (List.sort_uniq compare Ops.opcodes) in
  Printf.printf "%d constructors, %d entries, missing %d, extra %d, duplicates %b\n"
    (List.length constructors) (List.length Ops.all) (List.length missing) (List.length extra) dup;
  [%expect {| 105 constructors, 105 entries, missing 0, extra 0, duplicates false |}]

let%expect_test "legality" =
  let gated = List.filter (fun op -> required (Instruction.mk op []) <> None) constructors in
  Printf.printf "%d opcodes need a component\n" (List.length gated);
  [%expect {| 0 opcodes need a component |}]

(* an entry whose lowering is not code is lowered from its class: every builder shape of it *)
let%expect_test "regular lowering" =
  let regular, bad =
    List.fold_left
      (fun (n, bad) (e : Ops.entry) ->
        if e.Ops.lower then (n, bad)
        else
          let ok =
            List.for_all
              (fun vs ->
                List.for_all
                  (fun (v : Ops.variant) -> lower_regular (v.Ops.sample ~imm:8) <> None)
                  vs)
              e.Ops.shapes
          in
          (n + 1, if ok then bad else e.Ops.name :: bad))
      (0, []) Ops.all
  in
  Printf.printf "%d regular entries, %d with a shape not lowered from the table\n" regular
    (List.length bad);
  [%expect {| 75 regular entries, 0 with a shape not lowered from the table |}]

let%expect_test "spellings" =
  let bad =
    List.filter
      (fun op ->
        match Ops.find_opt op with
        | None -> true
        | Some e -> Opcode.of_mnemonic e.Ops.name <> Some op)
      constructors
  in
  Printf.printf "%d disagree\n" (List.length bad);
  [%expect {| 0 disagree |}]

(* {1 The operand lists} *)

(* the operands one atom stands for at position [i]: the alternatives a lowering may want *)
let atoms = Ops.Kind.[ X; D; Imm; Fimm; Mem; Sym; Sh ]

(* a general register of either width, a scalar floating-point register of either size: lowering
   and the encoder leave the agreement of widths among operands to the types of the builders *)
let generic : Ops.Kind.t -> Ops.Kind.t = function W -> X | S -> D | k -> k
let reg width i = { Reg.num = 1 + i; width; is_sp = false }

let alternatives (k : Ops.Kind.t) i =
  match k with
  | X -> [ Operand.Reg (reg 64 i); Operand.Reg (reg 32 i) ]
  | W -> [ Operand.Reg (reg 32 i) ]
  | D ->
      [
        Operand.Freg { Freg.num = 1 + i; double = true };
        Operand.Freg { Freg.num = 1 + i; double = false };
      ]
  | S -> [ Operand.Freg { Freg.num = 1 + i; double = false } ]
  | Imm -> [ Operand.Imm (Foundation.Bigint.of_int 8) ]
  | Fimm -> [ Operand.Fimm 1.0; Operand.Fimm 0.0 ]
  | Mem ->
      [ Operand.Mem { Mem.base = reg 64 i; offset = Disp.zero; writeback = false; pre = true } ]
  | Sym ->
      Operand.Sym (Asm_core.Expr.Modifier ("lo12", Asm_core.Expr.Symbol "l"))
      :: List.map (fun s -> Operand.Sym (Asm_core.Expr.Symbol s)) [ "l"; "ne"; "fpcr"; "nzcv" ]
  | Sh ->
      List.map
        (fun (kind, amount) -> Operand.Shift { Shift.kind; amount })
        [ (Shift.Lsl, 1); (Shift.Lsl, 12); (Shift.Lsl, 16); (Shift.Ext Extend.Uxtw, 1) ]
  | Q | Vec | Vlane | Vlist | Vlist_lane -> []

let rec product = function
  | [] -> [ [] ]
  | l :: rest ->
      let r = product rest in
      List.concat_map (fun x -> List.map (fun t -> x :: t) r) l

(* whether some choice of operands for these kinds lowers and encodes *)
let accepted op kinds =
  let choices = product (List.mapi (fun i k -> alternatives k i) kinds) in
  choices <> [] && List.exists (fun ops -> Result.is_ok (H.encode (Instruction.mk op ops))) choices

let rec lists n =
  if n = 0 then [ [] ]
  else [] :: List.concat_map (fun l -> List.map (fun a -> a :: l) atoms) (lists (n - 1))

let all_lists n =
  let rec go k =
    if k = 0 then [ [] ]
    else List.concat_map (fun l -> List.map (fun a -> a :: l) atoms) (go (k - 1))
  in
  List.concat (List.init (n + 1) go)

let simple kinds =
  not (List.exists (fun k -> List.mem k Ops.Kind.[ Q; Vec; Vlane; Vlist; Vlist_lane ]) kinds)

(* every builder shape's sample lowers and encodes, and no other list of the atoms does *)
let%expect_test "operand lists" =
  ignore lists;
  let bad_sample = ref [] and bad_extra = ref [] and bad_missing = ref [] in
  List.iter
    (fun (e : Ops.entry) ->
      let expected =
        List.concat_map (fun vs -> List.map (fun (v : Ops.variant) -> v.Ops.kinds) vs) e.Ops.shapes
        @ e.Ops.also
      in
      List.iter
        (fun vs ->
          List.iter
            (fun (v : Ops.variant) ->
              match H.encode (v.Ops.sample ~imm:8) with
              | Ok _ -> ()
              | Error m ->
                  bad_sample :=
                    Printf.sprintf "%s [%s]: %s" e.Ops.name
                      (String.concat " " (List.map Ops.Kind.name v.Ops.kinds))
                      m
                    :: !bad_sample)
            vs)
        e.Ops.shapes;
      let width = List.fold_left (fun m k -> max m (List.length k)) 0 expected in
      let simple_expected = List.map (List.map generic) (List.filter simple expected) in
      List.iter
        (fun kinds ->
          let want = List.mem kinds simple_expected in
          let got = accepted e.Ops.op kinds in
          let show =
            Printf.sprintf "%s [%s]" e.Ops.name (String.concat " " (List.map Ops.Kind.name kinds))
          in
          if got && not want then bad_extra := show :: !bad_extra
          else if want && not got then bad_missing := show :: !bad_missing)
        (all_lists (min 4 (width + 1))))
    Ops.all;
  let report name l =
    Printf.printf "%s: %d\n" name (List.length l);
    List.iter (Printf.printf "  %s\n") (List.rev l)
  in
  report "samples that do not encode" !bad_sample;
  report "lists lowering takes that no shape or also lists" !bad_extra;
  report "listed lists lowering refuses" !bad_missing;
  [%expect
    {|
    samples that do not encode: 0
    lists lowering takes that no shape or also lists: 0
    listed lists lowering refuses: 0 |}]

(* The encoder's per-opcode constants and spellings, pinned: the table switch leaves them as they
   are. *)
let%expect_test "pinned constants" =
  let fb =
    List.filter_map
      (fun op -> Option.map (fun o -> (Opcode.name op, o)) (fbinop_opcode op))
      constructors
  in
  List.iter (fun (n, o) -> Printf.printf "fbinop %s %d\n" n o) fb;
  List.iter
    (fun d ->
      let s = function Some v -> string_of_int v | None -> "-" in
      Printf.printf "dp1 %s %s %s\n" (dp1_name d)
        (s (dp1_opcode d ~width:32))
        (s (dp1_opcode d ~width:64)))
    [ Rbit; Rev16; Rev32; Rev; Clz; Cls ];
  List.iter
    (fun v ->
      let u, a, o = vbin_bits v in
      Printf.printf "vbin %s %d %d %d\n" (vbin_name v) u a o)
    [ Vfadd; Vfcmeq; Vfcmge; Vfcmgt; Vfdiv; Vfmax; Vfmla; Vfmul; Vfsub ];
  List.iter
    (fun v ->
      let u, a, o = vun_bits v in
      Printf.printf "vun %s %d %d %d\n" (vun_name v) u a o)
    [ Vfneg; Vfrintz; Vfsqrt ];
  List.iter
    (fun v ->
      let u, s = vlogic_bits v in
      Printf.printf "vlogic %s %d %d\n" (vlogic_name v) u s)
    [ Vand; Vbit; Vbsl; Veor; Vorr ];
  List.iter (fun k -> Printf.printf "%s %s\n" (shift_name k) (logical_name k)) [ 0; 1; 2; 3 ];
  List.iter
    (fun m ->
      Printf.printf "%s -> %s\n" m
        (match Opcode.of_mnemonic m with Some op -> Opcode.name op | None -> "none"))
    [
      "add";
      "mov";
      "mvn";
      "not";
      "b";
      "b.eq";
      "b.hs";
      "b.lo";
      "beq";
      "bge";
      "bne";
      "bl";
      "br";
      "blr";
      "bic";
      "ldr";
      "ldrsw";
      "fcvtzu";
      "ld1r";
      "tst";
      "frintz";
      "b.xx";
      "bxx";
      "nop";
      "";
    ];
  [%expect
    {|
    fbinop fadd 2
    fbinop fsub 3
    fbinop fmul 0
    fbinop fdiv 1
    fbinop fmax 4
    dp1 rbit 0 0
    dp1 rev16 1 1
    dp1 rev32 - 2
    dp1 rev 2 3
    dp1 clz 4 4
    dp1 cls 5 5
    vbin fadd 0 0 26
    vbin fcmeq 0 0 28
    vbin fcmge 1 0 28
    vbin fcmgt 1 1 28
    vbin fdiv 1 0 31
    vbin fmax 0 0 30
    vbin fmla 0 0 25
    vbin fmul 1 0 27
    vbin fsub 0 1 26
    vun fneg 1 1 15
    vun frintz 0 1 25
    vun fsqrt 1 1 31
    vlogic and 0 0
    vlogic bit 1 2
    vlogic bsl 1 1
    vlogic eor 1 0
    vlogic orr 0 2
    lsl and
    lsr orr
    asr eor
    ror ands
    add -> add
    mov -> mov
    mvn -> not
    not -> not
    b -> b
    b.eq -> b.eq
    b.hs -> b.cs
    b.lo -> b.cc
    beq -> b.eq
    bge -> b.ge
    bne -> b.ne
    bl -> bl
    br -> br
    blr -> none
    bic -> bic
    ldr -> ldr
    ldrsw -> ldrsw
    fcvtzu -> fcvtzu
    ld1r -> ld1r
    tst -> tst
    frintz -> frintz
    b.xx -> none
    bxx -> none
    nop -> none
     -> none |}]
