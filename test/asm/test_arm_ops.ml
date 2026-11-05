(* Arm_ops agrees with the encoder on every opcode: spelling, encoding fields, flag effects and
   the operand lists lowering takes. *)

open Arm_encode
module Ops = Arm_encode.Arm_ops

(* Every constructor, so a missing table entry is a compile error here rather than a gap. *)
let constructors =
  let shifts = [ 0; 1; 2; 3 ] and v3 = V3.[ Vadd; Vsub; Vmul; Vdiv; Vmla ] in
  let open Opcode in
  [
    Mov;
    Add;
    Sub;
    And;
    Cmp;
    Cmn;
    Eor;
    Rsb;
    Orr;
    Mvn;
    Bic;
    Adc;
    Sbc;
    Adds;
    Subs;
    Rsbs;
    Orrs;
    Adcs;
    Sbcs;
    Mul;
    Mla;
    Umull;
    Str;
    Ldr;
    Bx;
    Strb;
    Ldrb;
    Strh;
    Ldrh;
    Ldrsb;
    Ldrsh;
    Sbfx;
    Clz;
    Rev;
    Rev16;
    Udf;
    Bl;
    B;
    Movw;
    Movt;
    Ite;
    Nop;
    Rrx;
    Rrxs;
    Vmov_d;
    Vmov_s;
    Vmov_core;
    Vneg_d;
    Vneg_s;
    Vcmp_d;
    Vcmp_s;
    Vmrs;
    Vcvt_f32_f64;
    Vcvt_f64_f32;
    Vcvt_f32_s32;
    Vcvt_f64_s32;
    Vcvt_f64_u32;
    Vcvt_s32_f64;
    Vldr;
    Vstr;
    Vpush;
    Push;
    Stmia;
    Ldmia;
    Pop;
  ]
  @ List.map (fun k -> Shift k) shifts
  @ List.map (fun k -> Shifts k) shifts
  @ List.map (fun o -> V3d o) v3
  @ List.map (fun o -> V3s o) v3

(* a compile-time check that the list above names every constructor *)
let (_ : Opcode.t -> unit) =
  let open Opcode in
  function
  | Mov | Add | Sub | And | Cmp | Cmn | Eor | Rsb | Orr | Mvn | Bic | Adc | Sbc | Adds | Subs | Rsbs
  | Orrs | Adcs | Sbcs | Mul | Mla | Umull | Str | Ldr | Bx | Strb | Ldrb | Strh | Ldrh | Ldrsb
  | Ldrsh | Sbfx | Clz | Rev | Rev16 | Udf | Bl | B | Movw | Movt | Ite | Nop | Shift _ | Shifts _
  | Rrx | Rrxs | Vmov_d | Vmov_s | Vmov_core | V3d _ | V3s _ | Vneg_d | Vneg_s | Vcmp_d | Vcmp_s
  | Vmrs | Vcvt_f32_f64 | Vcvt_f64_f32 | Vcvt_f32_s32 | Vcvt_f64_s32 | Vcvt_f64_u32 | Vcvt_s32_f64
  | Vldr | Vstr | Vpush | Push | Stmia | Ldmia | Pop ->
      ()

let report what l = Printf.printf "%s: %s\n" what (if l = [] then "ok" else String.concat ", " l)

let%expect_test "the table has one entry per opcode" =
  let ops = List.map (fun (e : Ops.entry) -> e.Ops.op) Ops.all in
  report "missing"
    (List.filter_map (fun o -> if List.mem o ops then None else Some (Opcode.name o)) constructors);
  report "duplicate"
    (List.filter_map
       (fun o -> if List.length (List.filter (( = ) o) ops) > 1 then Some (Opcode.name o) else None)
       ops);
  report "extra"
    (List.filter_map (fun o -> if List.mem o constructors then None else Some (Opcode.name o)) ops);
  Printf.printf "%d entries\n" (List.length Ops.all);
  [%expect {|
    missing: ok
    duplicate: ok
    extra: ok
    83 entries |}]

(* [vmla.f32] is printed but not parsed: the encoder lowers it, its VFP spelling table lacks it. *)
let%expect_test "every spelling parses to its opcode" =
  report "parse"
    (List.filter_map
       (fun (e : Ops.entry) ->
         let parsed =
           match e.Ops.group with
           | Ops.Core -> Opcode.of_mnemonic e.Ops.name
           | Ops.Vfp -> (
               match String.split_on_char '.' e.Ops.name with
               | stem :: rest -> Opcode.vfp_opcode_of stem rest
               | [] -> None)
         in
         if parsed = Some e.Ops.op then None else Some e.Ops.name)
       Ops.all);
  [%expect {| parse: ok |}]

(* {1 The operand lists lowering takes} *)

let atoms = Ops.Kind.[ R; D; S; Imm; Fimm; Mem; Sym; Sh; Reglist; Dreglist; Wb ]

let operand i : Ops.Kind.t -> Operand.t = function
  | R -> Operand.Reg (Reg.of_num (1 + i))
  | D -> Operand.Dreg (Dreg.of_num (1 + i))
  | S -> Operand.Sreg (Sreg.of_num (1 + i))
  | Imm -> Operand.Imm (Foundation.Bigint.of_int 1)
  | Fimm -> Operand.FImm 1.0
  | Mem ->
      Operand.Mem
        { Mem.base = Reg.of_num (1 + i); offset = Mem.Imm 0L; writeback = false; pre = true }
  | Sym -> Operand.Sym (Asm_core.Expr.Symbol "l")
  | Sh -> Operand.Shifted { reg = Reg.of_num (1 + i); kind = 0; amount = 2 }
  | Reglist -> Operand.Reglist [ 1; 2 ]
  | Dreglist -> Operand.Dreglist [ 1; 2 ]
  | Wb -> Operand.Reg_writeback (Reg.of_num (1 + i))

let rec lists n =
  if n = 0 then [ [] ]
  else List.concat_map (fun rest -> List.map (fun k -> k :: rest) atoms) (lists (n - 1))

let all_lists = List.concat_map (fun n -> List.map List.rev (lists n)) [ 0; 1; 2; 3; 4 ]
let lowers insn = Result.is_ok (lower_instruction default_state insn)
let generic op l = Instruction.mk op (List.mapi operand l)

let%expect_test "lowering takes exactly the table's operand lists" =
  let bad = ref [] in
  List.iter
    (fun (e : Ops.entry) ->
      let named = List.map (fun (s : Ops.shape) -> s.Ops.kinds) e.Ops.shapes in
      (* each builder's sample lowers *)
      List.iter
        (fun (s : Ops.shape) ->
          if not (lowers (s.Ops.sample ~cond:Cond.Al 1)) then
            bad :=
              Printf.sprintf "%s refuses its %s sample" e.Ops.name
                (String.concat " " (List.map Ops.Kind.name s.Ops.kinds))
              :: !bad)
        e.Ops.shapes;
      (* the operand lists without a builder lower with plain operands *)
      List.iter
        (fun l ->
          if not (lowers (generic e.Ops.op l)) then
            bad :=
              Printf.sprintf "%s refuses %s" e.Ops.name
                (String.concat " " (List.map Ops.Kind.name l))
              :: !bad)
        e.Ops.also;
      (* and nothing else does; [ite] ignores its operands *)
      if e.Ops.op <> Opcode.Ite then
        List.iter
          (fun l ->
            if
              (not (List.mem l named))
              && (not (List.mem l e.Ops.also))
              && lowers (generic e.Ops.op l)
            then
              bad :=
                Printf.sprintf "%s takes %s" e.Ops.name
                  (String.concat " " (List.map Ops.Kind.name l))
                :: !bad)
          all_lists)
    Ops.all;
  report "lowering" (List.rev !bad);
  [%expect {| lowering: ok |}]

(* The encoder's constants per opcode, as the encoder spelled them before the table became their
   source: the table keeps them byte for byte. *)
let%expect_test "per-opcode constants" =
  List.iter
    (fun o ->
      let extra =
        match o with
        | Opcode.Clz | Opcode.Rev | Opcode.Rev16 ->
            let hi, lo = Option.get (Ops.rd_rm o) in
            Printf.sprintf " rd_rm %Lx %Lx" hi lo
        | Opcode.Strh | Opcode.Ldrh | Opcode.Ldrsb | Opcode.Ldrsh ->
            let k =
              match o with Opcode.Strh -> 0 | Opcode.Ldrh -> 1 | Opcode.Ldrsb -> 2 | _ -> 3
            in
            let l, s, h = x_bits k in
            Printf.sprintf " x %d %Ld%Ld%Ld %s" k l s h (x_name k)
        | Opcode.V3d v | Opcode.V3s v ->
            let a, b, c, d = V3.selector v in
            Printf.sprintf " v3 %Ld%Ld%Ld%Ld" a b c d
        | _ -> ""
      in
      Printf.printf "%-14s dp %2d%s%s\n" (Opcode.name o) (Opcode.to_dp o)
        (if Opcode.is_compare o then " compare" else "")
        extra)
    constructors;
  [%expect
    {|
    mov            dp 13
    add            dp  4
    sub            dp  2
    and            dp  0
    cmp            dp 10 compare
    cmn            dp 11 compare
    eor            dp  1
    rsb            dp  3
    orr            dp 12
    mvn            dp 15
    bic            dp 14
    adc            dp  5
    sbc            dp  6
    adds           dp  4
    subs           dp  2
    rsbs           dp  3
    orrs           dp 12
    adcs           dp  5
    sbcs           dp  6
    mul            dp -1
    mla            dp -1
    umull          dp -1
    str            dp -1
    ldr            dp -1
    bx             dp -1
    strb           dp -1
    ldrb           dp -1
    strh           dp -1 x 0 001 strh
    ldrh           dp -1 x 1 101 ldrh
    ldrsb          dp -1 x 2 110 ldrsb
    ldrsh          dp -1 x 3 111 ldrsh
    sbfx           dp -1
    clz            dp -1 rd_rm 16 1
    rev            dp -1 rd_rm 6b 3
    rev16          dp -1 rd_rm 6b b
    udf            dp -1
    bl             dp -1
    b              dp -1
    movw           dp -1
    movt           dp -1
    ite            dp -1
    nop            dp -1
    rrx            dp -1
    rrxs           dp -1
    vmov.f64       dp -1
    vmov.f32       dp -1
    vmov           dp -1
    vneg.f64       dp -1
    vneg.f32       dp -1
    vcmp.f64       dp -1
    vcmp.f32       dp -1
    vmrs           dp -1
    vcvt.f32.f64   dp -1
    vcvt.f64.f32   dp -1
    vcvt.f32.s32   dp -1
    vcvt.f64.s32   dp -1
    vcvt.f64.u32   dp -1
    vcvt.s32.f64   dp -1
    vldr           dp -1
    vstr           dp -1
    vpush.64       dp -1
    push           dp -1
    stmia          dp -1
    ldmia          dp -1
    pop            dp -1
    lsl            dp -1
    lsr            dp -1
    asr            dp -1
    ror            dp -1
    lsls           dp -1
    lsrs           dp -1
    asrs           dp -1
    rors           dp -1
    vadd.f64       dp -1 v3 0110
    vsub.f64       dp -1 v3 0111
    vmul.f64       dp -1 v3 0100
    vdiv.f64       dp -1 v3 1000
    vmla.f64       dp -1 v3 0000
    vadd.f32       dp -1 v3 0110
    vsub.f32       dp -1 v3 0111
    vmul.f32       dp -1 v3 0100
    vdiv.f32       dp -1 v3 1000
    vmla.f32       dp -1 v3 0000 |}]

let%expect_test "mnemonics" =
  let show m =
    Printf.printf "%-14s %s\n" m
      (match Opcode.of_mnemonic m with
      | Some o -> "core " ^ Opcode.name o
      | None -> (
          match Opcode.split_vfp_mnemonic m with
          | Some (stem, c, rest) -> (
              match Opcode.vfp_opcode_of stem rest with
              | Some o -> Printf.sprintf "vfp %s %s" (Opcode.name o) (Cond.name c)
              | None -> "-")
          | None -> "-"))
  in
  List.iter show
    [
      "mov";
      "it";
      "ite";
      "lsl";
      "lsrs";
      "ror";
      "rors";
      "rrx";
      "rrxs";
      "vmov";
      "vmov.f64";
      "vmoveq.f32";
      "vmrs";
      "vldr";
      "vstr";
      "vpush.64";
      "vcvt.f64.s32";
      "vcvt.s32.f64";
      "vadd.f64";
      "vmla.f64";
      "vmla.f32";
      "vmlaeq.f32";
      "vneg.f64";
      "vcmp.f32";
      "bleq";
      "blt";
      "addseq";
      "foo";
      "udf";
      "nop";
    ];
  [%expect
    {|
    mov            core mov
    it             core ite
    ite            core ite
    lsl            core lsl
    lsrs           core lsrs
    ror            core ror
    rors           core rors
    rrx            core rrx
    rrxs           core rrxs
    vmov           vfp vmov al
    vmov.f64       vfp vmov.f64 al
    vmoveq.f32     vfp vmov.f32 eq
    vmrs           vfp vmrs al
    vldr           vfp vldr al
    vstr           vfp vstr al
    vpush.64       vfp vpush.64 al
    vcvt.f64.s32   vfp vcvt.f64.s32 al
    vcvt.s32.f64   vfp vcvt.s32.f64 al
    vadd.f64       vfp vadd.f64 al
    vmla.f64       vfp vmla.f64 al
    vmla.f32       vfp vmla.f32 al
    vmlaeq.f32     vfp vmla.f32 eq
    vneg.f64       vfp vneg.f64 al
    vcmp.f32       vfp vcmp.f32 al
    bleq           -
    blt            -
    addseq         -
    foo            -
    udf            core udf
    nop            core nop |}]

let%expect_test "legality: no opcode needs a component" =
  Printf.printf "components %d\n" (List.length components);
  report "required"
    (List.filter_map
       (fun (e : Ops.entry) ->
         if e.Ops.ext = [] && required (Instruction.mk e.Ops.op []) = None then None
         else Some e.Ops.name)
       Ops.all);
  [%expect {|
    components 0
    required: ok |}]

let%expect_test "decoding names an opcode by its encoding class" =
  report "unique"
    (List.filter_map
       (fun (e : Ops.entry) ->
         if Ops.of_enc e.Ops.enc = Some e.Ops.op then None else Some e.Ops.name)
       Ops.all);
  [%expect {| unique: ok |}]
