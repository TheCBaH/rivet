(* The AArch64 builders: examples read like their text; every builder's sample encodes like the
   text it stands for; and the bytes read back as text that assembles to them again. *)

open Aarch64_asm
module Ops = Aarch64_encode.Aarch64_ops
module H = Harness.Make (Aarch64_encode)

let%expect_test "builders read like their text" =
  let c = H.check in
  c (add x1 x2 x3) "add x1, x2, x3";
  c (add w1 w2 w3) "add w1, w2, w3";
  c (add x1 xzr x3) "add x1, xzr, x3";
  c (add_i sp sp 16) "add sp, sp, #16";
  c (add_i x0 x1 4096) "add x0, x1, #4096";
  c (add_i_lsl12 x0 x1 2) "add x0, x1, #2, lsl #12";
  c (add_sh x0 x1 x2 (by_lsl 3)) "add x0, x1, x2, lsl #3";
  c (add_ext x0 x1 w2 (ext_uxtw 2)) "add x0, x1, w2, uxtw #2";
  c (add_lo12 x9 x9 (label "l")) "l: add x9, x9, #:lo12:l";
  c (sub_i sp sp 32) "sub sp, sp, #32";
  c (cmp x1 x2) "cmp x1, x2";
  c (cmp_i w0 5) "cmp w0, #5";
  c (cmn_i x0 3) "cmn x0, #3";
  c (mov x0 x1) "mov x0, x1";
  c (mov x0 sp) "mov x0, sp";
  c (mov_i x0 0xffff0000L) "mov x0, #0xffff0000";
  c (movz x0 7) "movz x0, #7";
  c (movk_lsl x0 1 16) "movk x0, #1, lsl #16";
  c (mul x0 x1 x2) "mul x0, x1, x2";
  c (madd x0 x1 x2 x3) "madd x0, x1, x2, x3";
  c (udiv w0 w1 w2) "udiv w0, w1, w2";
  c (lsl_ x0 x1 x2) "lsl x0, x1, x2";
  c (lsl_i x0 x1 3) "lsl x0, x1, #3";
  c (ubfx x0 x1 4 8) "ubfx x0, x1, #4, #8";
  c (sxtw x0 w1) "sxtw x0, w1";
  c (clz x0 x1) "clz x0, x1";
  c (rev32 x0 x1) "rev32 x0, x1";
  c (and_i x0 x1 0xffL) "and x0, x1, #0xff";
  c (eor_sh x0 x1 x2 (by_lsr 4)) "eor x0, x1, x2, lsr #4";
  c (tst_i x0 1L) "tst x0, #1";
  c (csel x0 x1 x2 Cond.Ne) "csel x0, x1, x2, ne";
  c (cset w0 Cond.Eq) "cset w0, eq";
  c (ldr x0 (off sp 16)) "ldr x0, [sp, #16]";
  c (ldr w0 (off x1 4)) "ldr w0, [x1, #4]";
  c (ldr x0 (roff x1 x2)) "ldr x0, [x1, x2]";
  c (ldr x0 (roff ~extend:Extend.Sxtw ~amount:3 x1 w2)) "ldr x0, [x1, w2, sxtw #3]";
  c (ldr_post x0 sp 8) "ldr x0, [sp], #8";
  c (ldrb w0 (off x1 1)) "ldrb w0, [x1, #1]";
  c (ldrsw x0 (off x1 4)) "ldrsw x0, [x1, #4]";
  c (str x0 (off_lo12 x1 (label "l"))) "l: str x0, [x1, #:lo12:l]";
  c (stp x29 x30 (pre sp (-16))) "stp x29, x30, [sp, #-16]!";
  c (ldp x29 x30 (off sp 16)) "ldp x29, x30, [sp, #16]";
  c (ldp_post x29 x30 sp 16) "ldp x29, x30, [sp], #16";
  c (ldr_f d0 (off x1 8)) "ldr d0, [x1, #8]";
  c (str_q q0 (off x1 16)) "str q0, [x1, #16]";
  c (ret ()) "ret";
  c (ret_r x1) "ret x1";
  c (br x16) "br x16";
  c (udf 7) "udf #7";
  c (mrs_fpcr x0) "mrs x0, fpcr";
  c (msr_nzcv x1) "msr nzcv, x1";
  c (fadd d0 d1 d2) "fadd d0, d1, d2";
  c (fmul s0 s1 s2) "fmul s0, s1, s2";
  c (fmov_i d0 1.0) "fmov d0, #1.0000000";
  c (fmov_d_x d0 xzr) "fmov d0, xzr";
  c (fcmp_z d0) "fcmp d0, #0.0";
  c (fcsel d0 d1 d2 Cond.Lt) "fcsel d0, d1, d2, lt";
  c (scvtf d0 x1) "scvtf d0, x1";
  c (fcvt_s_d s0 d1) "fcvt s0, d1";
  c (fcvtzs x0 d1) "fcvtzs x0, d1";
  c (fmadd d0 d1 d2 d3) "fmadd d0, d1, d2, d3";
  c (fadd_v (v4s v0) (v4s v1) (v4s v2)) "fadd v0.4s, v1.4s, v2.4s";
  c (fmla_v (v2d v0) (v2d v1) (v2d v2)) "fmla v0.2d, v1.2d, v2.2d";
  c (eor_v (v16b v0) (v16b v1) (v16b v2)) "eor v0.16b, v1.16b, v2.16b";
  c (dup_gpr (v4s v0) w1) "dup v0.4s, w1";
  c (ld1r (list_4s v0) (off x1 0)) "ld1r {v0.4s}, [x1]";
  [%expect
    {|
    aarch64  add x1, x2, x3               41 00 03 8b              same
    aarch64  add w1, w2, w3               41 00 03 0b              same
    aarch64  add x1, xzr, x3              e1 03 03 8b              same
    aarch64  add sp, sp, #16              ff 43 00 91              same
    aarch64  add x0, x1, #4096            20 04 40 91              same
    aarch64  add x0, x1, #2, lsl #12      20 08 40 91              same
    aarch64  add x0, x1, x2, lsl #3       20 0c 02 8b              same
    aarch64  add x0, x1, w2, uxtw #2      20 48 22 8b              same
    aarch64  l: add x9, x9, #:lo12:l      29 01 00 91              same
    aarch64  sub sp, sp, #32              ff 83 00 d1              same
    aarch64  cmp x1, x2                   3f 00 02 eb              same
    aarch64  cmp w0, #5                   1f 14 00 71              same
    aarch64  cmn x0, #3                   1f 0c 00 b1              same
    aarch64  mov x0, x1                   e0 03 01 aa              same
    aarch64  mov x0, sp                   e0 03 00 91              same
    aarch64  mov x0, #0xffff0000          e0 ff bf d2              same
    aarch64  movz x0, #7                  e0 00 80 d2              same
    aarch64  movk x0, #1, lsl #16         20 00 a0 f2              same
    aarch64  mul x0, x1, x2               20 7c 02 9b              same
    aarch64  madd x0, x1, x2, x3          20 0c 02 9b              same
    aarch64  udiv w0, w1, w2              20 08 c2 1a              same
    aarch64  lsl x0, x1, x2               20 20 c2 9a              same
    aarch64  lsl x0, x1, #3               20 f0 7d d3              same
    aarch64  ubfx x0, x1, #4, #8          20 2c 44 d3              same
    aarch64  sxtw x0, w1                  20 7c 40 93              same
    aarch64  clz x0, x1                   20 10 c0 da              same
    aarch64  rev32 x0, x1                 20 08 c0 da              same
    aarch64  and x0, x1, #0xff            20 1c 40 92              same
    aarch64  eor x0, x1, x2, lsr #4       20 10 42 ca              same
    aarch64  tst x0, #1                   1f 00 40 f2              same
    aarch64  csel x0, x1, x2, ne          20 10 82 9a              same
    aarch64  cset w0, eq                  e0 17 9f 1a              same
    aarch64  ldr x0, [sp, #16]            e0 0b 40 f9              same
    aarch64  ldr w0, [x1, #4]             20 04 40 b9              same
    aarch64  ldr x0, [x1, x2]             20 68 62 f8              same
    aarch64  ldr x0, [x1, w2, sxtw #3]    20 d8 62 f8              same
    aarch64  ldr x0, [sp], #8             e0 87 40 f8              same
    aarch64  ldrb w0, [x1, #1]            20 04 40 39              same
    aarch64  ldrsw x0, [x1, #4]           20 04 80 b9              same
    aarch64  l: str x0, [x1, #:lo12:l]    20 00 00 f9              same
    aarch64  stp x29, x30, [sp, #-16]!    fd 7b bf a9              same
    aarch64  ldp x29, x30, [sp, #16]      fd 7b 41 a9              same
    aarch64  ldp x29, x30, [sp], #16      fd 7b c1 a8              same
    aarch64  ldr d0, [x1, #8]             20 04 40 fd              same
    aarch64  str q0, [x1, #16]            20 04 80 3d              same
    aarch64  ret                          c0 03 5f d6              same
    aarch64  ret x1                       20 00 5f d6              same
    aarch64  br x16                       00 02 1f d6              same
    aarch64  udf #7                       07 00 00 00              same
    aarch64  mrs x0, fpcr                 00 44 3b d5              same
    aarch64  msr nzcv, x1                 01 42 1b d5              same
    aarch64  fadd d0, d1, d2              20 28 62 1e              same
    aarch64  fmul s0, s1, s2              20 08 22 1e              same
    aarch64  fmov d0, #1.0000000          00 10 6e 1e              same
    aarch64  fmov d0, xzr                 e0 03 67 9e              same
    aarch64  fcmp d0, #0.0                08 20 60 1e              same
    aarch64  fcsel d0, d1, d2, lt         20 bc 62 1e              same
    aarch64  scvtf d0, x1                 20 00 62 9e              same
    aarch64  fcvt s0, d1                  20 40 62 1e              same
    aarch64  fcvtzs x0, d1                20 00 78 9e              same
    aarch64  fmadd d0, d1, d2, d3         20 0c 42 1f              same
    aarch64  fadd v0.4s, v1.4s, v2.4s     20 d4 22 4e              same
    aarch64  fmla v0.2d, v1.2d, v2.2d     20 cc 62 4e              same
    aarch64  eor v0.16b, v1.16b, v2.16b   20 1c 22 6e              same
    aarch64  dup v0.4s, w1                20 0c 04 4e              same
    aarch64  ld1r {v0.4s}, [x1]           20 c8 40 4d              same |}]

let%expect_test "a list of builders reads like the lines it stands for" =
  let program = [ add x1 x2 x3; ldr x0 (off sp 16) ] in
  let text = "add x1, x2, x3\n\tldr x0, [sp, #16]" in
  let bytes = List.map H.encode program in
  let joined =
    List.fold_left
      (fun acc b -> match (acc, b) with Ok a, Ok b -> Ok (a ^ b) | _ -> Error "encode")
      (Ok "") bytes
  in
  Printf.printf "%b\n" (joined = H.assemble text);
  [%expect {| true |}]

(* The text of an instruction, with its label where the unbound encoding leaves zero: on the
   instruction itself. *)
let text insn =
  let t = Format.asprintf "%a" Aarch64_encode.Instruction.pp_gnu insn in
  let has_sym =
    List.exists
      (function
        | Aarch64_encode.Operand.Sym _ -> true
        | Aarch64_encode.Operand.Mem { Aarch64_encode.Mem.offset = Aarch64_encode.Disp.Sym _; _ } ->
            true
        | _ -> false)
      insn.Aarch64_encode.Instruction.ops
  in
  if has_sym then "l:\n\t" ^ t else t

let all_shapes =
  List.concat_map
    (fun (e : Ops.entry) -> List.concat_map (fun vs -> List.map (fun v -> (e, v)) vs) e.Ops.shapes)
    Ops.all

let%expect_test "every builder encodes like its text" =
  let bad = ref [] and n = ref 0 in
  List.iter
    (fun ((e : Ops.entry), (v : Ops.variant)) ->
      incr n;
      let insn = v.Ops.sample ~imm:8 in
      let t = text insn in
      match (H.encode insn, H.assemble t) with
      | Ok a, Ok b when String.equal a b -> ()
      | r, s ->
          bad :=
            Printf.sprintf "%s: %s / %s" e.Ops.name
              (String.map (function '\n' | '\t' -> ' ' | c -> c) t)
              (match (r, s) with
              | Error m, _ -> m
              | _, Error m -> m
              | Ok a, Ok b -> Harness.hex a ^ " vs " ^ Harness.hex b)
            :: !bad)
    all_shapes;
  Printf.printf "%d builder shapes, %d differ\n" !n (List.length !bad);
  List.iter print_endline (List.rev !bad);
  [%expect {| 305 builder shapes, 0 differ |}]

let%expect_test "the bytes read back as text that assembles to them" =
  let bad = ref [] in
  List.iter
    (fun ((e : Ops.entry), (v : Ops.variant)) ->
      let insn = v.Ops.sample ~imm:8 in
      match H.encode insn with
      | Error _ -> ()
      | Ok b -> (
          match H.decode b with
          | Error m -> bad := Printf.sprintf "%s: %s" e.Ops.name m :: !bad
          | Ok t -> (
              match H.assemble (if String.contains t ':' then t else t) with
              | Ok b' when String.equal b b' -> ()
              | _ -> bad := Printf.sprintf "%s: %s" e.Ops.name t :: !bad)))
    all_shapes;
  Printf.printf "%d do not\n" (List.length !bad);
  List.iter print_endline (List.sort_uniq compare (List.rev !bad));
  [%expect {| 0 do not |}]
