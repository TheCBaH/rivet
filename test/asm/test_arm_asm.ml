(* The ARM builders: examples read like their text; every builder's sample, with every condition,
   encodes like the text it stands for; a flag-setting form differs from its plain form in the S
   bit alone; and the bytes read back as text that assembles to them again. *)

open Arm_asm
module Ops = Arm_encode.Arm_ops
module H = Harness.Make (Arm_encode)

let%expect_test "builders read like their text" =
  let c = H.check in
  c (add r0 r1 r2) "add r0, r1, r2";
  c (add_i r0 r1 4L) "add r0, r1, #4";
  c (add_sh r0 r1 (shifted Lsl r2 2)) "add r0, r1, r2, lsl #2";
  c (adds ~cond:Cond.Ne r0 r1 r2) "addsne r0, r1, r2";
  c (sub_i ~cond:Cond.Cs sp sp 16L) "subcs sp, sp, #16";
  c (mov_i r7 0xff000000L) "mov r7, #0xff000000";
  c (mvn r3 r4) "mvn r3, r4";
  c (cmp_i r0 0L) "cmp r0, #0";
  c (orrs r1 r2 r3) "orrs r1, r2, r3";
  c (lsl_ r0 r1 r2) "lsl r0, r1, r2";
  c (lsr_i r0 r1 3) "lsr r0, r1, #3";
  c (lsrs_i r0 r1 1) "lsrs r0, r1, #1";
  c (rrx r0 r1) "rrx r0, r1";
  c (mul r0 r1 r2) "mul r0, r1, r2";
  c (mla r0 r1 r2 r3) "mla r0, r1, r2, r3";
  c (umull r0 r1 r2 r3) "umull r0, r1, r2, r3";
  c (ldr r0 (mem ~off:4 r1)) "ldr r0, [r1, #4]";
  c (str r0 (mem_wb ~off:(-4) sp)) "str r0, [sp, #-4]!";
  c (ldr_post r0 r1 4) "ldr r0, [r1], #4";
  c (ldrb r0 (mem_reg r1 r2)) "ldrb r0, [r1, r2]";
  c (ldr r0 (mem_shifted r1 r2 Lsl 2)) "ldr r0, [r1, r2, lsl #2]";
  c (ldrh r0 (mem ~off:2 r1)) "ldrh r0, [r1, #2]";
  c (ldrsb r0 (mem_reg ~negate:true r1 r2)) "ldrsb r0, [r1, -r2]";
  c (clz r0 r1) "clz r0, r1";
  c (rev16 r0 r1) "rev16 r0, r1";
  c (sbfx r0 r1 4 8) "sbfx r0, r1, #4, #8";
  c (bx lr) "bx lr";
  c (movw r0 0x1234) "movw r0, #0x1234";
  c (nop ()) "nop";
  c (udf 7) "udf #7";
  c (push [ r4; r5; lr ]) "push {r4, r5, lr}";
  c (pop [ r4; r5; pc ]) "pop {r4, r5, pc}";
  c (stmia r3 [ r0; r1 ]) "stmia r3!, {r0, r1}";
  c (vadd_f64 d0 d1 d2) "vadd.f64 d0, d1, d2";
  c (vmul_f32 ~cond:Cond.Eq s0 s1 s2) "vmuleq.f32 s0, s1, s2";
  c (vmov_f64_i d0 1.5) "vmov.f64 d0, #1.5";
  c (vmov_to_s s0 r1) "vmov s0, r1";
  c (vmov_from_d r0 r1 d2) "vmov r0, r1, d2";
  c (vcmp_f64_z d0) "vcmp.f64 d0, #0";
  c (vmrs ()) "vmrs APSR_nzcv, FPSCR";
  c (vcvt_f64_s32 d0 s1) "vcvt.f64.s32 d0, s1";
  c (vldr_d d0 (mem ~off:8 r1)) "vldr d0, [r1, #8]";
  c (vstr_s s0 (mem r1)) "vstr s0, [r1]";
  c (vpush_64 [ d8; d9 ]) "vpush.64 {d8, d9}";
  [%expect
    {|
    arm      add r0, r1, r2               02 00 81 e0              same
    arm      add r0, r1, #4               04 00 81 e2              same
    arm      add r0, r1, r2, lsl #2       02 01 81 e0              same
    arm      addsne r0, r1, r2            02 00 91 10              same
    arm      subcs sp, sp, #16            10 d0 4d 22              same
    arm      mov r7, #0xff000000          ff 74 a0 e3              same
    arm      mvn r3, r4                   04 30 e0 e1              same
    arm      cmp r0, #0                   00 00 50 e3              same
    arm      orrs r1, r2, r3              03 10 92 e1              same
    arm      lsl r0, r1, r2               11 02 a0 e1              same
    arm      lsr r0, r1, #3               a1 01 a0 e1              same
    arm      lsrs r0, r1, #1              a1 00 b0 e1              same
    arm      rrx r0, r1                   61 00 a0 e1              same
    arm      mul r0, r1, r2               91 02 00 e0              same
    arm      mla r0, r1, r2, r3           91 32 20 e0              same
    arm      umull r0, r1, r2, r3         92 03 81 e0              same
    arm      ldr r0, [r1, #4]             04 00 91 e5              same
    arm      str r0, [sp, #-4]!           04 00 2d e5              same
    arm      ldr r0, [r1], #4             04 00 91 e4              same
    arm      ldrb r0, [r1, r2]            02 00 d1 e7              same
    arm      ldr r0, [r1, r2, lsl #2]     02 01 91 e7              same
    arm      ldrh r0, [r1, #2]            b2 00 d1 e1              same
    arm      ldrsb r0, [r1, -r2]          d2 00 11 e1              same
    arm      clz r0, r1                   11 0f 6f e1              same
    arm      rev16 r0, r1                 b1 0f bf e6              same
    arm      sbfx r0, r1, #4, #8          51 02 a7 e7              same
    arm      bx lr                        1e ff 2f e1              same
    arm      movw r0, #0x1234             34 02 01 e3              same
    arm      nop                          00 f0 20 e3              same
    arm      udf #7                       f7 00 f0 e7              same
    arm      push {r4, r5, lr}            30 40 2d e9              same
    arm      pop {r4, r5, pc}             30 80 bd e8              same
    arm      stmia r3!, {r0, r1}          03 00 a3 e8              same
    arm      vadd.f64 d0, d1, d2          02 0b 31 ee              same
    arm      vmuleq.f32 s0, s1, s2        81 0a 20 0e              same
    arm      vmov.f64 d0, #1.5            08 0b b7 ee              same
    arm      vmov s0, r1                  10 1a 00 ee              same
    arm      vmov r0, r1, d2              12 0b 51 ec              same
    arm      vcmp.f64 d0, #0              40 0b b5 ee              same
    arm      vmrs APSR_nzcv, FPSCR        10 fa f1 ee              same
    arm      vcvt.f64.s32 d0, s1          e0 0b b8 ee              same
    arm      vldr d0, [r1, #8]            02 0b 91 ed              same
    arm      vstr s0, [r1]                00 0a 81 ed              same
    arm      vpush.64 {d8, d9}            04 8b 2d ed              same |}]

(* The text of a shape's sample: registers from 1, immediates [imm], [l] a label. *)
let operand_text imm i (k : Ops.Kind.t) =
  match k with
  | R -> Printf.sprintf "r%d" (1 + i)
  | D -> Printf.sprintf "d%d" (1 + i)
  | S -> Printf.sprintf "s%d" (1 + i)
  | Imm -> Printf.sprintf "#%d" imm
  | Fimm -> "#1."
  | Mem -> Printf.sprintf "[r%d]" (1 + i)
  | Sym -> "l"
  | Sh -> Printf.sprintf "r%d, lsl #2" (1 + i)
  | Reglist -> Printf.sprintf "{r%d, r%d}" (1 + i) (2 + i)
  | Dreglist -> Printf.sprintf "{d%d, d%d}" (1 + i) (2 + i)
  | Wb -> Printf.sprintf "r%d!" (1 + i)

(* The mnemonic with a condition: before a VFP type suffix, after anything else. *)
let with_cond name (cond : Arm_encode.Cond.t) =
  let c = Arm_encode.Cond.suffix cond in
  match String.index_opt name '.' with
  | Some d -> String.sub name 0 d ^ c ^ String.sub name d (String.length name - d)
  | None -> name ^ c

(* [text e s cond imm]: a program whose only instruction is the shape's sample. A label operand
   resolves to where the unbound encoding leaves zero: the word after the next, or an absolute 0. *)
let text (e : Ops.entry) (s : Ops.shape) cond imm =
  let kinds = s.Ops.kinds in
  let zero = e.Ops.op = Vcmp_d || e.Ops.op = Vcmp_s in
  let ops =
    match (e.Ops.op, kinds) with
    | (Movw | Movt), [ R; Sym ] ->
        [ "r1"; (if e.Ops.op = Movw then ":lower16:l" else ":upper16:l") ]
    | Vmrs, _ -> [ "APSR_nzcv"; "FPSCR" ]
    | _, [ D; Imm ] when zero -> [ "d1"; "#0" ]
    | _, [ S; Imm ] when zero -> [ "s1"; "#0" ]
    | _ -> List.mapi (operand_text imm) kinds
  in
  let insn = with_cond e.Ops.name cond ^ if ops = [] then "" else " " ^ String.concat ", " ops in
  if List.mem Ops.Kind.Sym kinds then insn ^ "\n\tnop\nl:" else insn

(* The builder's bytes against the text's: a label operand is left unresolved by the builder and
   resolved by the text, so the text is compared over the instruction's own bytes, and a
   [movw]/[movt] label without its immediate field. *)
let same (e : Ops.entry) insn t =
  match (H.encode insn, H.assemble t) with
  | Ok a, Ok b when String.length b >= String.length a -> (
      let b = String.sub b 0 (String.length a) in
      match e.Ops.op with
      | (Movw | Movt) when String.length a = 4 ->
          let word s = Int32.logand (String.get_int32_le s 0) 0xfff0f000l in
          Int32.equal (word a) (word b)
      | _ -> String.equal a b)
  | _ -> false

let all_shapes =
  List.concat_map (fun (e : Ops.entry) -> List.map (fun s -> (e, s)) e.Ops.shapes) Ops.all

let%expect_test "every builder encodes like its text under every condition" =
  let bad = ref [] and n = ref 0 in
  List.iter
    (fun ((e : Ops.entry), (s : Ops.shape)) ->
      let conds = if e.Ops.conditional then Arm_encode.Cond.all else [ Arm_encode.Cond.Al ] in
      List.iter
        (fun cond ->
          incr n;
          let imm = if e.Ops.op = Udf then 7 else 1 in
          let insn = s.Ops.sample ~cond imm in
          let t = text e s cond imm in
          if not (same e insn t) then
            bad :=
              Printf.sprintf "%s [%s]"
                (String.map (function '\n' | '\t' -> ' ' | c -> c) t)
                (match H.encode insn with Ok _ -> "assemble" | Error m -> m)
              :: !bad)
        conds)
    all_shapes;
  Printf.printf "%d builder x condition pairs, %d differ\n" !n (List.length !bad);
  List.iter print_endline (List.rev !bad);
  [%expect {|
    2130 builder x condition pairs, 0 differ |}]

let%expect_test "a flag-setting form differs from its plain form in the S bit alone" =
  let pairs =
    Arm_encode.Opcode.
      [ (Add, Adds); (Sub, Subs); (Orr, Orrs); (Adc, Adcs); (Sbc, Sbcs); (Rsb, Rsbs); (Rrx, Rrxs) ]
    @ List.init 4 (fun k -> (Arm_encode.Opcode.Shift k, Arm_encode.Opcode.Shifts k))
  in
  let bad = ref [] and n = ref 0 in
  List.iter
    (fun (plain, s_form) ->
      let ep = Ops.find plain and es = Ops.find s_form in
      List.iter
        (fun (s : Ops.shape) ->
          match List.find_opt (fun (p : Ops.shape) -> p.Ops.kinds = s.Ops.kinds) ep.Ops.shapes with
          | None -> ()
          | Some p -> (
              incr n;
              let word sh =
                Result.map
                  (fun b -> String.get_int32_le b 0)
                  (H.encode (sh.Ops.sample ~cond:Cond.Al 1))
              in
              match (word p, word s) with
              | Ok a, Ok b when Int32.logxor a b = 0x00100000l -> ()
              | _ -> bad := es.Ops.name :: !bad))
        es.Ops.shapes)
    pairs;
  Printf.printf "%d pairs, %d differ elsewhere\n" !n (List.length !bad);
  [%expect {| 13 pairs, 0 differ elsewhere |}]

let%expect_test "the bytes read back as text that assembles to them" =
  let bad = ref [] in
  List.iter
    (fun ((e : Ops.entry), (s : Ops.shape)) ->
      let insn = s.Ops.sample ~cond:Cond.Al 1 in
      match H.encode insn with
      | Error _ -> ()
      | Ok b -> (
          match H.decode b with
          | Error m -> bad := Printf.sprintf "%s: %s" e.Ops.name m :: !bad
          | Ok t -> (
              match H.assemble t with
              | Ok b' when String.equal b b' -> ()
              | _ -> bad := Printf.sprintf "%s: %s" e.Ops.name t :: !bad)))
    all_shapes;
  Printf.printf "%d do not\n" (List.length !bad);
  List.iter print_endline (List.sort_uniq compare (List.rev !bad));
  [%expect {|
    0 do not |}]
