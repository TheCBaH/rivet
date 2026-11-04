(* [Instruction.mk] values against their text on all six targets. *)

open Harness
module Rv32 = Make (Riscv32_encode)
module Rv64 = Make (Riscv64_encode)
module X32 = Make (X86_32_encode)
module X64 = Make (X86_64_encode)
module Arm = Make (Arm_encode)
module A64 = Make (Aarch64_encode)

let int n = Foundation.Bigint.of_int n

let%expect_test "Instruction.mk values encode like their text" =
  (let module E = Riscv32_encode in
  let x n = E.Operand.Reg (E.Reg.x n) in
  let mem n base =
    E.Operand.Mem { E.Mem.offset = Asm_core.Expr.Const (int n); base = E.Reg.x base }
  in
  Rv32.check (E.Instruction.mk E.Opcode.Add [ x 1; x 2; x 3 ]) "add x1, x2, x3";
  Rv32.check (E.Instruction.mk E.Opcode.Lw [ x 10; mem 8 2 ]) "lw a0, 8(sp)";
  Rv32.check
    (E.Instruction.mk E.Opcode.Addi [ x 5; x 6; E.Operand.Imm (int (-3)) ])
    "addi t0, t1, -3");
  (let module E = Riscv64_encode in
  let x n = E.Operand.Reg (E.Reg.x n) in
  Rv64.check (E.Instruction.mk E.Opcode.Addw [ x 1; x 2; x 3 ]) "addw x1, x2, x3");
  (let module E = X86_32_encode in
  let r w n = E.Operand.Reg (E.reg_at ~width:w n) in
  X32.check (E.Instruction.mk E.Opcode.Add 32 [ r 32 0; r 32 3 ]) "addl %eax, %ebx");
  (let module E = X86_64_encode in
  let r w n = E.Operand.Reg (E.reg_at ~width:w n) in
  X64.check (E.Instruction.mk E.Opcode.Add 64 [ r 64 0; r 64 3 ]) "addq %rax, %rbx";
  X64.check (E.Instruction.mk E.Opcode.Mov 32 [ E.Operand.Imm (int 5); r 32 1 ]) "movl $5, %ecx");
  (let module E = Arm_encode in
  let r n = E.Operand.Reg (E.Reg.of_num n) in
  Arm.check (E.Instruction.mk E.Opcode.Add [ r 1; r 2; r 3 ]) "add r1, r2, r3";
  Arm.check (E.Instruction.mk E.Opcode.Mov [ r 0; E.Operand.Imm (int 7) ]) "mov r0, #7");
  (let module E = Aarch64_encode in
  let x name = E.Operand.Reg (Option.get (E.Reg.find name)) in
  A64.check (E.Instruction.mk E.Opcode.Add [ x "x1"; x "x2"; x "x3" ]) "add x1, x2, x3";
  A64.check
    (E.Instruction.mk E.Opcode.Sub [ x "w0"; x "w1"; E.Operand.Imm (int 4) ])
    "sub w0, w1, #4");
  [%expect
    {|
    riscv32  add x1, x2, x3               b3 00 31 00              same
    riscv32  lw a0, 8(sp)                 03 25 81 00              same
    riscv32  addi t0, t1, -3              93 02 d3 ff              same
    riscv64  addw x1, x2, x3              bb 00 31 00              same
    x86_32   addl %eax, %ebx              01 c3                    same
    x86_64   addq %rax, %rbx              48 01 c3                 same
    x86_64   movl $5, %ecx                b9 05 00 00 00           same
    arm      add r1, r2, r3               03 10 82 e0              same
    arm      mov r0, #7                   07 00 a0 e3              same
    aarch64  add x1, x2, x3               41 00 03 8b              same
    aarch64  sub w0, w1, #4               20 10 00 51              same |}]

let%expect_test "decoded bytes read back as their text" =
  List.iter
    (fun (target, text) ->
      match assemble target text with
      | Error m -> Printf.printf "%s: %s\n" target m
      | Ok b ->
          Printf.printf "%-8s %-20s %s\n" target text
            (match decode target b with Ok t -> t | Error m -> "REFUSED " ^ m))
    [
      ("riscv64", "add x1, x2, x3");
      ("x86_64", "addq %rax, %rbx");
      ("arm", "add r1, r2, r3");
      ("aarch64", "add x1, x2, x3");
    ];
  [%expect
    {|
    riscv64  add x1, x2, x3       add x1, x2, x3
    x86_64   addq %rax, %rbx      addq %rax, %rbx
    arm      add r1, r2, r3       add r1, r2, r3
    aarch64  add x1, x2, x3       add x1, x2, x3 |}]
