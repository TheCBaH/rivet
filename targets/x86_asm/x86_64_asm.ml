(* The x86-64 builders: the registers this mode has, by name and by class, and the instruction
   builders that exist in it. *)

open X86_family_encode
include X86_asm

module Gpr64 = Regs (struct
  type t = w64 gpr

  let cls = Rclass.Gpr64
  let registers = base_registers X86_64_encode.Mode.registers
  let make = View.Unsafe.gpr
  let reg r = (r : t :> Reg.t)
end)

let rax = Gpr64.of_int_exn 0
let rcx = Gpr64.of_int_exn 1
let rdx = Gpr64.of_int_exn 2
let rbx = Gpr64.of_int_exn 3
let rsp = Gpr64.of_int_exn 4
let rbp = Gpr64.of_int_exn 5
let rsi = Gpr64.of_int_exn 6
let rdi = Gpr64.of_int_exn 7
let r8 = Gpr64.of_int_exn 8
let r9 = Gpr64.of_int_exn 9
let r10 = Gpr64.of_int_exn 10
let r11 = Gpr64.of_int_exn 11
let r12 = Gpr64.of_int_exn 12
let r13 = Gpr64.of_int_exn 13
let r14 = Gpr64.of_int_exn 14
let r15 = Gpr64.of_int_exn 15

module Gpr32 = Regs (struct
  type t = w32 gpr

  let cls = Rclass.Gpr32
  let registers = base_registers X86_64_encode.Mode.registers
  let make = View.Unsafe.gpr
  let reg r = (r : t :> Reg.t)
end)

let eax = Gpr32.of_int_exn 0
let ecx = Gpr32.of_int_exn 1
let edx = Gpr32.of_int_exn 2
let ebx = Gpr32.of_int_exn 3
let esp = Gpr32.of_int_exn 4
let ebp = Gpr32.of_int_exn 5
let esi = Gpr32.of_int_exn 6
let edi = Gpr32.of_int_exn 7
let r8d = Gpr32.of_int_exn 8
let r9d = Gpr32.of_int_exn 9
let r10d = Gpr32.of_int_exn 10
let r11d = Gpr32.of_int_exn 11
let r12d = Gpr32.of_int_exn 12
let r13d = Gpr32.of_int_exn 13
let r14d = Gpr32.of_int_exn 14
let r15d = Gpr32.of_int_exn 15

module Gpr16 = Regs (struct
  type t = w16 gpr

  let cls = Rclass.Gpr16
  let registers = base_registers X86_64_encode.Mode.registers
  let make = View.Unsafe.gpr
  let reg r = (r : t :> Reg.t)
end)

let ax = Gpr16.of_int_exn 0
let cx = Gpr16.of_int_exn 1
let dx = Gpr16.of_int_exn 2
let bx = Gpr16.of_int_exn 3
let sp = Gpr16.of_int_exn 4
let bp = Gpr16.of_int_exn 5
let si = Gpr16.of_int_exn 6
let di = Gpr16.of_int_exn 7
let r8w = Gpr16.of_int_exn 8
let r9w = Gpr16.of_int_exn 9
let r10w = Gpr16.of_int_exn 10
let r11w = Gpr16.of_int_exn 11
let r12w = Gpr16.of_int_exn 12
let r13w = Gpr16.of_int_exn 13
let r14w = Gpr16.of_int_exn 14
let r15w = Gpr16.of_int_exn 15

module Gpr8 = Regs (struct
  type t = w8 gpr

  let cls = Rclass.Gpr8
  let registers = base_registers X86_64_encode.Mode.registers
  let make = View.Unsafe.gpr
  let reg r = (r : t :> Reg.t)
end)

let al = Gpr8.of_int_exn 0
let cl = Gpr8.of_int_exn 1
let dl = Gpr8.of_int_exn 2
let bl = Gpr8.of_int_exn 3
let spl = Gpr8.of_int_exn 4
let bpl = Gpr8.of_int_exn 5
let sil = Gpr8.of_int_exn 6
let dil = Gpr8.of_int_exn 7
let r8b = Gpr8.of_int_exn 8
let r9b = Gpr8.of_int_exn 9
let r10b = Gpr8.of_int_exn 10
let r11b = Gpr8.of_int_exn 11
let r12b = Gpr8.of_int_exn 12
let r13b = Gpr8.of_int_exn 13
let r14b = Gpr8.of_int_exn 14
let r15b = Gpr8.of_int_exn 15

module Xmm = Regs (struct
  type t = xmm

  let cls = Rclass.Xmm
  let registers = base_registers X86_64_encode.Mode.registers
  let make = View.Unsafe.vec
  let reg r = (r : t :> Reg.t)
end)

let xmm0 = Xmm.of_int_exn 0
let xmm1 = Xmm.of_int_exn 1
let xmm2 = Xmm.of_int_exn 2
let xmm3 = Xmm.of_int_exn 3
let xmm4 = Xmm.of_int_exn 4
let xmm5 = Xmm.of_int_exn 5
let xmm6 = Xmm.of_int_exn 6
let xmm7 = Xmm.of_int_exn 7
let xmm8 = Xmm.of_int_exn 8
let xmm9 = Xmm.of_int_exn 9
let xmm10 = Xmm.of_int_exn 10
let xmm11 = Xmm.of_int_exn 11
let xmm12 = Xmm.of_int_exn 12
let xmm13 = Xmm.of_int_exn 13
let xmm14 = Xmm.of_int_exn 14
let xmm15 = Xmm.of_int_exn 15

module Ymm = Regs (struct
  type t = ymm

  let cls = Rclass.Ymm
  let registers = base_registers X86_64_encode.Mode.registers
  let make = View.Unsafe.vec
  let reg r = (r : t :> Reg.t)
end)

let ymm0 = Ymm.of_int_exn 0
let ymm1 = Ymm.of_int_exn 1
let ymm2 = Ymm.of_int_exn 2
let ymm3 = Ymm.of_int_exn 3
let ymm4 = Ymm.of_int_exn 4
let ymm5 = Ymm.of_int_exn 5
let ymm6 = Ymm.of_int_exn 6
let ymm7 = Ymm.of_int_exn 7
let ymm8 = Ymm.of_int_exn 8
let ymm9 = Ymm.of_int_exn 9
let ymm10 = Ymm.of_int_exn 10
let ymm11 = Ymm.of_int_exn 11
let ymm12 = Ymm.of_int_exn 12
let ymm13 = Ymm.of_int_exn 13
let ymm14 = Ymm.of_int_exn 14
let ymm15 = Ymm.of_int_exn 15

module Zmm = Regs (struct
  type t = zmm

  let cls = Rclass.Zmm
  let registers = base_registers X86_64_encode.Mode.registers
  let make = View.Unsafe.vec
  let reg r = (r : t :> Reg.t)
end)

let zmm0 = Zmm.of_int_exn 0
let zmm1 = Zmm.of_int_exn 1
let zmm2 = Zmm.of_int_exn 2
let zmm3 = Zmm.of_int_exn 3
let zmm4 = Zmm.of_int_exn 4
let zmm5 = Zmm.of_int_exn 5
let zmm6 = Zmm.of_int_exn 6
let zmm7 = Zmm.of_int_exn 7
let zmm8 = Zmm.of_int_exn 8
let zmm9 = Zmm.of_int_exn 9
let zmm10 = Zmm.of_int_exn 10
let zmm11 = Zmm.of_int_exn 11
let zmm12 = Zmm.of_int_exn 12
let zmm13 = Zmm.of_int_exn 13
let zmm14 = Zmm.of_int_exn 14
let zmm15 = Zmm.of_int_exn 15

module Gpr64_apx = Regs (struct
  type t = w64 agpr

  let cls = Rclass.Gpr64
  let registers = X86_64_encode.Mode.registers
  let make = View.Unsafe.agpr
  let reg r = (r : t :> Reg.t)
end)

let r16 = Gpr64_apx.of_int_exn 16
let r17 = Gpr64_apx.of_int_exn 17
let r18 = Gpr64_apx.of_int_exn 18
let r19 = Gpr64_apx.of_int_exn 19
let r20 = Gpr64_apx.of_int_exn 20
let r21 = Gpr64_apx.of_int_exn 21
let r22 = Gpr64_apx.of_int_exn 22
let r23 = Gpr64_apx.of_int_exn 23
let r24 = Gpr64_apx.of_int_exn 24
let r25 = Gpr64_apx.of_int_exn 25
let r26 = Gpr64_apx.of_int_exn 26
let r27 = Gpr64_apx.of_int_exn 27
let r28 = Gpr64_apx.of_int_exn 28
let r29 = Gpr64_apx.of_int_exn 29
let r30 = Gpr64_apx.of_int_exn 30
let r31 = Gpr64_apx.of_int_exn 31

module Gpr32_apx = Regs (struct
  type t = w32 agpr

  let cls = Rclass.Gpr32
  let registers = X86_64_encode.Mode.registers
  let make = View.Unsafe.agpr
  let reg r = (r : t :> Reg.t)
end)

let r16d = Gpr32_apx.of_int_exn 16
let r17d = Gpr32_apx.of_int_exn 17
let r18d = Gpr32_apx.of_int_exn 18
let r19d = Gpr32_apx.of_int_exn 19
let r20d = Gpr32_apx.of_int_exn 20
let r21d = Gpr32_apx.of_int_exn 21
let r22d = Gpr32_apx.of_int_exn 22
let r23d = Gpr32_apx.of_int_exn 23
let r24d = Gpr32_apx.of_int_exn 24
let r25d = Gpr32_apx.of_int_exn 25
let r26d = Gpr32_apx.of_int_exn 26
let r27d = Gpr32_apx.of_int_exn 27
let r28d = Gpr32_apx.of_int_exn 28
let r29d = Gpr32_apx.of_int_exn 29
let r30d = Gpr32_apx.of_int_exn 30
let r31d = Gpr32_apx.of_int_exn 31

module Gpr16_apx = Regs (struct
  type t = w16 agpr

  let cls = Rclass.Gpr16
  let registers = X86_64_encode.Mode.registers
  let make = View.Unsafe.agpr
  let reg r = (r : t :> Reg.t)
end)

let r16w = Gpr16_apx.of_int_exn 16
let r17w = Gpr16_apx.of_int_exn 17
let r18w = Gpr16_apx.of_int_exn 18
let r19w = Gpr16_apx.of_int_exn 19
let r20w = Gpr16_apx.of_int_exn 20
let r21w = Gpr16_apx.of_int_exn 21
let r22w = Gpr16_apx.of_int_exn 22
let r23w = Gpr16_apx.of_int_exn 23
let r24w = Gpr16_apx.of_int_exn 24
let r25w = Gpr16_apx.of_int_exn 25
let r26w = Gpr16_apx.of_int_exn 26
let r27w = Gpr16_apx.of_int_exn 27
let r28w = Gpr16_apx.of_int_exn 28
let r29w = Gpr16_apx.of_int_exn 29
let r30w = Gpr16_apx.of_int_exn 30
let r31w = Gpr16_apx.of_int_exn 31

module Gpr8_apx = Regs (struct
  type t = w8 agpr

  let cls = Rclass.Gpr8
  let registers = X86_64_encode.Mode.registers
  let make = View.Unsafe.agpr
  let reg r = (r : t :> Reg.t)
end)

let r16b = Gpr8_apx.of_int_exn 16
let r17b = Gpr8_apx.of_int_exn 17
let r18b = Gpr8_apx.of_int_exn 18
let r19b = Gpr8_apx.of_int_exn 19
let r20b = Gpr8_apx.of_int_exn 20
let r21b = Gpr8_apx.of_int_exn 21
let r22b = Gpr8_apx.of_int_exn 22
let r23b = Gpr8_apx.of_int_exn 23
let r24b = Gpr8_apx.of_int_exn 24
let r25b = Gpr8_apx.of_int_exn 25
let r26b = Gpr8_apx.of_int_exn 26
let r27b = Gpr8_apx.of_int_exn 27
let r28b = Gpr8_apx.of_int_exn 28
let r29b = Gpr8_apx.of_int_exn 29
let r30b = Gpr8_apx.of_int_exn 30
let r31b = Gpr8_apx.of_int_exn 31

module Xmm_evex = Regs (struct
  type t = l128 evec

  let cls = Rclass.Xmm
  let registers = X86_64_encode.Mode.registers
  let make = View.Unsafe.evec
  let reg r = (r : t :> Reg.t)
end)

let xmm16 = Xmm_evex.of_int_exn 16
let xmm17 = Xmm_evex.of_int_exn 17
let xmm18 = Xmm_evex.of_int_exn 18
let xmm19 = Xmm_evex.of_int_exn 19
let xmm20 = Xmm_evex.of_int_exn 20
let xmm21 = Xmm_evex.of_int_exn 21
let xmm22 = Xmm_evex.of_int_exn 22
let xmm23 = Xmm_evex.of_int_exn 23
let xmm24 = Xmm_evex.of_int_exn 24
let xmm25 = Xmm_evex.of_int_exn 25
let xmm26 = Xmm_evex.of_int_exn 26
let xmm27 = Xmm_evex.of_int_exn 27
let xmm28 = Xmm_evex.of_int_exn 28
let xmm29 = Xmm_evex.of_int_exn 29
let xmm30 = Xmm_evex.of_int_exn 30
let xmm31 = Xmm_evex.of_int_exn 31

module Ymm_evex = Regs (struct
  type t = l256 evec

  let cls = Rclass.Ymm
  let registers = X86_64_encode.Mode.registers
  let make = View.Unsafe.evec
  let reg r = (r : t :> Reg.t)
end)

let ymm16 = Ymm_evex.of_int_exn 16
let ymm17 = Ymm_evex.of_int_exn 17
let ymm18 = Ymm_evex.of_int_exn 18
let ymm19 = Ymm_evex.of_int_exn 19
let ymm20 = Ymm_evex.of_int_exn 20
let ymm21 = Ymm_evex.of_int_exn 21
let ymm22 = Ymm_evex.of_int_exn 22
let ymm23 = Ymm_evex.of_int_exn 23
let ymm24 = Ymm_evex.of_int_exn 24
let ymm25 = Ymm_evex.of_int_exn 25
let ymm26 = Ymm_evex.of_int_exn 26
let ymm27 = Ymm_evex.of_int_exn 27
let ymm28 = Ymm_evex.of_int_exn 28
let ymm29 = Ymm_evex.of_int_exn 29
let ymm30 = Ymm_evex.of_int_exn 30
let ymm31 = Ymm_evex.of_int_exn 31

module Zmm_evex = Regs (struct
  type t = l512 evec

  let cls = Rclass.Zmm
  let registers = X86_64_encode.Mode.registers
  let make = View.Unsafe.evec
  let reg r = (r : t :> Reg.t)
end)

let zmm16 = Zmm_evex.of_int_exn 16
let zmm17 = Zmm_evex.of_int_exn 17
let zmm18 = Zmm_evex.of_int_exn 18
let zmm19 = Zmm_evex.of_int_exn 19
let zmm20 = Zmm_evex.of_int_exn 20
let zmm21 = Zmm_evex.of_int_exn 21
let zmm22 = Zmm_evex.of_int_exn 22
let zmm23 = Zmm_evex.of_int_exn 23
let zmm24 = Zmm_evex.of_int_exn 24
let zmm25 = Zmm_evex.of_int_exn 25
let zmm26 = Zmm_evex.of_int_exn 26
let zmm27 = Zmm_evex.of_int_exn 27
let zmm28 = Zmm_evex.of_int_exn 28
let zmm29 = Zmm_evex.of_int_exn 29
let zmm30 = Zmm_evex.of_int_exn 30
let zmm31 = Zmm_evex.of_int_exn 31

module Tmm = Regs (struct
  type t = tmm

  let cls = Rclass.Tmm
  let registers = X86_64_encode.Mode.registers
  let make = View.Unsafe.tmm
  let reg r = (r : t :> Reg.t)
end)

let tmm0 = Tmm.of_int_exn 0
let tmm1 = Tmm.of_int_exn 1
let tmm2 = Tmm.of_int_exn 2
let tmm3 = Tmm.of_int_exn 3
let tmm4 = Tmm.of_int_exn 4
let tmm5 = Tmm.of_int_exn 5
let tmm6 = Tmm.of_int_exn 6
let tmm7 = Tmm.of_int_exn 7

module Creg = Regs (struct
  type t = creg

  let cls = Rclass.Cr
  let registers = X86_64_encode.Mode.registers
  let make = View.Unsafe.creg
  let reg r = (r : t :> Reg.t)
end)

let cr0 = Creg.of_int_exn 0
let cr1 = Creg.of_int_exn 1
let cr2 = Creg.of_int_exn 2
let cr3 = Creg.of_int_exn 3
let cr4 = Creg.of_int_exn 4
let cr5 = Creg.of_int_exn 5
let cr6 = Creg.of_int_exn 6
let cr7 = Creg.of_int_exn 7
let cr8 = Creg.of_int_exn 8
let cr9 = Creg.of_int_exn 9
let cr10 = Creg.of_int_exn 10
let cr11 = Creg.of_int_exn 11
let cr12 = Creg.of_int_exn 12
let cr13 = Creg.of_int_exn 13
let cr14 = Creg.of_int_exn 14
let cr15 = Creg.of_int_exn 15

module Dreg = Regs (struct
  type t = dreg

  let cls = Rclass.Dr
  let registers = X86_64_encode.Mode.registers
  let make = View.Unsafe.dreg
  let reg r = (r : t :> Reg.t)
end)

let dr0 = Dreg.of_int_exn 0
let dr1 = Dreg.of_int_exn 1
let dr2 = Dreg.of_int_exn 2
let dr3 = Dreg.of_int_exn 3
let dr4 = Dreg.of_int_exn 4
let dr5 = Dreg.of_int_exn 5
let dr6 = Dreg.of_int_exn 6
let dr7 = Dreg.of_int_exn 7

module Mmx = Regs (struct
  type t = mmx

  let cls = Rclass.Mmx
  let registers = base_registers X86_64_encode.Mode.registers
  let make = View.Unsafe.mmx
  let reg r = (r : t :> Reg.t)
end)

let mm0 = Mmx.of_int_exn 0
let mm1 = Mmx.of_int_exn 1
let mm2 = Mmx.of_int_exn 2
let mm3 = Mmx.of_int_exn 3
let mm4 = Mmx.of_int_exn 4
let mm5 = Mmx.of_int_exn 5
let mm6 = Mmx.of_int_exn 6
let mm7 = Mmx.of_int_exn 7

module Kmask = Regs (struct
  type t = kmask

  let cls = Rclass.Kmask
  let registers = base_registers X86_64_encode.Mode.registers
  let make = View.Unsafe.kmask
  let reg r = (r : t :> Reg.t)
end)

let k0 = Kmask.of_int_exn 0
let k1 = Kmask.of_int_exn 1
let k2 = Kmask.of_int_exn 2
let k3 = Kmask.of_int_exn 3
let k4 = Kmask.of_int_exn 4
let k5 = Kmask.of_int_exn 5
let k6 = Kmask.of_int_exn 6
let k7 = Kmask.of_int_exn 7

module St = Regs (struct
  type t = st

  let cls = Rclass.St
  let registers = st_registers
  let make = View.Unsafe.st
  let reg r = (r : t :> Reg.t)
end)

let st = St.of_int_exn 0
let st1 = St.of_int_exn 1
let st2 = St.of_int_exn 2
let st3 = St.of_int_exn 3
let st4 = St.of_int_exn 4
let st5 = St.of_int_exn 5
let st6 = St.of_int_exn 6
let st7 = St.of_int_exn 7

(* [disp(%rip)]: RIP-relative. *)
let rip ?(disp = 0L) () =
  { Mem.base = Some Reg.rip; index = None; scale = 1; disp = Disp.Const disp }

let rip_e disp = { (rip ()) with disp = Disp.Sym disp }
