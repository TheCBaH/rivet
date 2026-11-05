(* The x86-32 builders: the registers this mode has, by name and by class, and the instruction
   builders that exist in it. *)

open X86_family_encode
include X86_asm

module Gpr32 = Regs (struct
  type t = w32 gpr

  let cls = Rclass.Gpr32
  let registers = base_registers X86_32_encode.Mode.registers
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

module Gpr16 = Regs (struct
  type t = w16 gpr

  let cls = Rclass.Gpr16
  let registers = base_registers X86_32_encode.Mode.registers
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

module Gpr8 = Regs (struct
  type t = w8 gpr

  let cls = Rclass.Gpr8
  let registers = base_registers X86_32_encode.Mode.registers
  let make = View.Unsafe.gpr
  let reg r = (r : t :> Reg.t)
end)

let al = Gpr8.of_int_exn 0
let cl = Gpr8.of_int_exn 1
let dl = Gpr8.of_int_exn 2
let bl = Gpr8.of_int_exn 3

module Gpr8_high = Regs (struct
  type t = w8 gpr

  let cls = Rclass.Gpr8_high
  let registers = base_registers X86_32_encode.Mode.registers
  let make = View.Unsafe.gpr
  let reg r = (r : t :> Reg.t)
end)

let ah = Gpr8_high.of_int_exn 4
let ch = Gpr8_high.of_int_exn 5
let dh = Gpr8_high.of_int_exn 6
let bh = Gpr8_high.of_int_exn 7

module Xmm = Regs (struct
  type t = xmm

  let cls = Rclass.Xmm
  let registers = base_registers X86_32_encode.Mode.registers
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

module Ymm = Regs (struct
  type t = ymm

  let cls = Rclass.Ymm
  let registers = base_registers X86_32_encode.Mode.registers
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

module Zmm = Regs (struct
  type t = zmm

  let cls = Rclass.Zmm
  let registers = base_registers X86_32_encode.Mode.registers
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

module Tmm = Regs (struct
  type t = tmm

  let cls = Rclass.Tmm
  let registers = X86_32_encode.Mode.registers
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
  let registers = X86_32_encode.Mode.registers
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
  let registers = X86_32_encode.Mode.registers
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
  let registers = base_registers X86_32_encode.Mode.registers
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
  let registers = base_registers X86_32_encode.Mode.registers
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
