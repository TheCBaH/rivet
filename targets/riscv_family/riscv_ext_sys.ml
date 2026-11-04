(* CSR access, fences, privileged, cache-management, hint and control-flow-integrity extensions.
   Most have only generated rows. *)

module Opcode = Riscv_types.Opcode

let zicsr =
  Riscv_ext.component ~id:"riscv.zicsr" ~feature:"zicsr"
    ~summary:"control and status register access"
    [
      (Opcode.Csrrw, "rv_zicsr/csrrw");
      (Opcode.Csrrs, "rv_zicsr/csrrs");
      (Opcode.Csrrc, "rv_zicsr/csrrc");
      (Opcode.Csrrwi, "rv_zicsr/csrrwi");
      (Opcode.Csrrsi, "rv_zicsr/csrrsi");
      (Opcode.Csrrci, "rv_zicsr/csrrci");
      (Opcode.Csrr, "rv_zicsr/csrr");
      (Opcode.Csrw, "rv_zicsr/csrw");
      (Opcode.Csrs, "rv_zicsr/csrs");
      (Opcode.Csrc, "rv_zicsr/csrc");
      (Opcode.Csrwi, "rv_zicsr/csrwi");
      (Opcode.Csrsi, "rv_zicsr/csrsi");
      (Opcode.Csrci, "rv_zicsr/csrci");
    ]

let zifencei =
  Riscv_ext.component ~id:"riscv.zifencei" ~feature:"zifencei" ~summary:"instruction-fetch fence"
    [ (Opcode.Fence_i, "rv_zifencei/fence.i") ]

let table ?(requires = []) ?legal feature summary =
  Riscv_ext.component ~id:("riscv." ^ feature) ~feature ~requires ?legal ~summary []

(* The compressed spellings of a hint or CFI instruction also need the compressed extension that
   carries them. *)
let compressed_too also m =
  if String.starts_with ~prefix:"c." m then
    Some (Target_component.Req.All [ Feature also.(0); Feature also.(1) ])
  else None

let zicntr = table ~requires:[ "zicsr" ] "zicntr" "base counters and timers"

let zihintntl =
  table "zihintntl" "non-temporal locality hints" ~legal:(compressed_too [| "zihintntl"; "zca" |])

let zawrs = table "zawrs" "wait-on-reservation-set"
let zimop = table "zimop" "may-be-operations"
let zicfiss = table "zicfiss" "shadow stack" ~legal:(compressed_too [| "zicfiss"; "zcmop" |])
let zicfilp = table "zicfilp" "landing pads"
let zicbom = table "zicbom" "cache-block management"
let zicboz = table "zicboz" "cache-block zero"
let zicbop = table "zicbop" "cache-block prefetch"
let system = table "system" "machine-mode return and wait-for-interrupt"
let s = table "s" "supervisor-mode return and address-translation fence"
let h = table ~requires:[ "s" ] "h" "hypervisor"
let svinval = table "svinval" "fine-grained address-translation cache invalidation"
let sdext = table "sdext" "debug-mode return"
let ssctr = table "ssctr" "control transfer records"
