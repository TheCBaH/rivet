(* CSR access, fences, privileged, cache-management, hint and control-flow-integrity extensions.
   Most have only generated rows. *)

let zicsr =
  Riscv_ext.component ~id:"riscv.zicsr" ~feature:"zicsr"
    ~summary:"control and status register access"
    [
      ("csrrw", "rv_zicsr/csrrw");
      ("csrrs", "rv_zicsr/csrrs");
      ("csrrc", "rv_zicsr/csrrc");
      ("csrrwi", "rv_zicsr/csrrwi");
      ("csrrsi", "rv_zicsr/csrrsi");
      ("csrrci", "rv_zicsr/csrrci");
      ("csrr", "rv_zicsr/csrr");
      ("csrw", "rv_zicsr/csrw");
      ("csrs", "rv_zicsr/csrs");
      ("csrc", "rv_zicsr/csrc");
      ("csrwi", "rv_zicsr/csrwi");
      ("csrsi", "rv_zicsr/csrsi");
      ("csrci", "rv_zicsr/csrci");
    ]

let zifencei =
  Riscv_ext.component ~id:"riscv.zifencei" ~feature:"zifencei" ~summary:"instruction-fetch fence"
    [ ("fence.i", "rv_zifencei/fence.i") ]

let table ?(requires = []) feature summary =
  Riscv_ext.component ~id:("riscv." ^ feature) ~feature ~requires ~summary []

let zicntr = table ~requires:[ "zicsr" ] "zicntr" "base counters and timers"
let zihintntl = table "zihintntl" "non-temporal locality hints"
let zawrs = table "zawrs" "wait-on-reservation-set"
let zimop = table "zimop" "may-be-operations"
let zicfiss = table "zicfiss" "shadow stack"
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
