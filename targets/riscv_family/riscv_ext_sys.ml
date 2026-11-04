(* CSR access, fences, privileged, cache-management, hint and control-flow-integrity extensions.
   Most have only generated rows. *)

let zicsr =
  Riscv_ext.component ~id:"riscv.zicsr" ~feature:"zicsr"
    ~summary:"control and status register access" (Riscv_ops.forms "zicsr")

let zifencei =
  Riscv_ext.component ~id:"riscv.zifencei" ~feature:"zifencei" ~summary:"instruction-fetch fence"
    (Riscv_ops.forms "zifencei")

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
