(* The x86 builder libraries: each mode's register classes keep the REGS laws, and their names
   are the parser's spellings of exactly the registers that mode has (below 16: the upper ones
   have their own views). *)

open X86_family_encode

module type REGS = Target_encode.REGS

let check (type t) mode registers (module R : REGS with type t = t) to_reg cls =
  let range = List.filter R.mem (List.init 66 (fun n -> n - 1)) in
  let laws =
    range = R.numbers
    && List.for_all
         (fun n -> Option.is_some (R.of_int n) = List.mem n R.numbers)
         (List.init 66 (fun n -> n - 1))
    && List.for_all (fun n -> R.to_int (R.of_int_exn n) = n) R.numbers
    && R.count = List.length R.numbers
    && List.map R.to_int R.all = R.numbers
    &&
    match R.range with
    | Some (lo, hi) -> hi - lo + 1 = R.count && List.hd R.numbers = lo
    | None -> R.count = 0 || List.nth R.numbers (R.count - 1) - List.hd R.numbers + 1 <> R.count
  in
  (* every value is the register its spelling reads as, and every register of the class has one *)
  let read r =
    let n = R.name r in
    if cls = Rclass.St then true else Reg.find_in registers n = Some (to_reg r)
  in
  let covered =
    List.for_all
      (fun (r : Reg.t) -> r.cls <> cls || List.exists (fun v -> Reg.equal (to_reg v) r) R.all)
      (X86_asm.base_registers registers)
  in
  Printf.printf "%s %-9s %2d [%s .. %s] laws %b parser %b\n" mode
    (Reg.name { Reg.cls; num = List.hd R.numbers })
    R.count
    (R.name (List.hd R.all))
    (R.name (List.nth R.all (R.count - 1)))
    laws
    (List.for_all read R.all && covered)

let%expect_test "x86_32 registers" =
  let regs = X86_32_encode.Mode.registers in
  let open X86_32_asm in
  check "x86_32" regs (module Gpr32) (fun r -> (r :> Reg.t)) Gpr32;
  check "x86_32" regs (module Gpr16) (fun r -> (r :> Reg.t)) Gpr16;
  check "x86_32" regs (module Gpr8) (fun r -> (r :> Reg.t)) Gpr8;
  check "x86_32" regs (module Gpr8_high) (fun r -> (r :> Reg.t)) Gpr8_high;
  check "x86_32" regs (module Xmm) (fun r -> (r :> Reg.t)) Xmm;
  check "x86_32" regs (module Ymm) (fun r -> (r :> Reg.t)) Ymm;
  check "x86_32" regs (module Zmm) (fun r -> (r :> Reg.t)) Zmm;
  check "x86_32" regs (module Mmx) (fun r -> (r :> Reg.t)) Mmx;
  check "x86_32" regs (module Kmask) (fun r -> (r :> Reg.t)) Kmask;
  check "x86_32" st_registers (module St) (fun r -> (r :> Reg.t)) St;
  [%expect
    {|
    x86_32 eax        8 [eax .. edi] laws true parser true
    x86_32 ax         8 [ax .. di] laws true parser true
    x86_32 al         4 [al .. bl] laws true parser true
    x86_32 ah         4 [ah .. bh] laws true parser true
    x86_32 xmm0       8 [xmm0 .. xmm7] laws true parser true
    x86_32 ymm0       8 [ymm0 .. ymm7] laws true parser true
    x86_32 zmm0       8 [zmm0 .. zmm7] laws true parser true
    x86_32 mm0        8 [mm0 .. mm7] laws true parser true
    x86_32 k0         8 [k0 .. k7] laws true parser true
    x86_32 st         8 [st .. st(7)] laws true parser true |}]

let%expect_test "x86_64 registers" =
  let regs = X86_64_encode.Mode.registers in
  let open X86_64_asm in
  check "x86_64" regs (module Gpr64) (fun r -> (r :> Reg.t)) Gpr64;
  check "x86_64" regs (module Gpr32) (fun r -> (r :> Reg.t)) Gpr32;
  check "x86_64" regs (module Gpr16) (fun r -> (r :> Reg.t)) Gpr16;
  check "x86_64" regs (module Gpr8) (fun r -> (r :> Reg.t)) Gpr8;
  check "x86_64" regs (module Xmm) (fun r -> (r :> Reg.t)) Xmm;
  check "x86_64" regs (module Ymm) (fun r -> (r :> Reg.t)) Ymm;
  check "x86_64" regs (module Zmm) (fun r -> (r :> Reg.t)) Zmm;
  check "x86_64" regs (module Mmx) (fun r -> (r :> Reg.t)) Mmx;
  check "x86_64" regs (module Kmask) (fun r -> (r :> Reg.t)) Kmask;
  check "x86_64" st_registers (module St) (fun r -> (r :> Reg.t)) St;
  [%expect
    {|
    x86_64 rax       16 [rax .. r15] laws true parser true
    x86_64 eax       16 [eax .. r15d] laws true parser true
    x86_64 ax        16 [ax .. r15w] laws true parser true
    x86_64 al        16 [al .. r15b] laws true parser true
    x86_64 xmm0      16 [xmm0 .. xmm15] laws true parser true
    x86_64 ymm0      16 [ymm0 .. ymm15] laws true parser true
    x86_64 zmm0      16 [zmm0 .. zmm15] laws true parser true
    x86_64 mm0        8 [mm0 .. mm7] laws true parser true
    x86_64 k0         8 [k0 .. k7] laws true parser true
    x86_64 st         8 [st .. st(7)] laws true parser true |}]
