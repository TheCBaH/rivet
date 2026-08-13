(* The target registry (.ai/asm_plan.md §3.7, §5.1).

   A pure value, not a mutable table populated by module-initialisation side
   effects. A registry that depended on link order would not survive being
   compiled three ways: js_of_ocaml and Melange are free to order module
   initialisation differently from the native linker, and "the target list is
   empty under Node" is exactly the class of failure the three-build equality
   requirement exists to catch.

   This is the only package that may name all four targets at once, and the only
   place an existential target type is introduced. Everything downstream - the
   CLI, and any architecture-erased caller - sees [DRIVER] and never a
   target-specific type. *)

module T_intf = Target_intf.Target

(* Functor application happens once, here, at module level. The four results are
   ordinary modules; packing them into first-class module values below is what
   erases their types. *)
module X86_32 = Pipeline.Make (X86_32)
module X86_64 = Pipeline.Make (X86_64)
module Arm = Pipeline.Make (Arm)
module Aarch64 = Pipeline.Make (Aarch64)
module Riscv32 = Pipeline.Make (Riscv32)
module Riscv64 = Pipeline.Make (Riscv64)

type driver = (module T_intf.DRIVER)

let drivers : driver list =
  [
    (module X86_32 : T_intf.DRIVER);
    (module X86_64 : T_intf.DRIVER);
    (module Arm : T_intf.DRIVER);
    (module Aarch64 : T_intf.DRIVER);
    (module Riscv32 : T_intf.DRIVER);
    (module Riscv64 : T_intf.DRIVER);
  ]

let names = List.map (fun (module D : T_intf.DRIVER) -> D.name) drivers
let find name = List.find_opt (fun (module D : T_intf.DRIVER) -> String.equal D.name name) drivers
