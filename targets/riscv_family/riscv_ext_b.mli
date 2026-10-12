(** The RISC-V bit-manipulation, scalar-crypto and conditional-operation components. Zk, Zkn, Zks and
    B are implication-only groups of the leaf extensions and are off by default, so that the leaves they
    imply can still be removed individually. *)

val zba : Target_component.t
val zbb : Target_component.t
val zbs : Target_component.t
val zbc : Target_component.t
val zbkb : Target_component.t
val zbkc : Target_component.t
val zbkx : Target_component.t
val zknd : Target_component.t
val zkne : Target_component.t
val zknh : Target_component.t
val zksed : Target_component.t
val zksh : Target_component.t
val zicond : Target_component.t
val b : Target_component.t
val zkn : Target_component.t
val zks : Target_component.t
val zk : Target_component.t
