(** The RISC-V bit-manipulation, scalar-crypto and conditional-operation components. Zk, Zkn, Zks and
    B are implication-only groups of the leaf extensions and are off by default, so that the leaves they
    imply can still be removed individually. *)

val zba : Rivet_target_features.Component.t
val zbb : Rivet_target_features.Component.t
val zbs : Rivet_target_features.Component.t
val zbc : Rivet_target_features.Component.t
val zbkb : Rivet_target_features.Component.t
val zbkc : Rivet_target_features.Component.t
val zbkx : Rivet_target_features.Component.t
val zknd : Rivet_target_features.Component.t
val zkne : Rivet_target_features.Component.t
val zknh : Rivet_target_features.Component.t
val zksed : Rivet_target_features.Component.t
val zksh : Rivet_target_features.Component.t
val zicond : Rivet_target_features.Component.t
val b : Rivet_target_features.Component.t
val zkn : Rivet_target_features.Component.t
val zks : Rivet_target_features.Component.t
val zk : Rivet_target_features.Component.t
