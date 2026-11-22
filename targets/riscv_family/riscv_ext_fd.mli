(** The RISC-V F and D floating-point components. F requires Zicsr and D requires F. *)

val f : Rivet_target_features.Component.t
val d : Rivet_target_features.Component.t
