(** The RISC-V floating-point extensions beyond F and D. Each requires the base format it
    extends; Zfh implies Zfhmin. *)

val q : Rivet_target_features.Component.t
val zfh : Rivet_target_features.Component.t
val zfhmin : Rivet_target_features.Component.t
val zfa : Rivet_target_features.Component.t
val zfbfmin : Rivet_target_features.Component.t
