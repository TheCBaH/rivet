(** The RISC-V CSR, fence, privileged, cache-management, hint and control-flow-integrity
    components. *)

val zicsr : Rivet_target_features.Component.t
val zifencei : Rivet_target_features.Component.t
val zicntr : Rivet_target_features.Component.t
val zihintntl : Rivet_target_features.Component.t
val zawrs : Rivet_target_features.Component.t
val zimop : Rivet_target_features.Component.t
val zicfiss : Rivet_target_features.Component.t
val zicfilp : Rivet_target_features.Component.t
val zicbom : Rivet_target_features.Component.t
val zicboz : Rivet_target_features.Component.t
val zicbop : Rivet_target_features.Component.t
val system : Rivet_target_features.Component.t
val s : Rivet_target_features.Component.t
val h : Rivet_target_features.Component.t
val svinval : Rivet_target_features.Component.t
val sdext : Rivet_target_features.Component.t
val ssctr : Rivet_target_features.Component.t
