(** The RISC-V CSR, fence, privileged, cache-management, hint and control-flow-integrity
    components. *)

val zicsr : Target_component.t
val zifencei : Target_component.t
val zicntr : Target_component.t
val zihintntl : Target_component.t
val zawrs : Target_component.t
val zimop : Target_component.t
val zicfiss : Target_component.t
val zicfilp : Target_component.t
val zicbom : Target_component.t
val zicboz : Target_component.t
val zicbop : Target_component.t
val system : Target_component.t
val s : Target_component.t
val h : Target_component.t
val svinval : Target_component.t
val sdext : Target_component.t
val ssctr : Target_component.t
