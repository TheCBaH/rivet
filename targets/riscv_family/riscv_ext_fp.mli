(** The RISC-V floating-point extensions beyond F and D. Each requires the base format it
    extends; Zfh implies Zfhmin. *)

val q : Target_component.t
val zfh : Target_component.t
val zfhmin : Target_component.t
val zfa : Target_component.t
val zfbfmin : Target_component.t
