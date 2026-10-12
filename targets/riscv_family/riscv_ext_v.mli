(** The RISC-V vector component and its crypto and BF16 subsets. V requires D; the subsets require V.
    The Zvkn and Zvks suites are implication-only groups, off by default. *)

val v : Target_component.t
val zvbb : Target_component.t
val zvkned : Target_component.t
val zvbc : Target_component.t
val zvknha : Target_component.t
val zvksed : Target_component.t
val zvkg : Target_component.t
val zvksh : Target_component.t
val zvfbfmin : Target_component.t
val zvfbfwma : Target_component.t
val zvkb : Target_component.t
val zvknhb : Target_component.t
val zvkn : Target_component.t
val zvks : Target_component.t
val zvknc : Target_component.t
val zvkng : Target_component.t
val zvksc : Target_component.t
val zvksg : Target_component.t
