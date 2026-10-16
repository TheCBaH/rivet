(** The RISC-V vector components and their crypto and BF16 subsets. Zve32x, Zve32f and Zve64x own
    the vector forms (integer, floating-point, and loads and stores of 64-bit elements); Zve64f,
    Zve64d and V only imply them, and Zve64d requires D. The crypto subsets require Zve32x (Zvbc and
    Zvknhb Zve64x) and the BF16 ones Zve32f. The Zvkn and Zvks suites are implication-only groups,
    off by default. *)

val zve32x : Target_component.t
val zve32f : Target_component.t
val zve64x : Target_component.t
val zve64f : Target_component.t
val zve64d : Target_component.t
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
