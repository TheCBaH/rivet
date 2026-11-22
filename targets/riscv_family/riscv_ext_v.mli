(** The RISC-V vector components and their crypto and BF16 subsets. Zve32x, Zve32f and Zve64x own
    the vector forms (integer, floating-point, and loads and stores of 64-bit elements); Zve64f,
    Zve64d and V only imply them, and Zve64d requires D. The crypto subsets require Zve32x (Zvbc and
    Zvknhb Zve64x) and the BF16 ones Zve32f. The Zvkn and Zvks suites are implication-only groups,
    off by default. *)

val zve32x : Rivet_target_features.Component.t
val zve32f : Rivet_target_features.Component.t
val zve64x : Rivet_target_features.Component.t
val zve64f : Rivet_target_features.Component.t
val zve64d : Rivet_target_features.Component.t
val v : Rivet_target_features.Component.t
val zvbb : Rivet_target_features.Component.t
val zvkned : Rivet_target_features.Component.t
val zvbc : Rivet_target_features.Component.t
val zvknha : Rivet_target_features.Component.t
val zvksed : Rivet_target_features.Component.t
val zvkg : Rivet_target_features.Component.t
val zvksh : Rivet_target_features.Component.t
val zvfbfmin : Rivet_target_features.Component.t
val zvfbfwma : Rivet_target_features.Component.t
val zvkb : Rivet_target_features.Component.t
val zvknhb : Rivet_target_features.Component.t
val zvkn : Rivet_target_features.Component.t
val zvks : Rivet_target_features.Component.t
val zvknc : Rivet_target_features.Component.t
val zvkng : Rivet_target_features.Component.t
val zvksc : Rivet_target_features.Component.t
val zvksg : Rivet_target_features.Component.t
