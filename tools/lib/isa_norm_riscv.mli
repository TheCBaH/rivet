(** RISC-V source-record normalization: the frozen pilot examples -
    [sw], [beq], [c.addi], [sh1add], [fadd.s] - plus the
    explicit R-type (register-register) and I-type (register-immediate)
    integer mnemonic allowlists named in the RISC-V pilot (add, addi,
    sub, mul and their RV32I/RV64I/M-extension siblings). Any other
    [native_name] returns an explicit [Unhandled] diagnostic rather than a
    fabricated or partial {!Isa_norm_model.form} - full-family coverage
    (loads, branches beyond [beq], shifts, compressed forms beyond
    [c.addi], ...) remains later work. *)

val normalize : Isa_source_record.t -> (Isa_norm_model.form, Isa_norm_model.diagnostic) result
