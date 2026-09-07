(** XED source-record normalization: the frozen pilot examples -
    [ADD_GPRv_IMMz] and one x87 form, [FADD_ST0_X87] - plus
    the legacy register/register and register/immediate ADD/MOV forms
    named in the x86 pilot ([ADD_GPRv_GPRv_01], [ADD_GPRv_GPRv_03],
    [MOV_GPRv_GPRv_89], [MOV_GPRv_GPRv_8B], [MOV_GPRv_IMMz]), normalized
    generically from each form's own [encoding.operands] read/write facts,
    plus the bounded memory-direction forms [MOV_GPRv_MEMv] and
    [MOV_MEMv_GPRv].  [FADD_ST0_X87] has a verified AT&T stack recipe;
    its reverse-direction sibling remains unhandled.
    Dispatches on [provenance.iform], not [native_name] - a resolved XED
    record's [native_name] is the shared ICLASS (e.g. both normalize
    [ADD]/[FADD]), not the per-form identity. Any other iform returns an
    explicit [Unhandled] diagnostic; full-family coverage remains later
    work. *)

val normalize : Isa_source_record.t -> (Isa_norm_model.form, Isa_norm_model.diagnostic) result
