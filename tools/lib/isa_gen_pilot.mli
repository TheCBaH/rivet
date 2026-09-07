(** The frozen S3 pilot manifest ("Pilot scope for S3").

    Freezes, before any case-generation code is written, exactly which
    source records/normalized forms make up the pilot, on which of the six
    targets, and how each side's IMPLEMENTATION form count was enumerated -
    stating, per family, how the implementation side is enumerated, since
    the enumeration method is part of what gets reviewed. This rules out a
    single authority across
    both architectures: x86's composite [Codec.choice] is close to one
    alternative per form, so [--dump-codec] approximates a form registry
    there, while RISC-V's codec is two generic wrapper alternatives
    ([word]/[pair]) around explicit encoder functions and cannot be read
    that way - so RISC-V's side of this manifest is a recorded hand reading
    of those functions instead. *)

type enumeration_method =
  | Dump_codec of { alternative_label : string; evidence : string }
      (** x86: reproducible with [dune exec tool/asm.exe -- --target <t>
          --dump-codec <any-file>] (roughly one labelled
          alternative per form). [alternative_label] is the label
          [Codec.inspect] prints for the chosen alternative;
          [evidence] is the exact matched dump line or opcode-table entry
          from [x86_family_encode.ml] that selects it within that
          alternative (e.g. an ALU op's ModR/M-reg extension or
          [to_rm_r]/[to_r_rm] byte). *)
  | Hand_read_table of { function_name : string; evidence : string }
      (** RISC-V: read by hand from the named table-driven function in
          [riscv_family_encode.ml]; [evidence] is the exact matched table
          row, e.g. ["Add -> Some (0x33, 0, 0x00)"]. *)

type pilot_entry = {
  form_id : string;  (** must equal what {!Isa_norm_riscv}/{!Isa_norm_xed} actually produce today *)
  target : Target.t;
  lookup_key : string;
      (** the checked-in export's [native_name] (RISC-V) or [provenance.iform] (XED) identifying
          the source record this entry names *)
  implementation : enumeration_method;
}

val riscv_pilot : pilot_entry list
(** [add]/[sub]/[mul] (R-type) and [addi] (I-type) on both RV32 and RV64,
    plus [addw] on RV64 ONLY as the frozen "RISC-V XLEN restriction case"
    the pilot includes: RV64I's [addw] has no RV32
    counterpart at all (it does not exist in the riscv32 export), so its
    single-target presence here already IS the restriction - no additional
    machinery is needed to demonstrate it. *)

val x86_pilot : pilot_entry list
(** The six already-normalized legacy register/register and
    register/immediate [add]/[mov] forms, each on both x86-32 and x86-64:
    [ADD_GPRv_GPRv_01], [ADD_GPRv_GPRv_03], [ADD_GPRv_IMMz],
    [MOV_GPRv_GPRv_89], [MOV_GPRv_GPRv_8B], [MOV_GPRv_IMMz]. Excludes
    [FADD_ST0_X87]: x87 is explicitly not in the S3 pilot (it waits on a
    later stage). *)

val all : pilot_entry list

(** {1 Mandatory cases} *)

type mandatory_obligation =
  | Negative_immediate_boundary
  | Riscv_xlen_restriction
  | X86_short_vs_full_immediate

val obligation_form_ids : mandatory_obligation -> string list
(** Which pilot {!pilot_entry.form_id}s are responsible for satisfying this
    mandatory obligation once concrete cases are generated from
    them - a freeze of RESPONSIBILITY, not of the cases themselves, which
    do not exist yet. *)

val normalize_entry : Repo.t -> pilot_entry -> (Isa_norm_model.form, Tool_error.t) Err.t
(** Find the real record in the checked-in export matching [entry]'s
    [lookup_key] and normalize it (via {!Isa_norm_riscv}/{!Isa_norm_xed}),
    failing if no matching record normalizes at all. Does NOT check that the
    result's [form_id] equals [entry.form_id] - {!repo_tests.ml}'s own
    grounding check does that comparison; this is the shared lookup the
    case builder also needs. *)
