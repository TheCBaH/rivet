(** Negative cases: source both GAS and this project's assembler must reject.

    A positive case shows two assemblers agree on bytes. A negative case shows they agree on what is
    {e not} an instruction, which byte equality cannot: a recipe that forgot a range check, an
    XLEN or mode restriction, or a feature gate would pass every positive case. Each entry names
    the stable diagnostic category ({!Isa_generated_corpus.expected_code}) our rejection must carry;
    GNU's own error text is never matched.

    Negatives are a separate coverage class from canonical spellings, aliases and pseudo-ops: they
    carry [negative = true], and the case's rule ids say what kind of rejection it is
    ([negative], [category:<c>], [expect-code:<code>]) and, for a feature-configuration negative,
    the [ours-features:<spec>] our assembler runs under beside GAS's [-march]. *)

type entry = {
  name : string;  (** the case's stable short name, e.g. ["imm12-out-of-range"] *)
  target : Target.t;
  category : string;
      (** ["immediate-range"], ["operand-shape"], ["xlen-restricted"], ["operand-width"],
          ["address-form"] or ["feature-disabled"] *)
  mnemonic : string;  (** the mnemonic the case exercises, for the form id *)
  lines : string list;  (** the source lines after [.text] *)
  gas_configuration : string list;  (** the GAS argv fragment, [-march] included *)
  ours_features : string option;  (** [--features] for our assembler, if the case is configured *)
  expected_code : string;  (** the diagnostic category our rejection must carry *)
}

val all : entry list
val case_of : entry -> Isa_generated_case.case

val dummy_encoding : Isa_norm_model.encoding
(** Passed to {!Isa_gen_oracle.run}, which only consults it when GAS {e accepts} the source - and
    an accepted negative is a failure however the bytes look. *)
