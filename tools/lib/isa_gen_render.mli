(** Renders a {!Isa_norm_model.syntax_recipe} into concrete GAS source text
    by substituting each [Syn_operand name] with caller-supplied text. Pure:
    no filesystem or process access. Kept separate from {!Isa_norm_model} -
    which owns [render_syntax], rendering an operand NAME as its own
    placeholder text for design-test inspection - because concrete case
    rendering is case-generation's concern, not the frozen normalized-model's. *)

val render_line :
  Isa_norm_model.syntax_recipe -> operands:(string * string) list -> (string, string) result
(** Walk the recipe exactly as {!Isa_norm_model.render_syntax} does, but look
    up each [Syn_operand name]'s concrete text in [operands] instead of using
    [name] itself. [Error] names the first operand the recipe references
    that [operands] has no assignment for. *)

val render_source : string -> string
(** Wrap one rendered instruction line as the minimal relocation-free GAS
    source this generator's pilot cases need: [".text\n" ^ line ^ "\n"]
    ("begin with relocation-free instructions in .text"). Equivalent to
    [render_source_lines [line]]. *)

val render_source_lines : string list -> string
(** [".text\n" ^ String.concat "\n" lines ^ "\n"] - the multi-line generalization
    {!Isa_gen_difficult} needs for a case whose legal rendering requires more
    than the one instruction line {!render_source} wraps, e.g. a branch's local
    numeric label (["1:"]) or the [nop] it targets. Still relocation-free as
    long as every referenced label is defined within the same [lines]: GAS
    resolves a local label's displacement at assembly time when both ends are
    in the same section of the same translation unit. *)
