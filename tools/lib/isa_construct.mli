(** The operand and encoding vocabulary of the captured ISA records, and the
    construct-keyed blocker it yields.

    Every record is described by the set of constructs it needs: encoding
    constructs (XED encoding space and map, vector length, [W1], EVEX opmask,
    zeroing, broadcast, rounding, SAE, VSIB, REX2 and the APX EVEX forms,
    relative-branch displacements, [is4] register immediates, [lock]) and
    operand constructs (register class by XED lookup function, memory width,
    immediate width, and the fixed registers that appear in AT&T syntax). A
    riscv-opcodes record needs one operand construct per [variable_fields]
    name, plus [len16] for a compressed encoding.

    A construct is {e known} when some record that normalizes today needs it;
    that set is measured from the export, never listed by hand. A record the
    normalizer has no rule for is then blocked by its first unknown construct
    ([unsupported-encoding:<c>] before [unknown-operand:<c>]), or by
    [no-rule:known-constructs] when every construct it needs is already
    handled for some other form - the cheapest remaining admissions. *)

type kind = Encoding | Operand
type construct = { kind : kind; name : string }

val to_string : construct -> string
(** [unsupported-encoding:<name>] or [unknown-operand:<name>]. *)

val of_record : Isa_source_record.t -> construct list
(** Sorted, duplicate-free; encoding constructs first. *)

val is_catch_all : string -> bool
(** The normalizers' "no rule for this record" diagnostics
    ([unhandled-iform], [unhandled-native-name]) - the only blockers this
    module refines. *)

val no_rule : string
(** ["no-rule:known-constructs"]. *)

type known

val known_of : (Isa_source_record.t * bool) list -> known
(** [(record, normalizes)] pairs; the known set is the union of the
    constructs of every record that normalizes. *)

val missing : known -> Isa_source_record.t -> construct list
val blocker : known -> Isa_source_record.t -> string
