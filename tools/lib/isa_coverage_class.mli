(** Coverage classes for generated cases, and the per-architecture obligations over them.

    A canonical spelling, an alias, a pseudo-op expansion and a negative case answer different
    questions, so a pass in one says nothing about another (plan sections 5.2 and 7). Every case
    belongs to exactly one class, read from what the case already declares - [negative], or the
    [alias-spelling] and [pseudo-expansion] rule ids - so the corpus schema is unchanged.

    The generator is a fixed, enumerated manifest with no sampling: there is no random seed and the
    budget is one case per (form, target, obligation). That is stated in the report rather than
    left implicit, so a future sampled family has to change it visibly.

    An {!obligation} is what the plan requires of an architecture in a class. It is either
    satisfied - the corpus has at least one case - or unsatisfied with a reason and a task. The
    check fails when a satisfied obligation has no case, and when an unsatisfied one already has,
    so neither a lost case nor a stale excuse survives. *)

type klass = Canonical | Alias | Pseudo | Negative

val klass_of : Isa_generated_case.case -> klass
val klass_to_string : klass -> string

type status = Satisfied | Unsatisfied of { reason : string; task : string }
type obligation = { target : Target.t; klass : klass; status : status }

val obligations : obligation list
(** Alias, pseudo and negative for each of the four profiles the corpus covers. Canonical is the
    baseline every admitted form already has and is not an obligation here. *)

val count : Isa_generated_case.case list -> Target.t -> klass -> int

val problems : Isa_generated_case.case list -> string list
(** The obligation violations for a set of cases; empty means every obligation holds. *)

val report_lines : Isa_generated_case.case list -> string list

val run : Repo.t -> Command.t
(** Print the classes and obligations for the committed pilot and difficult corpora, and fail on
    any {!problems}. Offline: it reads only the committed JSONL. *)
