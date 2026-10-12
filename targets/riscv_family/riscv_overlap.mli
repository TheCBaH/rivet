(** The generated rows whose raw encodings overlap, and why each overlap is acceptable. Rows are
    tried in table order, so a [Specialization] is only correct while its first name precedes its
    second; the tests check both. *)

type relation =
  | Specialization
      (** the first form is a special case of the second (fixed registers, or equal source
          registers): it wins whenever both are legal *)
  | Disjoint_operands
      (** the raw masks overlap but an operand constraint (a zero or reserved field value) keeps the
          decoded sets apart *)
  | Exclusive_by_configuration
      (** the extensions providing the two forms conflict, so no valid configuration enables both *)

val approved : (string * string * relation) list

val find : string -> string -> relation option
(** The relation between two forms, in either order. *)
