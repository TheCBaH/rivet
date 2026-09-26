(** The residual ledger: every source family that still has blocked records is owned by a row that
    names what is missing.

    Plan section 7's closure rule is that a deferral must name the missing capability, the
    evidence, an owner task and a reopening gate, and that "everything difficult is deferred" is
    not closure. The ledger is that rule as data, checked against the family-admission matrix: a
    blocked family no row owns, a family two rows claim, a row that owns nothing, and a row that
    names a family with no blocked records are all failures. Adding a form that closes a family
    therefore forces the ledger to be edited, and a new blocker cannot appear unowned.

    Only ownership is checked, deliberately not counts: the report prints the live counts, so an
    admission slice never makes the ledger stale merely by shrinking a number. *)

type row = {
  id : string;  (** stable, [RES-<source>-<area>] *)
  source : string;  (** ["riscv_opcodes"] or ["xed_resolved"] *)
  families : string list;  (** the source-native family names this row owns *)
  capability : string;  (** what is missing, concretely enough to start work from *)
  evidence : string;  (** where the gap is measured or demonstrated *)
  task : string;  (** the tracker task that owns closing it *)
  reopening_gate : string;  (** the condition under which the deferral is reopened *)
}

val rows : row list

type cell = {
  source : string;
  family : string;
  target : Target.t;
  total : int;
  normalized_only : int;
  gas_generatable : int;
  promoted : int;
  oracle_unavailable : int;
  blocked : int;
}
(** One family's tally in one profile of one source. *)

type audit = {
  unowned : (string * string) list;  (** (source, family) with blocked records and no owning row *)
  ambiguous : (string * string * string list) list;  (** claimed by more than one row *)
  empty_rows : string list;  (** rows that own no blocked family at all *)
  stale_names : (string * string) list;
      (** (row id, family) where the row names a family with no blocked records *)
}

val inputs : (string * Target.t) list
(** The four source/profile exports the ledger reconciles. *)

val audit : row list -> cell list -> audit
val is_clean : audit -> bool
val problems : audit -> string list
val cells : Repo.t -> (cell list, Tool_error.t) Err.t
val report_lines : row list -> cell list -> string list

val run : Repo.t -> Command.t
(** Print the ledger with live counts and fail if {!audit} finds a problem. *)
