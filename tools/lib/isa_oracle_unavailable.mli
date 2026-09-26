(** Recorded GNU assembler capability gaps: an extension (or one spelling) the
    installed GAS for one profile cannot assemble, with the probe that showed
    it. A record listed here for its profile is reported as
    [oracle-unavailable] rather than blocked or silently left without a case,
    and no case is generated for it. *)

type t = {
  source : string;  (** ["riscv_opcodes"] or ["xed_resolved"] *)
  target : Target.t;
  extension : string;  (** the source family, e.g. ["rv_zicfiss"] *)
  native_name : string option;
      (** [None]: every record of the family. [Some n]: only that record - a spelling GAS has
          no mnemonic for at all. Either applies whether or not the record normalizes. *)
  reason : string;  (** short label used in reports *)
  probe : string;  (** the tool, its version, the spelling tried and GAS's answer *)
}

val all : t list

val find : source:string -> Target.t -> extension:string -> t option
(** The family-wide entry, if any. *)

val find_record : source:string -> Target.t -> extension:string -> native_name:string -> t option
(** The entry naming this record specifically, if any. *)
