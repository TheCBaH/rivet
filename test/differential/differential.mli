(** The differential gate over a compiler corpus: bytes, objdump spelling, reassembly and
    relocations of every case on every target, against the committed GNU oracle artifacts. Each
    check prints one transcript row per case and target; an instance pins the rows in its own
    expect tests. *)

val read : string -> string
(** a file's whole contents *)

val normalize : ?side:[ `Ours | `Objdump ] -> string -> string -> string
(** [normalize target line]: the declared spelling normalizations the objdump comparison applies
    to both sides *)

(** A corpus in the fixture layout: [root/<case>/<target>/*.s] with the oracle artifacts beside
    them, and an optional [root/<case>/manifest.txt] whose [supported-targets] record narrows the
    targets. *)
module type CORPUS = sig
  val root : string
  (** the fixture directory, one subdirectory per case and per target below it *)

  val targets : string list
  (** the targets every case is checked on, unless its manifest restricts them *)

  val is_case : string -> bool
  (** whether a directory under [root], given as its path, is a case *)
end

type record = string * int * string * string * int64 option
(** a relocation as the comparison keys it: section, offset, type, symbol and addend *)

module Make (_ : CORPUS) : sig
  val corpus_root : string
  val cases : unit -> string list
  val targets_for : string -> string list

  val check_bytes : string -> string -> unit
  (** [check_bytes case target]: every bound segment against the controlled reference link *)

  val check_disasm : string -> string -> unit
  (** the diagnostic dump against objdump, after {!normalize} *)

  val check_round_trip : string -> string -> unit
  (** the canonical dump reassembled to the same bytes *)

  val check_relocs : string -> string -> unit
  (** fixup observations classified against the measured relocations *)

  val show_record : record -> string

  val compare_records : predicted:record list -> measured:record list -> string list
  (** every way the two multisets disagree: missing, extra, mismatched and duplicate keys *)
end
