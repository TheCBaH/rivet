(** The fixture and gas-xref manifests: one [key<TAB>value] per line, keys
    sorted bytewise, values escaped for backslash, tab and newline.

    The primary record is the SHA-256 of the exact committed bytes. Everything
    else - a compiler banner, a configure argument - can agree while the bytes
    differ, which is why a manifest that carried only provenance would prove
    nothing. *)

type key =
  | Generator
  | Source
  | Source_unit of string
      (** M3 (.ai/asm_plan.md §12): one of several sources in a multi-source
          case, keyed by its own unit name. The single-source [Source] key is
          unchanged and still what every case with one source carries - this is
          additive, not a replacement, so no existing manifest's bytes change.
      *)
  | Inputs  (** gas-xref only *)
  | Ccomp_version of Target.t
  | Ccomp_target of Target.t
  | Ccomp_args of Target.t
  | Ccomp_configure_args of Target.t
  | Sha256 of string
  | Abi_version
      (** M4 (.ai/asm_plan.md §12): the fixture-exec ABI version a case
          dispatches to - ["1"], ["2"] or ["3"]; absent means ["1"].
          Author-declared, never derived, so regen/rehash always carry it
          forward verbatim rather than computing it. *)
  | Supported_targets
      (** M4: the subset of the six targets this case is committed for,
          space-separated in {!Target.all}'s canonical order; absent means all
          six. Consulted by every target/case traversal before any compiler
          requirement or filesystem access, so an unsupported target is skipped
          rather than treated as a missing-file error. *)
  | Expected_value of int
      (** M4: element [i] (>= 1; element 0 is always [expected-status.txt]) of a
          v3 fixture's result vector, as a decimal [Int64]. Paired one-to-one
          with an {!Observation} of the same index; both require
          [abi-version: 3]. *)
  | Observation of int
      (** M4: the host-side observation descriptor -
          ["<symbol> <width> <sign-extend:0|1>"] - that produces
          {!Expected_value} of the same index. [symbol] is resolved against a
          built [Image.exports] at exec time, not here. *)
  | Origin of string
      (** M4: a unit whose committed [<target>/<stem>.s] is preprocessed from an
          upstream, not-project-authored [.S] source (e.g. a CompCert runtime
          helper) rather than compiled from a project [.c] file - keyed by the
          unit's own stem, exactly parallel to {!Source_unit}. The value is the
          upstream repo-relative source path. *)
  | Unknown of string

type record = { key : key; value : string option }
(** [value = None] means the line has NO TAB at all, which is a different
    manifest from one with an empty value: `printf 'ccomp-args:%s\n' "$t"` emits
    a bare key when the array is empty, so `ccomp-args:aarch64` and
    `ccomp-args:aarch64<TAB>` are distinct records. *)

type t

val parse : string -> (t, Tool_error.t Err.Accum.errors) Err.t
(** Accumulating: per-record validation wants EVERY complaint and no partial
    value, which is the one shape [Err.Accum] fits. The error type therefore
    reaches the CLI only through [Command.of_accum_result].

    Rejections, each a deliberate divergence from the shell:
    - a missing final newline (D4): the shell's `while read` loop silently drops
      an unterminated final record;
    - duplicate [Sha256] paths and values that are not [0-9a-f]{64}] (D5);
    - TAB, newline or NUL in a KEY, and unsafe relative paths (D6) - the key
      side has no escape layer, so these could not round-trip;
    - a duplicate [Ccomp_version] for one target (D12): the shell's
      carry-forward `grep` emits EVERY match, so today all duplicates are
      carried forward verbatim;
    - an invalid escape sequence in a value (D7): the shell's reader never
      unescapes at all.

    Duplicate [Source]: FIRST WINS (P3), matching `awk '... {print; exit}'`. *)

val records : t -> record list

val serialize : t -> string
(** Rendered lines sorted with [String.compare], which is bytewise and therefore
    identical to `LC_ALL=C sort`. *)

val of_records : record list -> t
val hashes : t -> (string * string) list

val find : t -> key -> string option option
(** [None] = no such record; [Some None] = a bare key with no tab. *)

val source_rel : t -> (string option, Tool_error.t) Err.t
(** The validated [source] value, or [None] when the manifest has none. *)

val source_units : t -> ((string * string) list, Tool_error.t) Err.t
(** Every [Source_unit] record as [(unit_name, source_rel)], sorted by unit name
    for a deterministic build order - [[]] when the manifest has none (the
    single-source case, where {!source_rel} is what a caller reads instead). *)

val supported_targets : t -> (Target.t list option, Tool_error.t) Err.t
(** The decoded {!Supported_targets} value, or [None] when the manifest has none
    (interpreted by every caller as "all six" - this function does not apply
    that default itself). Values are already validated at {!parse} time; this
    only re-decodes them. *)

val origins : t -> ((string * string) list, Tool_error.t) Err.t
(** Every [Origin] record as [(stem, upstream_path)], sorted by stem - the same
    shape and ordering contract as {!source_units}. *)

val escape : string -> string

val unescape : string -> (string, Tool_error.t) Err.t
(** Order matters and is the shell's: backslash first, then TAB, then newline.
    [unescape] is the exact inverse and rejects any other escape. *)
