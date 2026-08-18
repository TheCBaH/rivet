(** The fixture and gas-xref manifests: one [key<TAB>value] per line, keys
    sorted bytewise, values escaped for backslash, tab and newline.

    The primary record is the SHA-256 of the exact committed bytes. Everything
    else - a compiler banner, a configure argument - can agree while the bytes
    differ, which is why a manifest that carried only provenance would prove
    nothing. *)

type key =
  | Generator
  | Source
  | Inputs  (** gas-xref only *)
  | Ccomp_version of Target.t
  | Ccomp_target of Target.t
  | Ccomp_args of Target.t
  | Ccomp_configure_args of Target.t
  | Sha256 of string
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

val escape : string -> string

val unescape : string -> (string, Tool_error.t) Err.t
(** Order matters and is the shell's: backslash first, then TAB, then newline.
    [unescape] is the exact inverse and rejects any other escape. *)
