(** The committed hex dump format: lowercase, space-separated, 16 bytes per
    line, a newline after every line including the last.

    Three copies of this existed in shell - asm-fixture-oracle.sh,
    asm-fixture-exec.sh and asm-gas-xref.sh, all retired in Phase 8. This is
    the single one, and the duplication it replaced is why it exists. *)

val of_bytes : string -> string
(** Empty input yields [""] - a zero-byte file, NOT a bare newline. A short
    final line is emitted as-is, so a 23-byte input produces a 16-byte line and
    a 7-byte line, matching the committed corpus. *)

val parse : string -> (string, Tool_error.t) Err.t
(** The exact inverse. Rejects uppercase, odd-length tokens, non-hex
    characters, a missing final newline, and any line longer than 16 bytes -
    each of which would round-trip lossily and so would let a corrupted oracle
    artifact compare equal. *)
