(** The per-case expected exit status, read from expected-status.txt.

    A committed input, not a literal: the whole file is compared against the
    CANONICAL SERIALIZATION of the value read back, not just its first line. A
    line count would accept "42\njunk" when junk has no terminating newline,
    since that file contains exactly one \n. Round-tripping also rejects CRLF,
    surrounding whitespace, leading zeros and a missing final newline.

    The accepted range stops at 123 because the host runner owns everything
    above: `timeout` reports 124 for a timeout and 125-127 for its own
    failures, and 128+n is a signal death of the qemu process. Allowing those
    would make a guest result observationally identical to a harness failure. *)

val parse : case:string -> string -> (int, Tool_error.t) Err.t
val read : case:string -> Fpath.t -> (int, Tool_error.t) Err.t
