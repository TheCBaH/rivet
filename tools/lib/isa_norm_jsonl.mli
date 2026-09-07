(** Bidirectional JSONL codec for {!Isa_norm_model.form}: a
    first-class inspection artifact, with round-trip and
    deterministic regeneration tests.

    Every {!Isa_norm_model} sum type is encoded with an explicit string tag
    (["kind"] for multi-field variants, a bare string for enum-shaped types);
    an unrecognized tag is a decode error, never a silently-accepted default,
    matching this project's "unknown constructs must be reported" convention
    (§3.3). Each encoded object also carries a
    [schema_version] member, so a future incompatible change to the model is
    a detectable version mismatch on decode rather than a silent shape drift.

    Encoding is total and deterministic: the member order for every object is
    fixed by this module's source code (not by input data), so encoding the
    same {!Isa_norm_model.form} value always produces the same bytes. *)

val schema_version : int
(** The version stamped into every encoded object's [schema_version] member,
    and required to match exactly on decode. *)

val to_json : Isa_norm_model.form -> Jsont.json
(** Total: every {!Isa_norm_model.form} value has a JSON representation. *)

val of_json : Jsont.json -> (Isa_norm_model.form, string) result
(** The left inverse of {!to_json} on its own output: for every [form],
    [of_json (to_json form) = Ok form]. Rejects a [schema_version] other than
    {!schema_version}, a member of the wrong JSON kind, a missing required
    member, or an unrecognized tag string, each with a distinct message. *)

val encode_line : Isa_norm_model.form -> (string, Tool_error.t) Err.t
(** One compact JSON object (RFC 8259 minified, {!Jsont.Minify}), no
    trailing newline - the same per-line shape as {!Isa_db_jsonl}'s input. *)

val decode_line : string -> (Isa_norm_model.form, Tool_error.t) Err.t
(** Parse one line produced by {!encode_line} (or any equivalent JSON text)
    back into a {!Isa_norm_model.form}. *)
