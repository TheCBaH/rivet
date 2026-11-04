(** Hazards in generated OCaml that only some CI legs reveal: a long list literal (Chromium's
    parser overflows on the jsoo/Melange bundle) and generated values in a functor body (ARM32
    load offsets overflow). Emitters check their text with it, so emission and its check both
    fail. Strings and comments are skipped. *)

val max_list : int
(** The longest list literal allowed: 200 elements. *)

val check : string -> (unit, string) result
(** [Error] names the first hazard and its line: a list literal over {!max_list} elements, the
    [functor] keyword or a [module M (...)] definition. Array literals are fine. *)

val check_files : (Fpath.t * string) list -> (unit, Tool_error.t Err.Error.t) result
(** {!check} over files about to be written; the error names the file. *)
