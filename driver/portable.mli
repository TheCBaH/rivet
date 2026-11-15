type unknown_target = { requested : string; known : string list }

type error =
  [ `Bind_multi_segment of string list
  | `Diagnostics of Foundation.Diagnostic.t list
  | `Unknown_target of unknown_target ]

val pp_error : Format.formatter -> error -> unit
val error_code : error -> string
val render_error : error -> string
val set_trace_policy : unit -> unit
val targets : string list

val dump :
  String.t ->
  which:[< `Lowered_ast | `Normalized_ast | `Source_ast | `Tokens ] ->
  unit_name:string ->
  text:string ->
  (string, error) Err.t

val dump_codec : String.t -> (string, error) Err.t

type planned = { laid_out : Image.laid_out; target : string }

val assemble :
  ?entry:string -> String.t -> unit_name:string -> text:string -> (planned, error) Err.t

val assemble_many : ?entry:string -> String.t -> (string * string) list -> (planned, error) Err.t
val plan_text : planned -> string
val fixup_observations : planned -> string list
val segments : planned -> (string * int * int * int) list

type bound = { image : Image.t; target : string }

val bind : planned -> addresses:(string * int64) list -> (bound, error Err.Error.t) result
val planned_segments : planned -> Image.segment_plan list
val bind_at : planned -> base:int64 -> (bound, error) Err.t
val bind_sequential : planned -> base:int64 -> gap:int -> (bound, error Err.Error.t) result
val image_text : bound -> string
val section_bytes : bound -> String.t -> string option
val entry : bound -> int64 option
val exports : bound -> (string * int64) list

type manifest_segment = { name : string; address : int64; perms : Asm_core.Perms.t; bytes : string }

type manifest = {
  target : string;
  entry : int64 option;
  exports : (string * int64) list;
  segments : manifest_segment list;
}

val manifest : bound -> manifest

val disassemble :
  bound -> mode:[< `Canonical | `Diagnostic ] -> address:int64 -> string -> (string, error) Err.t
