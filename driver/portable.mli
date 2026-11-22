type unknown_target = { requested : string; known : string list }

type error =
  [ `Bind_multi_segment of string list
  | `Diagnostics of Rivet_foundation.Diagnostic.t list
  | `Unknown_target of unknown_target ]

val pp_error : Format.formatter -> error -> unit
val error_code : error -> string
val render_error : error -> string
val set_trace_policy : unit -> unit
val targets : string list

val dump :
  ?features:Rivet_target_features.Config.spec ->
  String.t ->
  which:[< `Lowered_ast | `Normalized_ast | `Source_ast | `Tokens ] ->
  unit_name:string ->
  text:string ->
  (string, error) Err.t

val dump_codec : String.t -> (string, error) Err.t

type planned = { laid_out : Rivet_image.laid_out; target : string }

type request = Rivet_target_intf.Target.request = {
  entry : string option;
  features : Rivet_target_features.Config.spec option;
  max_features : Rivet_target_features.Config.spec option;
  externs : string list;
  exported : string list option;
  max_size : int option;
  allow_rwx : bool;
  group_sections : bool;
}
(** every planning option in one value: entry symbol, feature spec, symbols left undefined until
    bind ([externs]), the exported-symbol list, a size limit, whether an RWX segment is allowed *)

val default_request : request
val assemble_with : request -> String.t -> unit_name:string -> text:string -> (planned, error) Err.t
val assemble_many_with : request -> String.t -> (string * string) list -> (planned, error) Err.t

val assemble :
  ?entry:string -> String.t -> unit_name:string -> text:string -> (planned, error) Err.t
(** [assemble_with] over {!default_request} with [entry] set *)

val assemble_many : ?entry:string -> String.t -> (string * string) list -> (planned, error) Err.t
val plan_text : planned -> string
val fixup_observations : planned -> string list
val segments : planned -> (string * int * int * int) list

type bound = { image : Rivet_image.t; target : string }

type bind_request = {
  imports : (string * int64) list;  (** addresses of the module's [externs] *)
  overlays : string list list;  (** groups of segments allowed to share addresses *)
  load_addresses : (string * int64) list;  (** per-segment load address when it differs *)
}

val default_bind_request : bind_request

val bind_with :
  bind_request -> planned -> addresses:(string * int64) list -> (bound, error Err.Error.t) result

val bind : planned -> addresses:(string * int64) list -> (bound, error Err.Error.t) result
val planned_segments : planned -> Rivet_image.segment_plan list
val bind_at : planned -> base:int64 -> (bound, error) Err.t
val bind_sequential : planned -> base:int64 -> gap:int -> (bound, error Err.Error.t) result
val image_text : bound -> string
val section_bytes : bound -> String.t -> string option
val entry : bound -> int64 option
val exports : bound -> (string * int64) list

type manifest_segment = {
  name : string;
  address : int64;
  perms : Rivet_asm_core.Perms.t;
  bytes : string;
}

type manifest = {
  target : string;
  entry : int64 option;
  exports : (string * int64) list;
  segments : manifest_segment list;
}

val manifest : bound -> manifest

val disassemble_for :
  ?features:Rivet_target_features.Config.spec ->
  String.t ->
  mode:[< `Canonical | `Diagnostic ] ->
  address:int64 ->
  string ->
  (string, error) Err.t
(** disassemble for a named target, with no [bound] value needed *)

val disassemble :
  bound -> mode:[< `Canonical | `Diagnostic ] -> address:int64 -> string -> (string, error) Err.t

val disassemble_records_for :
  ?features:Rivet_target_features.Config.spec ->
  ?inspect:bool ->
  String.t ->
  address:int64 ->
  string ->
  (Rivet_target_intf.Target.disasm_record list, error) Err.t
(** the records both disassembly modes are rendered from, for a named target *)

val disassemble_records :
  ?inspect:bool ->
  bound ->
  address:int64 ->
  string ->
  (Rivet_target_intf.Target.disasm_record list, error) Err.t

val listing : ?inspect:bool -> bound -> segment:string -> (string, error) Err.t
(** one segment of a bound image disassembled, with symbols at their definitions and beside the
    instructions whose fixups name them *)

val listing_records :
  ?inspect:bool ->
  bound ->
  segment:string ->
  (Rivet_target_intf.Target.listing_line list, error) Err.t
