(** Runs a laid-out image natively, in this process. The code being run is trusted: a fault in it is
    a fault of this process. Mapping, protection and copying are internal. *)

open Asm_core

type io = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external io_address : ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t -> int64
  = "native_exec_io_address"

val host_isa : string option
(** The target name of the ISA this process runs, if one the assembler targets. *)

type event =
  | Mapped of { base : int64; size : int }
  | Protected of { segment : string; address : int64; size : int; perms : Perms.t }
  | Flushed of { segment : string; address : int64; size : int }
  | Calling of { entry : int64 }
  | Unmapped of { base : int64; size : int }

type outcome = { value : int64; globals : (string * string) list }

type error =
  | No_entry
  | Bind of string
  | Missing_global of string
  | Os of string
  | Signal of int
  | Timeout of float
  | Child of string
  | Foreign_target of { target : string; host : string option }
  | Closed
  | Missing_symbol of string
  | Global_size of { name : string; expected : int; actual : int }
  | Global_range of string
  | Rwx_segment of string

val pp_error : Format.formatter -> error -> unit
val host_symbol : string -> int64 option

val trampolines : target:string -> (string * int64) list -> (string, error) result
(** Assembly text defining every name as a jump to its address. *)

val bind_host : target:string -> string list -> (string, error) result
(** [trampolines] for names resolved in this process. *)

type loaded

val load : ?observe:(event -> unit) -> ?target:string -> Image.laid_out -> (loaded, error) result
val call : loaded -> io:io -> (int64, error) result
val close : loaded -> unit
val read_global : loaded -> string -> (string, error) result
val write_global : loaded -> string -> string -> (unit, error) result

val run :
  ?target:string ->
  ?observe:(event -> unit) ->
  ?read_globals:string list ->
  ?isolate:bool ->
  ?timeout_s:float ->
  Image.laid_out ->
  io:io ->
  (outcome, error) result
