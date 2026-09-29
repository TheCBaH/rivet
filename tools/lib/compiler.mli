(** The compiler a fixture corpus is generated with.

    The fixture machinery only needs to know how to compile one C file to
    assembly and which provenance to record; it is the same for the cross gcc
    this repository ships and for any other compiler a consumer plugs in. *)

type t = {
  name : string;
      (** The manifest key family for the compiler's provenance: a lowercase word,
          for instance [gcc] records [gcc-version:<target>]. *)
  generator : string;  (** The manifest's [generator] value: the command that regenerates it. *)
  installed : Target.t -> bool;
      (** Whether the target's compiler can be started. Performs no [ensure]
          and creates nothing. *)
  require_all : Target.t list -> (unit, Tool_error.t) Err.t;
      (** Names every missing target in one diagnostic. *)
  version : Target.t -> (string, Tool_error.t) Err.t;
      (** The first line of the compiler's version banner. *)
  provenance : Target.t -> (Manifest.key * string option) list;
      (** The records that pin how the target was compiled, besides its
          version. A [None] value is a bare key. *)
  compile_s :
    Target.t ->
    cwd:Fpath.t ->
    out_rel:string ->
    source_rel:string ->
    case:string ->
    (unit, Tool_error.t) Err.t;
      (** Compiles [source_rel] to assembly at [out_rel], both relative to
          [cwd]. Relative, because a compiler that records its command line in
          the output would otherwise put this checkout's location into the
          committed bytes. *)
}
