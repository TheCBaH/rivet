type t = {
  name : string;
  generator : string;
  installed : Target.t -> bool;
  require_all : Target.t list -> (unit, Tool_error.t) Err.t;
  version : Target.t -> (string, Tool_error.t) Err.t;
  provenance : Target.t -> (Manifest.key * string option) list;
  compile_s :
    Target.t ->
    cwd:Fpath.t ->
    out_rel:string ->
    source_rel:string ->
    case:string ->
    (unit, Tool_error.t) Err.t;
}
