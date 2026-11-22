(* requires: rivet.image rivet.text_pipeline rivet.target_features rivet.target_encode rivet.x86_64 rivet.x86_64_encode *)
let _ : Rivet_image.t option = None
let _ : Rivet_target_features.Config.t option = None

module P = Rivet_text_pipeline.Pipeline.Make (Rivet_x86_64)
module E : Rivet_target_encode.ENCODE = Rivet_x86_64_encode
