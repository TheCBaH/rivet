(* Foundation's errors are values of the consumer's Err, not of a second copy:
   the consumer's Err inspects them. *)

let () =
  match Rivet_foundation.Bigint.of_string "not a number" with
  | Ok _ -> failwith "parsed"
  | Error e -> (
      let r : (Rivet_foundation.Bigint.t, Rivet_foundation.Bigint.error) Err.t = Error e in
      match Err.payload r with Error _ -> () | Ok _ -> failwith "payload")

(* The consumer's Fmt and the one Rivet prints through are opam fmt. *)
let () =
  if Fmt.str "%a" Rivet_foundation.Bigint.pp (Rivet_foundation.Bigint.of_int 42) <> "42" then
    failwith "Fmt"
