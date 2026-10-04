(* Foundation's errors are values of the consumer's Err, not of a second copy:
   the consumer's Err inspects them. *)

let () =
  match Foundation.Bigint.of_string "not a number" with
  | Ok _ -> failwith "parsed"
  | Error e -> (
      let r : (Foundation.Bigint.t, Foundation.Bigint.error) Err.t = Error e in
      match Err.payload r with Error _ -> () | Ok _ -> failwith "payload")
