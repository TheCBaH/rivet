(* [gnu_text_emit CASES DIR]: writes each RV32 corpus case's GNU print as DIR/<n>.s, with the GNU
   as arguments of the case in <n>.argv (one per line) and the bytes GNU as made from the original
   text in <n>.bytes, for scripts/gnu-text-oracle.sh. *)

module C = Gnu_text_corpus

let write path s = Out_channel.with_open_text path (fun oc -> output_string oc s)

let () =
  match Sys.argv with
  | [| _; cases; dir |] ->
      let n = ref 0 and refused = ref 0 in
      List.iter
        (fun (c : C.case) ->
          match
            C.print ?features:c.features ~instruction:Rivet_riscv32.Instruction.pp_gnu c.source
          with
          | Error m ->
              incr refused;
              Printf.eprintf "%s: %s\n" c.id m
          | Ok text ->
              let base = Filename.concat dir (Printf.sprintf "%04d" !n) in
              incr n;
              write (base ^ ".s") text;
              write (base ^ ".orig") c.source;
              write (base ^ ".argv") (String.concat "\n" c.argv ^ "\n");
              write (base ^ ".bytes") (C.hex c.bytes ^ "\n");
              write (base ^ ".id") (c.id ^ "\n"))
        (C.load cases);
      Printf.printf "%d cases written, %d not printed\n" !n !refused;
      if !refused > 0 then exit 1
  | _ ->
      prerr_endline "usage: gnu_text_emit CASES DIR";
      exit 2
