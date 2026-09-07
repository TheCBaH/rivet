(* A performance regression guard for the parse-time pathology asm/docs/
   corpus.md's "One shared timeout, not a per-target rejection" note
   describes: CompCert's test/regression/floats.c (~110K lines of generated
   assembly) once made --dump-source-ast fail to return in any bounded time
   this project tried. That pathology is now fixed, but nothing short of a
   full corpus regen (needs a cross toolchain, and is deliberately kept off
   the asm-test/asm-ci path) would notice its return.

   This generates a comparably large, synthetic x86_64 source in-process
   (no CompCert/Rocq dependency, so it runs on every asm-test/asm-ci leg) and
   bounds --dump-source-ast's own stage, [Driver.Portable.dump]'s
   `Source_ast case, against a realistic wall-clock budget rather than the
   corpus classifier's own generous 120s [classify_timeout_s]
   (asm/tools/lib/corpus_classify_cmd.ml) - tight enough that a reintroduced
   quadratic-or-worse blowup fails fast instead of merely being bounded. *)

let function_count = 20_000
(* Each function contributes 8 source lines below, so this is ~160K lines -
   the same order of magnitude as floats.c's ~110K. *)

let bound_seconds = 20.0

let synthetic_source () =
  let b = Buffer.create (function_count * 64) in
  Buffer.add_string b "\t.text\n";
  for i = 0 to function_count - 1 do
    Buffer.add_string b
      (Printf.sprintf
         "\t.align\t16\n\
          \t.globl fn%d\n\
          fn%d:\n\
          \tsubq\t$8, %%rsp\n\
          \tmovl\t$%d, %%eax\n\
          \taddq\t$8, %%rsp\n\
          \tret\n\
          \t.size\tfn%d, . - fn%d\n"
         i i i i i)
  done;
  Buffer.contents b

let () =
  let text = synthetic_source () in
  (* Enforce the bound while dump is actually running, not just after it
     returns: without this, a reintroduced hang (rather than a merely-slow
     parse) would block asm-test/asm-ci indefinitely instead of failing fast
     at [bound_seconds]. *)
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle
       (fun _ ->
         Printf.eprintf
           "test_parse_perf: --dump-source-ast is still running past the %.1fs bound - this is the \
            floats.c-style parse-time blowup asm/docs/corpus.md's \"One shared timeout\" note \
            describes\n"
           bound_seconds;
         exit 1));
  ignore (Unix.alarm (int_of_float bound_seconds));
  let t0 = Unix.gettimeofday () in
  let result = Driver.Portable.dump "x86_64" ~which:`Source_ast ~unit_name:"parse_perf" ~text in
  ignore (Unix.alarm 0);
  let elapsed = Unix.gettimeofday () -. t0 in
  (match result with
  | Ok _ -> ()
  | Error e ->
      Printf.eprintf "test_parse_perf: unexpected parse failure: %s\n"
        (Driver.Portable.render_error (Err.Error.kind e));
      exit 1);
  Printf.printf "test_parse_perf: parsed %d synthetic functions in %.3fs (bound %.1fs)\n"
    function_count elapsed bound_seconds;
  if elapsed > bound_seconds then (
    Printf.eprintf
      "test_parse_perf: --dump-source-ast took %.3fs, over the %.1fs bound - this is the \
       floats.c-style parse-time blowup asm/docs/corpus.md's \"One shared timeout\" note describes\n"
      elapsed bound_seconds;
    exit 1)
