(* Link-side performance guard: synthetic x86_64 programs with many
   functions, branches, fixups and a large data section go through
   assemble and bind, plus targeted inputs for alignment, .ascii, large
   Bigints and decoding. The bounds are generous (CI legs vary); they
   catch a quadratic blowup, not a regression of a few percent. The
   measured times are printed so they can be recorded. *)

let functions =
  match Sys.getenv_opt "RIVET_PERF_FUNCTIONS" with Some n -> int_of_string n | None -> 1_500

let bound_seconds = 120.0

let timed name f =
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle
       (fun _ ->
         Printf.eprintf "test_assemble_perf: %s still running past %.0fs\n" name bound_seconds;
         exit 1));
  ignore (Unix.alarm (int_of_float bound_seconds));
  let t0 = Unix.gettimeofday () in
  let r = f () in
  ignore (Unix.alarm 0);
  Printf.printf "test_assemble_perf: %-22s %7.3fs\n%!" name (Unix.gettimeofday () -. t0);
  r

let get = function
  | Ok v -> v
  | Error e ->
      Printf.eprintf "test_assemble_perf: %s\n" (Driver.Portable.render_error (Err.Error.kind e));
      exit 1

let program functions =
  let b = Buffer.create (functions * 400) in
  let add fmt = Printf.bprintf b fmt in
  add "\t.text\n";
  for i = 0 to functions - 1 do
    add "\t.globl fn%d\nfn%d:\n\tsubq\t$8, %%rsp\n" i i;
    for k = 0 to 3 do
      add "\tcmpl\t$%d, %%edi\n\tje\t.L%d_%d\n\tmovl\t$%d, %%eax\n.L%d_%d:\n" k i k i i k
    done;
    add "\tcall\tfn%d\n\tjmp\tfn%d\n\taddq\t$8, %%rsp\n\tret\n\t.size\tfn%d, . - fn%d\n"
      ((i + 1) mod functions)
      (i * 7 mod functions)
      i i
  done;
  add "\t.data\n\t.balign 8\n";
  for i = 0 to functions - 1 do
    add "tab%d:\n\t.quad\tfn%d\n\t.quad\ttab%d - tab%d\n\t.ascii\t\"%s\"\n" i i i i
      (String.make 64 'x')
  done;
  add "\t.bss\n\t.zero\t0x400000\n";
  Buffer.contents b

let assemble text = get (Driver.Portable.assemble "x86_64" ~unit_name:"perf" ~text)

let () =
  let text = program functions in
  let planned = timed "assemble" (fun () -> assemble text) in
  ignore
    (timed "bind" (fun () ->
         get (Driver.Portable.bind_sequential planned ~base:0x400000L ~gap:4096)));
  let big_align =
    String.concat "" (List.init 200 (fun _ -> "\t.byte 1\n\t.p2align 12\n")) |> fun s ->
    "\t.data\n" ^ s
  in
  ignore (timed "large alignment" (fun () -> assemble big_align));
  let ascii = "\t.data\n\t.ascii \"" ^ String.make 200_000 'a' ^ "\"\n" in
  ignore (timed "long .ascii" (fun () -> assemble ascii));
  let bigint =
    "\t.data\n"
    ^ String.concat ""
        (List.init 300 (fun i -> Printf.sprintf "\t.quad ((1 << 4000) >> 3990) + %d - 1024\n" i))
  in
  ignore (timed "large Bigints" (fun () -> assemble bigint));
  let ints =
    "\t.data\n"
    ^ String.concat "" (List.init 30_000 (fun i -> Printf.sprintf "\t.zero %d\n" (i mod 7)))
  in
  ignore (timed "integer arguments" (fun () -> assemble ints));
  let table =
    "\t.text\n"
    ^ String.concat ""
        (List.init 5_000 (fun i ->
             Printf.sprintf "\tvpaddd\t%%xmm%d, %%xmm2, %%xmm3\n\tpshufb\t%%xmm%d, %%xmm1\n"
               (i mod 8) (i mod 8)))
  in
  ignore (timed "table-row mnemonics" (fun () -> assemble table));
  let many_symbols =
    "\t.text\n"
    ^ String.concat ""
        (List.init 150_000 (fun i -> Printf.sprintf "\t.globl s%d\ns%d:\n\tjmp s%d\n" i i (i / 2)))
  in
  let planned = timed "150,000 symbols" (fun () -> assemble many_symbols) in
  ignore
    (timed "bind, 150,000 symbols" (fun () ->
         get (Driver.Portable.bind_sequential planned ~base:0x400000L ~gap:4096)));
  let small = assemble (program 300) in
  let b = get (Driver.Portable.bind_sequential small ~base:0x400000L ~gap:4096) in
  let code = Option.get (Driver.Portable.section_bytes b ".text") in
  ignore
    (timed "disassemble" (fun () ->
         get (Driver.Portable.disassemble b ~mode:`Canonical ~address:0x400000L code)))
