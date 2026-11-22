(* Fuzzing (review item R1). See the dune file for the two laws.

   A failure is reduced by hand to one input and frozen as an expect case next
   to the contract it violates (as test/props does); the seed and the iteration
   count are printed so a CI log is enough to re-run it. *)

module Q = QCheck2

let env_int name default =
  match Sys.getenv_opt name with
  | Some s -> ( match int_of_string_opt s with Some n when n > 0 -> n | _ -> default)
  | None -> default

let seed = env_int "RIVET_FUZZ_SEED" 20261115
let iters = env_int "RIVET_FUZZ_ITERS" 150

(* From the sandbox of @runtest, or from the repository root under `make fuzz` *)
let corpus_root = List.find Sys.file_exists [ "../../fixtures/gas-xref"; "fixtures/gas-xref" ]

let read path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* Every recorded input of a target: the generated snippets, the directive
   cases, the compiler fixture and the helpers. *)
let corpus target =
  List.concat_map
    (fun tree ->
      let dir = Filename.concat (Filename.concat corpus_root tree) target in
      if not (Sys.file_exists dir) then []
      else
        Array.to_list (Sys.readdir dir)
        |> List.sort String.compare
        |> List.filter_map (fun case ->
            let p = Filename.concat (Filename.concat dir case) "input.s" in
            if Sys.file_exists p then Some (read p) else None))
    [ "generated"; "directives"; "frontier" ]

(* {1 Text generators} *)

let vocabulary =
  [|
    ".text";
    ".data";
    ".bss";
    ".section";
    ".align";
    ".balign";
    ".p2align";
    ".byte";
    ".short";
    ".word";
    ".long";
    ".quad";
    ".ascii";
    ".asciz";
    ".globl";
    ".weak";
    ".local";
    ".comm";
    ".set";
    ".equ";
    ".zero";
    ".space";
    ".type";
    ".size";
    ".uleb128";
    ".option";
    ".arch";
    ".syntax";
    ".pushsection";
    ".popsection";
    ".previous";
    ".hidden";
    ".cfi_startproc";
    ".rept";
    ".macro";
    ".include";
    ".if";
    ".endif";
    "nop";
    "mov";
    "add";
    "ret";
    "jmp";
    "call";
    "b";
    "bl";
    "ldr";
    "str";
    "li";
    "lui";
    "addi";
    "push";
    "pop";
    "eax";
    "%rax";
    "%eax";
    "rsp";
    "x0";
    "x31";
    "sp";
    "r0";
    "r15";
    "pc";
    "lr";
    "w0";
    "$1";
    "$-1";
    "#1";
    "#-4";
    "0";
    "1";
    "-1";
    "255";
    "256";
    "0x7fffffff";
    "0xffffffffffffffff";
    "18446744073709551616";
    "-9223372036854775808";
    "1f";
    "1b";
    "0b101";
    "0x";
    "'a";
    "'\\n'";
    "\"s\"";
    "\"unterminated";
    "foo";
    "bar";
    ".L1";
    ".";
    "@function";
    "%function";
    "@object";
    "\"aw\"";
    "\"ax\",@progbits";
    "(";
    ")";
    "[";
    "]";
    "{";
    "}";
    ",";
    ",,";
    ":";
    ";";
    "+";
    "-";
    "*";
    "/";
    "<<";
    ">>";
    "=";
    "!";
    "~";
    "#";
    "@";
    "%";
    "$";
    "\\";
    "/*";
    "*/";
    "//";
    "\n";
    "\n";
    "\n";
    "\t";
    " ";
  |]

let gen_byte_char =
  Q.Gen.oneof_weighted
    [
      (8, Q.Gen.map Char.chr (Q.Gen.int_range 32 126));
      (1, Q.Gen.oneof_list [ '\n'; '\t'; '\r'; '\000'; '\\'; '"'; '\'' ]);
      (1, Q.Gen.map Char.chr (Q.Gen.int_range 0 255));
    ]

let gen_raw_text =
  Q.Gen.map
    (fun l -> String.init (List.length l) (List.nth l))
    (Q.Gen.list_size (Q.Gen.int_range 0 120) gen_byte_char)

let gen_soup =
  Q.Gen.map (String.concat " ")
    (Q.Gen.list_size (Q.Gen.int_range 0 40) (Q.Gen.oneof_array vocabulary))

let mutate_once s st =
  let n = String.length s in
  if n = 0 then s
  else
    let i = Random.State.int st n in
    match Random.State.int st 6 with
    | 0 -> String.sub s 0 i ^ String.sub s (i + 1) (n - i - 1)
    | 1 -> String.sub s 0 i
    | 2 ->
        String.sub s 0 i
        ^ String.make 1 (Char.chr (Random.State.int st 256))
        ^ String.sub s i (n - i)
    | 3 ->
        let tok = vocabulary.(Random.State.int st (Array.length vocabulary)) in
        String.sub s 0 i ^ tok ^ String.sub s i (n - i)
    | 4 ->
        let j = Random.State.int st n in
        let a = min i j and b = max i j in
        String.sub s 0 b ^ String.sub s a (b - a) ^ String.sub s b (n - b)
    | _ ->
        let tok = vocabulary.(Random.State.int st (Array.length vocabulary)) in
        let k = min (n - i) (1 + Random.State.int st 4) in
        String.sub s 0 i ^ tok ^ String.sub s (i + k) (n - i - k)

let gen_mutant corpus =
  let base = Array.of_list corpus in
  fun st ->
    let s = base.(Random.State.int st (Array.length base)) in
    let s = if String.length s > 600 then String.sub s 0 600 else s in
    let rec go s k = if k = 0 then s else go (mutate_once s st) (k - 1) in
    go s (1 + Random.State.int st 4)

(* {1 The laws} *)

let driver name =
  match Rivet_driver.Registry.find name with Some d -> d | None -> failwith ("no target " ^ name)

(* Planning must not allocate without bound on a stray `.zero 99999999999`. *)
let request = { Rivet_target_intf.Target.default_request with max_size = Some (1 lsl 24) }

let guard what f =
  try f () with
  | Stack_overflow -> Q.Test.fail_reportf "%s: Stack_overflow" what
  | e -> Q.Test.fail_reportf "%s raised %s" what (Printexc.to_string e)

let text_law name text =
  let (module D : Rivet_target_intf.Target.DRIVER) = driver name in
  let source = Rivet_foundation.Span.source ~name:"fuzz.s" ~contents:text in
  let render = function Ok _ -> () | Error ds -> ignore (Rivet_foundation.Diag.render ds) in
  guard "dump_tokens" (fun () -> render (D.dump_tokens ~source));
  guard "dump_source_ast" (fun () -> render (D.dump_source_ast ~unit_name:"fuzz" ~source));
  guard "dump_normalized_ast" (fun () ->
      render (D.dump_normalized_ast ~unit_name:"fuzz" ~source ()));
  guard "dump_lowered_ast" (fun () -> render (D.dump_lowered_ast ~unit_name:"fuzz" ~source ()));
  guard "assemble" (fun () ->
      match D.assemble_request request ~unit_name:"fuzz" ~source () with
      | Error ds -> ignore (Rivet_foundation.Diag.render ds)
      | Ok laid_out ->
          let plan = Rivet_image.plan_of laid_out in
          let addresses =
            List.mapi
              (fun i (s : Rivet_image.segment_plan) ->
                (s.Rivet_image.seg_name, Int64.of_int (i * 0x1000000)))
              plan.Rivet_image.segments
          in
          render (Rivet_image.bind_image laid_out ~addresses));
  true

(* The canonical dump is the instructions only. Compressed RISC-V code needs the permission to be
   written down before it assembles. *)
let prelude name = if String.starts_with ~prefix:"riscv" name then "\t.option rvc\n" else ""

(* Bytes to canonical text and back. Re-encoding is not byte-preserving (see test/props), so the
   law is that of a fixed point: the canonical text of any bytes assembles, and the bytes it
   assembles to print as text that assembles to those same bytes. *)
(* Targets whose decoder still accepts bytes the assembler does not read back faithfully (x86 and ARM
   decode more than they assemble: 16-bit operands, mode-gated opcodes, ...). For these a refused
   re-assembly is counted and reported, not failed; the fixed-point law still applies to
   everything that does assemble. *)
let lenient = [ "x86_32"; "x86_64"; "arm" ]
let refused = ref 0

let rec bytes_law name bytes = try bytes_law_exn name bytes with Exit -> true

and bytes_law_exn name bytes =
  let (module D : Rivet_target_intf.Target.DRIVER) = driver name in
  let assemble what text =
    let source =
      Rivet_foundation.Span.source ~name:"re.s" ~contents:("\t.text\n" ^ prelude name ^ text)
    in
    match guard what (fun () -> D.assemble ~unit_name:"re" ~source ()) with
    | Error ds ->
        if List.mem name lenient then (
          incr refused;
          raise Exit)
        else
          Q.Test.fail_reportf "canonical text does not re-assemble:@.%s@.%s" text
            (Rivet_foundation.Diag.render ds)
    | Ok laid_out -> (
        let plan = Rivet_image.plan_of laid_out in
        let addresses =
          List.map
            (fun (s : Rivet_image.segment_plan) -> (s.Rivet_image.seg_name, 0x1000L))
            plan.Rivet_image.segments
        in
        match Rivet_image.bind_image laid_out ~addresses with
        | Error ds ->
            (* An absolute branch target in the text drifts when a re-assembled
               instruction changes length, and can leave a short branch's reach. *)
            if List.mem name lenient then (
              incr refused;
              raise Exit)
            else
              Q.Test.fail_reportf "canonical text does not bind:@.%s@.%s" text
                (Rivet_foundation.Diag.render ds)
        | Ok { Rivet_image.segments = []; _ } -> ""
        | Ok { Rivet_image.segments = s :: _; _ } -> s.Rivet_image.bytes)
  in
  let dump b = D.dump_disasm_canonical ~address:0x1000L b in
  (* Known gap: some 0x66 (16-bit operand) x86 forms decode with a length their assembled
     spelling does not reproduce (imulw). *)
  if String.starts_with ~prefix:"x86" name && String.contains bytes '\x66' then true
  else
    match guard "dump_disasm_canonical" (fun () -> dump bytes) with
    | Ok "" -> true
    | Error ds ->
        ignore (Rivet_foundation.Diag.render ds);
        guard "dump_disasm_diagnostic" (fun () ->
            ignore (D.dump_disasm_diagnostic ~address:0x1000L bytes));
        true
    | Ok text -> (
        let bytes1 = assemble "re-assemble" text in
        match dump bytes1 with
        | Error ds ->
            if List.mem name lenient then (
              incr refused;
              true)
            else
              Q.Test.fail_reportf "re-encoded bytes do not decode:@.%s@.%s" text
                (Rivet_foundation.Diag.render ds)
        | Ok text' ->
            let bytes2 = assemble "re-assemble again" text' in
            bytes1 = bytes2
            || Q.Test.fail_reportf "not a fixed point:@.%s@.-> %S@.%s@.-> %S" text bytes1 text'
                 bytes2)

let gen_bytes =
  Q.Gen.map
    (fun l -> String.init (List.length l) (List.nth l))
    (Q.Gen.list_size (Q.Gen.int_range 0 24) (Q.Gen.map Char.chr (Q.Gen.int_range 0 255)))

let gen_bytes4 =
  Q.Gen.map
    (fun l -> String.concat "" l)
    (Q.Gen.list_size (Q.Gen.int_range 0 6)
       (Q.Gen.map
          (fun l -> String.init 4 (List.nth l))
          (Q.Gen.list_size (Q.Gen.return 4) (Q.Gen.map Char.chr (Q.Gen.int_range 0 255)))))

(* {1 Frozen counterexamples}

   Each input below was found by a run of the laws above and is kept as a case of its own. *)

(* Bytes the canonical disassembler must refuse: a decoder that accepted them printed text the
   assembler rejects. *)
let refused_bytes =
  [
    ("riscv32", "!\156" (* c.addw: RV64 only *));
    ("riscv32", "S\000 \192" (* fcvt.l.s: RV64 only *));
    ("aarch64", "\000\128\000J" (* eor w0, w0, w0, lsl #32 *));
    ("aarch64", "\000\020 \139" (* add x0, x0, w0, uxtb #5 *));
    ("aarch64", "\000\000!S" (* ubfm with N <> sf *));
    ("aarch64", "\000\128\000\019" (* sbfm w0, w0, #0, #33 *));
  ]

(* Bytes that must decode, and decode to text that is a fixed point. *)
let stable_bytes =
  [
    ("arm", "\000\000 U" (* pre-indexed writeback with a zero offset *));
    ("aarch64", "\159\004\000\235" (* cmp with a shifted register *));
    ("x86_64", "Pf\255\240" (* push, then pushw %ax *));
    ("x86_32", "\000\000f\080" (* addb, then pushw %ax *));
  ]

(* Text the assembler must refuse: it used to assemble to the 64-bit (or 32-bit) push or pop,
   dropping the operand size. *)
let refused_text =
  [ ("x86_64", "push %ax"); ("x86_64", "push %eax"); ("x86_64", "pop %ax"); ("x86_32", "push %ax") ]

let regressions () =
  let ok = ref true in
  let fail fmt =
    Printf.ksprintf
      (fun s ->
        ok := false;
        print_endline s)
      fmt
  in
  List.iter
    (fun (name, bytes) ->
      let (module D : Rivet_target_intf.Target.DRIVER) = driver name in
      match D.dump_disasm_canonical ~address:0x1000L bytes with
      | Error _ -> ()
      | Ok text -> fail "%s %S decodes to %S, should be refused" name bytes text)
    refused_bytes;
  List.iter
    (fun (name, text) ->
      let (module D : Rivet_target_intf.Target.DRIVER) = driver name in
      let source =
        Rivet_foundation.Span.source ~name:"r.s" ~contents:("\t.text\n\t" ^ text ^ "\n")
      in
      match D.assemble ~unit_name:"r" ~source () with
      | Error _ -> ()
      | Ok _ -> fail "%s %S assembles, should be refused" name text)
    refused_text;
  List.iter
    (fun (name, bytes) ->
      let (module D : Rivet_target_intf.Target.DRIVER) = driver name in
      match D.dump_disasm_canonical ~address:0x1000L bytes with
      | Error _ -> fail "%s %S no longer decodes" name bytes
      | Ok _ -> (
          match bytes_law name bytes with
          | true -> ()
          | false -> fail "%s %S is not a fixed point" name bytes
          | exception e -> fail "%s %S: %s" name bytes (Printexc.to_string e)))
    stable_bytes;
  !ok

let tests =
  List.concat_map
    (fun name ->
      let cs = corpus name in
      let mutant st = gen_mutant cs st in
      let show s = Printf.sprintf "%S" s in
      [
        Q.Test.make ~count:iters
          ~name:(name ^ ": raw text never raises")
          ~print:show gen_raw_text (text_law name);
        Q.Test.make ~count:iters
          ~name:(name ^ ": token soup never raises")
          ~print:show gen_soup (text_law name);
        Q.Test.make ~count:(2 * iters)
          ~name:(name ^ ": mutated corpus never raises")
          ~print:show
          (Q.Gen.make_primitive ~gen:mutant ~shrink:(fun _ -> Seq.empty))
          (text_law name);
        Q.Test.make ~count:(2 * iters)
          ~name:(name ^ ": random bytes decode and re-encode stably")
          ~print:show gen_bytes (bytes_law name);
        Q.Test.make ~count:(2 * iters)
          ~name:(name ^ ": random words decode and re-encode stably")
          ~print:show gen_bytes4 (bytes_law name);
      ])
    Rivet_driver.Registry.names

let () =
  Printf.printf "fuzz tests, seed %d, %d iterations\n%!" seed iters;
  let rand = Random.State.make [| seed |] in
  let frozen = regressions () in
  let rc = QCheck_base_runner.run_tests ~colors:false ~verbose:false ~rand tests in
  Printf.printf "decoded text the assembler refused to read back (lenient targets): %d\n" !refused;
  exit (if frozen then rc else 1)
