(* Do this assembler and GNU as produce the same bytes?

   test/differential asks that of one CompCert fixture per target. What it can
   establish is bounded by what those fixtures contain: 24 encoding forms, all
   of them a function prologue and epilogue. This file asks the same question
   over the whole corpus tools/asm-gas-xref.sh builds.

   Two trees, two different questions:

   - [generated] is written by snippet_emit from the AST corpus, through the
     same canonical printer test/snippets pins. Both assemblers must accept it
     and must agree, and a disagreement is attributable to the encoders because
     the input is not in question. A rejection here is a bug.

   - [frontier] is the assembly in this tree that a real toolchain reads: our
     own ABI helpers, and CompCert's runtime library. GNU as assembles all of
     it. This assembler mostly cannot, by construction - contracts.md §2.3
     freezes M1 at 24 forms - so a rejection is a boundary rather than a
     failure. What must never happen is the third outcome: both assemblers
     accepting a file and disagreeing about the bytes.

   The comparison is exact and unnormalized. There is no spelling normalization
   here of the kind test/differential needs for its disassembly comparison,
   because nothing is being compared as text: two assemblers were given one file
   and produced bytes, and the bytes either match or they do not.

   test/cram/gas_xref.t and gas_frontier.t ask the same of the *CLI*, and print
   the diagnostics. This file is the machine-checked half: it is the one that
   fails the build. *)

let corpus_root = "../../fixtures/gas-xref"

let read path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

(* [text.hex] is a whitespace-separated byte list in memory order wrapped at
   sixteen bytes a line. Read back as bytes rather than compared as text, so a
   difference in line wrapping is not reported as a difference in code - the
   same decision, and the same reason, as test/differential. *)
let bytes_of_hex_file path =
  let buf = Buffer.create 64 in
  String.split_on_char '\n' (read path)
  |> List.iter (fun line ->
      String.split_on_char ' ' line
      |> List.iter (fun tok ->
          let tok = String.trim tok in
          if tok <> "" then Buffer.add_char buf (Char.chr (int_of_string ("0x" ^ tok)))));
  Buffer.contents buf

(* The canonical target order tools/target-matrix.sh fixes, which is the order
   every generated artifact and manifest in this project uses. *)
let targets = [ "x86_32"; "x86_64"; "arm"; "aarch64"; "riscv32"; "riscv64" ]

let cases tree target =
  let dir = Filename.concat (Filename.concat corpus_root tree) target in
  if not (Sys.file_exists dir) then []
  else Array.to_list (Sys.readdir dir) |> List.sort String.compare

(* Assemble one case with this project's assembler and bind it at zero, which is
   the base the GNU oracle's objcopy dump is relative to. Every M1 case is
   relocation-free, so the base cannot matter - and if one day it does, this is
   where that shows up as a difference rather than as a silent success. *)
let our_bytes target source_text =
  match Driver.Registry.find target with
  | None -> Error (target ^ ": no such target")
  | Some (module D : Target_intf.Target.DRIVER) -> (
      let source = Foundation.Span.source ~name:(target ^ ".s") ~contents:source_text in
      match D.assemble ~unit_name:"asm_snippet" ~source () with
      | Error ds -> Error (String.trim (Foundation.Diag.render ds))
      | Ok laid_out -> (
          let plan = Image.plan_of laid_out in
          let addresses =
            List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0L)) plan.Image.segments
          in
          match Image.bind_image laid_out ~addresses with
          | Error ds -> Error (String.trim (Foundation.Diag.render ds))
          | Ok img -> (
              match img.Image.segments with
              | s :: _ -> Ok s.Image.bytes
              | [] -> Error "the image has no segment")))

type verdict = Agree | Differ of string | Rejected of string | Gas_rejected

let verdict_of tree target case =
  let dir = Filename.concat (Filename.concat (Filename.concat corpus_root tree) target) case in
  let gas_ok =
    let p = Filename.concat dir "gas.txt" in
    (not (Sys.file_exists p))
    || String.equal (String.trim (List.hd (String.split_on_char '\n' (read p)))) "ok"
  in
  if not gas_ok then Gas_rejected
  else
    let gnu = bytes_of_hex_file (Filename.concat dir "text.hex") in
    match our_bytes target (read (Filename.concat dir "input.s")) with
    | Error msg -> Rejected msg
    | Ok ours when String.equal ours gnu -> Agree
    | Ok ours -> Differ (Printf.sprintf "ours %s\n      gnu  %s" (hex ours) (hex gnu))

(* {1 The generated tree}

   Both assemblers must accept every case and must agree. Nothing here is a
   scope statement: this project wrote the input. *)

let%expect_test "generated corpus: our bytes against GNU as" =
  let agree = ref 0 and other = ref 0 in
  List.iter
    (fun target ->
      List.iter
        (fun case ->
          match verdict_of "generated" target case with
          | Agree ->
              incr agree;
              Printf.printf "%-8s %-16s agree\n" target case
          | Differ d ->
              incr other;
              Printf.printf "%-8s %-16s DIFFER\n      %s\n" target case d
          | Rejected msg ->
              (* Refusing to read back text we produced is a round-trip bug -
                 target.ml requires the canonical dump to be exactly
                 re-parseable - not a boundary. *)
              incr other;
              Printf.printf "%-8s %-16s REJECTED %s\n" target case msg
          | Gas_rejected ->
              incr other;
              Printf.printf "%-8s %-16s GNU as REJECTED OUR OWN OUTPUT\n" target case)
        (cases "generated" target))
    targets;
  Printf.printf "\n%d agree, %d not\n" !agree !other;
  [%expect
    {|
    x86_32   callee_clobber   agree
    x86_32   return41         agree
    x86_32   return42         agree
    x86_32   sp_align         agree
    x86_32   sp_corrupt       agree
    x86_32   spin             agree
    x86_32   stack_full       agree
    x86_32   stack_over       agree
    x86_32   trap             agree
    x86_64   callee_clobber   agree
    x86_64   return41         agree
    x86_64   return42         agree
    x86_64   sp_align         agree
    x86_64   sp_corrupt       agree
    x86_64   spin             agree
    x86_64   stack_full       agree
    x86_64   stack_over       agree
    x86_64   trap             agree
    arm      callee_clobber   agree
    arm      return41         agree
    arm      return42         agree
    arm      sp_align         agree
    arm      sp_corrupt       agree
    arm      spin             agree
    arm      stack_full       agree
    arm      stack_over       agree
    arm      trap             agree
    aarch64  callee_clobber   agree
    aarch64  return41         agree
    aarch64  return42         agree
    aarch64  sp_align         agree
    aarch64  sp_corrupt       agree
    aarch64  spin             agree
    aarch64  stack_full       agree
    aarch64  stack_over       agree
    aarch64  trap             agree
    riscv32  callee_clobber   agree
    riscv32  return41         agree
    riscv32  return42         agree
    riscv32  sp_align         agree
    riscv32  sp_corrupt       agree
    riscv32  spin             agree
    riscv32  stack_full       agree
    riscv32  stack_over       agree
    riscv32  trap             agree
    riscv64  callee_clobber   agree
    riscv64  return41         agree
    riscv64  return42         agree
    riscv64  sp_align         agree
    riscv64  sp_corrupt       agree
    riscv64  spin             agree
    riscv64  stack_full       agree
    riscv64  stack_over       agree
    riscv64  trap             agree

    54 agree, 0 not |}]

(* {1 The frontier tree}

   GNU as assembles all of it; this assembler mostly cannot, and that is the
   measured M1 boundary rather than a failure. The one outcome that must never
   appear is [DIFFER]: two assemblers that both accepted a file and produced
   different machine code.

   Counts and agreements only. test/cram/gas_frontier.t carries the per-file
   diagnostics, and repeating 47 of them here would be a second copy of one
   table - which is the thing this whole change exists to stop doing. *)

let%expect_test "frontier corpus: agreement wherever both assemblers accept" =
  let agree = ref 0 and beyond = ref 0 and differ = ref 0 and gas_no = ref 0 in
  List.iter
    (fun target ->
      List.iter
        (fun case ->
          match verdict_of "frontier" target case with
          | Agree ->
              incr agree;
              Printf.printf "%-8s %-24s agree\n" target case
          | Differ d ->
              incr differ;
              Printf.printf "%-8s %-24s DIFFER\n      %s\n" target case d
          | Rejected _ -> incr beyond
          | Gas_rejected -> incr gas_no)
        (cases "frontier" target))
    targets;
  Printf.printf "\n%d agree, %d differ, %d beyond M1, %d not assembled by GNU as\n" !agree !differ
    !beyond !gas_no;
  [%expect
    {|
    x86_32   fixture-asm_test_entry   agree
    x86_64   fixture-asm_test_entry   agree
    arm      fixture-asm_test_entry   agree
    aarch64  fixture-asm_test_entry   agree
    riscv32  fixture-asm_test_entry   agree
    riscv64  fixture-asm_test_entry   agree

    6 agree, 0 differ, 43 beyond M1, 0 not assembled by GNU as |}]
