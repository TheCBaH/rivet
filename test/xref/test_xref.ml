(* Do this assembler and GNU as produce the same bytes?

   test/differential asks that of one the compiler fixture per target. What it can
   establish is bounded by what those fixtures contain: 24 encoding forms, all
   of them a function prologue and epilogue. This file asks the same question
   over the whole corpus scripts/asm-gas-xref.sh builds.

   Two trees, two different questions:

   - [generated] is written by snippet_emit from the AST corpus, through the
     same canonical printer test/snippets pins. Both assemblers must accept it
     and must agree, and a disagreement is attributable to the encoders because
     the input is not in question. A rejection here is a bug.

   - [frontier] is the assembly in this tree that a real toolchain reads: our
     own ABI helpers, and the compiler's runtime library. GNU as assembles all of
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

(* The canonical target order scripts/target-matrix.sh fixes, which is the order
   every generated artifact and manifest in this project uses. *)
let targets = [ "x86_32"; "x86_64"; "arm"; "aarch64"; "riscv32"; "riscv64" ]

let cases tree target =
  let dir = Filename.concat (Filename.concat corpus_root tree) target in
  if not (Sys.file_exists dir) then []
  else Array.to_list (Sys.readdir dir) |> List.sort String.compare

(* Assemble one case with this project's assembler and bind its first segment at zero, which is
   the base the GNU oracle's objcopy dump is relative to; any further segment gets a slot of its
   own. Every case is relocation-free, so the base cannot matter - and if one day it does, this
   is where that shows up as a difference rather than as a silent success. *)
let our_bytes target source_text =
  match Driver.Registry.find target with
  | None -> Error (target ^ ": no such target")
  | Some (module D : Rivet_target_intf.Target.DRIVER) -> (
      let source = Rivet_foundation.Span.source ~name:(target ^ ".s") ~contents:source_text in
      match D.assemble ~unit_name:"asm_snippet" ~source () with
      | Error ds -> Error (String.trim (Rivet_foundation.Diag.render ds))
      | Ok laid_out -> (
          let plan = Rivet_image.plan_of laid_out in
          let addresses =
            List.mapi
              (fun i (s : Rivet_image.segment_plan) ->
                (s.Rivet_image.seg_name, Int64.of_int (i * 0x1000000)))
              plan.Rivet_image.segments
          in
          match Rivet_image.bind_image laid_out ~addresses with
          | Error ds -> Error (String.trim (Rivet_foundation.Diag.render ds))
          | Ok img -> (
              match img.Rivet_image.segments with
              | s :: _ -> Ok s.Rivet_image.bytes
              | [] -> Error "the image has no segment")))

type verdict = Agree | Differ of string | Rejected of string | Gas_rejected

(* A same-file, same-section branch/call to a *global* symbol still gets a linker
   relocation from real GNU as - R_AARCH64_JUMP26/CALL26, R_ARM_CALL/JUMP24 - because a
   global symbol can be interposed at link time, unlike a local label, even though the
   definition is already known. GNU's own raw object bytes at that word are therefore a
   placeholder (0), not the real displacement; the differential harness's own [our_bytes]
   has no such placeholder state to reproduce - [bind_image] always resolves every
   same-image reference to its real value, which is the entire point of a single-pass
   assembler that also links (M3, the corpus notes), and every fixture-oracle QEMU
   execution test depends on exactly that behavior. So this specific relocation family is
   the one place two conforming assemblers may legitimately disagree on the raw bytes -
   verified against real aarch64-linux-gnu-as/objdump: `b earlier_function` (a tail call
   to an earlier function in the same file) assembles to `14000000` (offset 0, deferred
   to `R_AARCH64_JUMP26 earlier_function`) under real `as`, and to the real, already-
   correct backward displacement under this project's own image binding. Every other
   relocation kind this corpus records (ADRP/ADD-lo12 pairs, PC-relative loads, ...) is
   *not* in this list: GNU computes and writes the real value for those directly, so a
   mismatch there stays a reportable [Differ] instead of being masked. *)
let deferred_relocation_kinds =
  [ "R_AARCH64_JUMP26"; "R_AARCH64_CALL26"; "R_ARM_CALL"; "R_ARM_JUMP24" ]

(* [objdump.txt] carries lines like ["\t\t\tb0: R_AARCH64_JUMP26\tsome_symbol"] -
   an offset, a colon, one of the [Elf_reloc] kind names, then the symbol. Extract the
   4-byte-word offsets of only the deferred-relocation kinds above. *)
let deferred_relocation_offsets path =
  if not (Sys.file_exists path) then []
  else
    String.split_on_char '\n' (read path)
    |> List.filter_map (fun line ->
        match String.index_opt line ':' with
        | None -> None
        | Some colon -> (
            let before = String.trim (String.sub line 0 colon) in
            let after =
              String.trim (String.sub line (colon + 1) (String.length line - colon - 1))
            in
            match int_of_string_opt ("0x" ^ before) with
            | None -> None
            | Some off ->
                if
                  List.exists
                    (fun k ->
                      String.length after >= String.length k
                      && String.sub after 0 (String.length k) = k)
                    deferred_relocation_kinds
                then Some off
                else None))

(* Zero the 4-byte word at each given offset in [s], the same fixed relocation-field width
   every kind in [deferred_relocation_kinds] uses (a whole aarch64/arm instruction word, or
   an x86 disp32 - never checked against a target that needs a different width here). Out-
   of-range offsets are ignored rather than raising, so a stale or unrelated recorded
   relocation can never crash the comparison. *)
let mask_words s offsets =
  let b = Bytes.of_string s in
  List.iter
    (fun off ->
      for k = 0 to 3 do
        if off + k >= 0 && off + k < Bytes.length b then Bytes.set b (off + k) '\000'
      done)
    offsets;
  Bytes.to_string b

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
    | Ok ours when tree = "frontier" -> (
        let offsets = deferred_relocation_offsets (Filename.concat dir "objdump.txt") in
        match offsets with
        | [] -> Differ (Printf.sprintf "ours %s\n      gnu  %s" (hex ours) (hex gnu))
        | _ :: _ ->
            if String.equal (mask_words ours offsets) (mask_words gnu offsets) then Agree
            else Differ (Printf.sprintf "ours %s\n      gnu  %s" (hex ours) (hex gnu)))
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

(* {1 The directives tree}

   Hand-written, instruction-light cases from test/xref/directives, assembled by
   GNU as for all six targets (only .text is compared, so each case puts what it
   checks there). A case is expected to agree, except those named [diff-*]:
   documented differences from GNU as, such as arbitrary-precision constant
   folding against GNU's 64-bit wrapping (docs/contracts.md). Those print both
   sides, so a change in either assembler shows as a changed transcript. *)

let%expect_test "directive corpus: agreement, and the documented differences" =
  let agree = ref 0 and bad = ref 0 in
  let is_diff case = String.length case > 5 && String.sub case 0 5 = "diff-" in
  List.iter
    (fun target ->
      List.iter
        (fun case ->
          match (is_diff case, verdict_of "directives" target case) with
          | false, Agree -> incr agree
          | true, Differ d -> Printf.printf "%-8s %-22s DIFFER\n      %s\n" target case d
          | true, Rejected m ->
              Printf.printf "%-8s %-22s REJECTED %s\n" target case
                (List.hd (String.split_on_char '\n' m))
          | _, v ->
              incr bad;
              Printf.printf "%-8s %-22s UNEXPECTED %s\n" target case
                (match v with
                | Agree -> "agreement"
                | Differ d -> "difference " ^ d
                | Rejected m -> "rejection " ^ m
                | Gas_rejected -> "GNU as rejected it"))
        (cases "directives" target))
    targets;
  Printf.printf "\n%d agree, %d unexpected\n" !agree !bad;
  [%expect
    {|
    x86_32   diff-add-wraps         REJECTED x86_32.s:2:2: error[lower.data]: value does not fit 64 bits
    x86_32   diff-byte-too-wide     REJECTED x86_32.s:2:2: error[lower.data]: value 4660 does not fit in 1 byte
    x86_32   diff-large-intermediate DIFFER
          ours 04 00 00 00 00 00 00 00
          gnu  00 00 00 00 00 00 00 00
    x86_32   diff-shl-past-64       REJECTED x86_32.s:2:2: error[lower.data]: value does not fit 64 bits
    x86_32   diff-shr-minus-one     DIFFER
          ours ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff
          gnu  01 00 00 00 00 00 00 00 ff ff ff ff ff ff ff 7f
    x86_32   diff-shr-negative      DIFFER
          ours fc ff ff ff ff ff ff ff
          gnu  fc ff ff ff ff ff ff 3f
    x86_32   diff-sub-wraps         REJECTED x86_32.s:2:2: error[lower.data]: value does not fit 64 bits
    x86_64   diff-add-wraps         REJECTED x86_64.s:2:2: error[lower.data]: value does not fit 64 bits
    x86_64   diff-byte-too-wide     REJECTED x86_64.s:2:2: error[lower.data]: value 4660 does not fit in 1 byte
    x86_64   diff-large-intermediate DIFFER
          ours 04 00 00 00 00 00 00 00
          gnu  00 00 00 00 00 00 00 00
    x86_64   diff-shl-past-64       REJECTED x86_64.s:2:2: error[lower.data]: value does not fit 64 bits
    x86_64   diff-shr-minus-one     DIFFER
          ours ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff
          gnu  01 00 00 00 00 00 00 00 ff ff ff ff ff ff ff 7f
    x86_64   diff-shr-negative      DIFFER
          ours fc ff ff ff ff ff ff ff
          gnu  fc ff ff ff ff ff ff 3f
    x86_64   diff-sub-wraps         REJECTED x86_64.s:2:2: error[lower.data]: value does not fit 64 bits
    arm      diff-add-wraps         REJECTED arm.s:2:2: error[lower.data]: value does not fit 64 bits
    arm      diff-byte-too-wide     REJECTED arm.s:2:2: error[lower.data]: value 4660 does not fit in 1 byte
    arm      diff-large-intermediate DIFFER
          ours 04 00 00 00 00 00 00 00
          gnu  00 00 00 00 00 00 00 00
    arm      diff-shl-past-64       REJECTED arm.s:2:2: error[lower.data]: value does not fit 64 bits
    arm      diff-shr-minus-one     DIFFER
          ours ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff
          gnu  01 00 00 00 00 00 00 00 ff ff ff ff ff ff ff 7f
    arm      diff-shr-negative      DIFFER
          ours fc ff ff ff ff ff ff ff
          gnu  fc ff ff ff ff ff ff 3f
    arm      diff-sub-wraps         REJECTED arm.s:2:2: error[lower.data]: value does not fit 64 bits
    aarch64  diff-add-wraps         REJECTED aarch64.s:2:2: error[lower.data]: value does not fit 64 bits
    aarch64  diff-byte-too-wide     REJECTED aarch64.s:2:2: error[lower.data]: value 4660 does not fit in 1 byte
    aarch64  diff-large-intermediate DIFFER
          ours 04 00 00 00 00 00 00 00
          gnu  00 00 00 00 00 00 00 00
    aarch64  diff-shl-past-64       REJECTED aarch64.s:2:2: error[lower.data]: value does not fit 64 bits
    aarch64  diff-shr-minus-one     DIFFER
          ours ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff
          gnu  01 00 00 00 00 00 00 00 ff ff ff ff ff ff ff 7f
    aarch64  diff-shr-negative      DIFFER
          ours fc ff ff ff ff ff ff ff
          gnu  fc ff ff ff ff ff ff 3f
    aarch64  diff-sub-wraps         REJECTED aarch64.s:2:2: error[lower.data]: value does not fit 64 bits
    riscv32  diff-add-wraps         REJECTED riscv32.s:2:2: error[lower.data]: value does not fit 64 bits
    riscv32  diff-byte-too-wide     REJECTED riscv32.s:2:2: error[lower.data]: value 4660 does not fit in 1 byte
    riscv32  diff-large-intermediate DIFFER
          ours 04 00 00 00 00 00 00 00
          gnu  00 00 00 00 00 00 00 00
    riscv32  diff-shl-past-64       REJECTED riscv32.s:2:2: error[lower.data]: value does not fit 64 bits
    riscv32  diff-shr-minus-one     DIFFER
          ours ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff
          gnu  01 00 00 00 00 00 00 00 ff ff ff ff ff ff ff 7f
    riscv32  diff-shr-negative      DIFFER
          ours fc ff ff ff ff ff ff ff
          gnu  fc ff ff ff ff ff ff 3f
    riscv32  diff-sub-wraps         REJECTED riscv32.s:2:2: error[lower.data]: value does not fit 64 bits
    riscv64  diff-add-wraps         REJECTED riscv64.s:2:2: error[lower.data]: value does not fit 64 bits
    riscv64  diff-byte-too-wide     REJECTED riscv64.s:2:2: error[lower.data]: value 4660 does not fit in 1 byte
    riscv64  diff-large-intermediate DIFFER
          ours 04 00 00 00 00 00 00 00
          gnu  00 00 00 00 00 00 00 00
    riscv64  diff-shl-past-64       REJECTED riscv64.s:2:2: error[lower.data]: value does not fit 64 bits
    riscv64  diff-shr-minus-one     DIFFER
          ours ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff
          gnu  01 00 00 00 00 00 00 00 ff ff ff ff ff ff ff 7f
    riscv64  diff-shr-negative      DIFFER
          ours fc ff ff ff ff ff ff ff
          gnu  fc ff ff ff ff ff ff 3f
    riscv64  diff-sub-wraps         REJECTED riscv64.s:2:2: error[lower.data]: value does not fit 64 bits

    143 agree, 0 unexpected |}]

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
    riscv32  helper-helper            DIFFER
          ours b7 02 00 03 93 82 02 00 23 a0 d2 00 b7 02 00 03 93 82 c2 00 23 a0 22 00 b7 02 00 03 93 82 82 00 23 a0 12 00 b7 02 00 02 93 82 02 00 23 a0 82 00 23 a2 92 00 23 a4 22 01 23 a6 32 01 23 a8 42 01 23 aa 52 01 23 ac 62 01 23 ae 72 01 23 a0 82 03 23 a2 92 03 23 a4 a2 03 23 a6 b2 03 b7 02 00 03 93 82 42 00 23 a0 b2 00 13 04 10 18 93 04 20 19 13 09 30 1a 93 09 40 1b 13 0a 50 1c 93 0a 60 1d 13 0b 70 1e 93 0b 80 1f 13 0c 90 20 93 0c a0 21 13 0d b0 22 93 0d c0 23 13 0e 05 00 13 81 05 00 93 02 10 00 23 15 56 00 0f 00 10 03 e7 00 0e 00 13 0e 05 00 93 0e 01 00 b7 02 00 03 93 82 02 01 03 a3 02 00 93 02 30 00 23 2e 53 0c 0f 00 10 03 13 0f 00 00 93 02 10 18 63 1e 54 04 93 02 20 19 63 9a 54 04 93 02 30 1a 63 16 59 04 93 02 40 1b 63 92 59 04 93 02 50 1c 63 1e 5a 02 93 02 60 1d 63 9a 5a 02 93 02 70 1e 63 16 5b 02 93 02 80 1f 63 92 5b 02 93 02 90 20 63 1e 5c 00 93 02 a0 21 63 9a 5c 00 93 02 b0 22 63 16 5d 00 93 02 c0 23 63 84 5d 00 13 0f 10 00 b7 02 00 03 93 82 c2 00 03 a1 02 00 b7 02 00 02 93 82 02 00 03 a4 02 00 83 a4 42 00 03 a9 82 00 83 a9 c2 00 03 aa 02 01 83 aa 42 01 03 ab 82 01 83 ab c2 01 03 ac 02 02 83 ac 42 02 03 ad 82 02 83 ad c2 02 b7 02 00 03 93 82 82 00 83 a0 02 00 b7 02 00 03 93 82 02 00 83 a2 02 00 23 a0 c2 01 23 a2 d2 01 23 a4 e2 01 67 80 00 00 03 46 15 00 83 46 55 00 03 43 05 00 03 47 25 00 83 48 45 00 83 47 65 00 83 45 75 00 03 48 35 00 13 16 86 00 93 96 86 00 33 66 66 00 b3 e6 16 01 13 17 07 01 93 97 07 01 33 67 c7 00 13 15 88 01 b3 e7 d7 00 93 95 85 01 33 65 e5 00 b3 e5 f5 00 67 80 00 00 93 05 00 00 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 e0 05 73 00 00 00 73 00 10 00 73 00 10 00 6f f0 9f ff b7 07 00 03 83 a7 07 01 13 01 01 ff 23 26 11 00 13 07 10 00 23 8d b7 0c a3 8d 07 0c a3 8e 07 0c 23 8f 07 0c a3 8f 07 0c 23 8e e7 0c 0f 00 10 03 23 8c a7 0c a3 8c 07 0c 97 00 00 00 e7 80 00 fa 83 26 05 00 13 01 01 d1 23 26 11 2e 23 24 81 2e 93 07 20 00 63 f4 d7 16 93 97 26 00 93 87 87 00 b3 07 f5 00 03 a7 07 00 13 03 05 00 63 08 07 00 03 a7 47 00 93 87 47 00 e3 1c 07 fe 03 a8 47 00 13 0e 00 00 93 87 47 00 63 04 08 02 13 06 00 00 13 07 60 00 63 16 e8 00 03 a6 47 00 13 0e 10 00 03 a8 87 00 93 87 87 00 e3 16 08 fe 13 08 06 00 93 07 30 00 63 84 f6 06 03 26 03 01 b7 07 00 01 93 87 07 00 b3 05 f6 40 37 05 00 01 33 87 b7 00 03 47 07 00 83 c6 07 00 93 87 17 00 63 98 e6 00 13 07 85 00 e3 94 e7 fe 37 28 00 00 b7 07 00 01 93 87 c7 00 33 06 f6 40 b7 05 00 01 33 87 c7 00 03 47 07 00 83 c6 07 00 93 87 17 00 63 98 e6 00 13 87 45 01 e3 94 e7 fe 13 0e 00 00 83 25 c3 00 13 05 c0 f9 13 06 20 0c 93 06 00 18 13 07 00 00 93 07 00 00 93 08 80 03 73 00 00 00 b7 fe ff ff 13 04 05 00 63 ec ae 06 b7 15 00 00 13 06 00 00 93 06 00 00 93 08 e0 02 73 00 00 00 63 e0 ae 06 b7 06 10 00 23 22 91 2e 93 86 26 02 37 05 00 30 b7 05 10 00 13 07 f0 ff 93 08 e0 0d 73 00 00 00 37 07 00 30 63 08 e5 06 13 05 20 04 23 20 21 2f 23 2e 31 2d 23 2c 41 2d 23 2a 51 2d 23 28 61 2d 23 26 71 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 40 e2 13 05 10 04 23 22 91 2e 23 20 21 2f 23 2e 31 2d 23 2c 41 2d 23 2a 51 2d 23 28 61 2d 23 26 71 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 c0 de 13 05 00 00 b7 15 00 00 13 06 30 00 93 06 10 00 13 07 04 00 73 00 00 00 93 04 05 00 e3 ec ae f6 23 28 61 2d 37 06 00 04 37 0b 00 03 23 20 21 2f 23 28 ab 00 13 06 c6 00 93 07 00 00 93 05 80 00 33 07 f6 00 83 46 07 00 33 87 f4 00 93 87 17 00 23 00 d7 00 e3 96 b7 fe 13 09 20 00 a3 84 04 00 23 84 24 01 23 2e 31 2d 23 2c 41 2d 63 0e 0e 0a 23 26 71 2d b7 17 00 00 63 04 f8 02 93 05 10 00 13 05 50 04 23 2a 51 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 40 d7 83 25 83 00 13 05 c0 f9 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 80 03 73 00 00 00 37 fa ff ff 93 09 05 00 63 64 aa 08 b7 05 00 04 13 06 08 00 93 85 85 00 93 06 00 20 13 07 01 03 93 08 30 12 73 00 00 00 63 66 aa 08 13 05 81 05 97 00 00 00 e7 80 00 ca 23 24 b1 00 23 2a 51 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 93 0b 05 00 63 82 05 08 93 05 20 00 13 05 00 04 97 00 00 00 e7 80 c0 ce 93 05 09 00 13 05 50 04 23 2a 51 2d 23 26 71 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 40 cc 93 05 30 00 13 05 10 04 23 2a 51 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 00 ca 93 05 40 00 13 05 10 04 23 2a 51 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 c0 c7 93 07 70 07 63 f4 a7 08 b7 07 10 00 e3 ea a7 f6 93 06 09 00 13 87 09 00 13 05 00 00 93 85 0b 00 13 06 10 00 93 07 00 00 93 08 e0 0d 73 00 00 00 13 09 05 00 63 64 aa 06 37 06 00 04 13 06 06 00 93 07 00 00 93 05 80 00 b3 06 f9 00 33 07 f6 00 83 c6 06 00 03 47 07 00 63 9a e6 04 93 87 17 00 e3 94 b7 fe 03 4c 99 00 83 46 89 00 13 07 20 00 13 1c 8c 00 33 6c dc 00 63 02 ec 04 93 05 40 00 13 05 00 04 97 00 00 00 e7 80 00 bf 93 05 10 00 13 05 00 04 97 00 00 00 e7 80 00 be 93 05 50 00 13 05 10 04 97 00 00 00 e7 80 00 bd 93 05 30 00 13 05 00 04 97 00 00 00 e7 80 00 bc 03 47 b9 00 83 46 a9 00 93 05 50 00 13 17 87 00 33 67 d7 00 e3 10 b7 fe 03 46 d9 00 83 45 c9 00 83 46 e9 00 03 47 f9 00 13 16 86 00 33 66 b6 00 93 96 06 01 b3 e6 c6 00 13 17 87 01 33 67 d7 00 93 05 60 00 e3 18 77 fb 03 4a 59 01 03 47 49 01 93 05 70 00 13 1a 8a 00 33 6a ea 00 13 07 fa ff e3 ea e5 f8 03 47 79 01 03 46 69 01 93 06 10 00 13 17 87 00 33 67 c7 00 63 0a d7 00 93 85 07 00 13 05 00 04 97 00 00 00 e7 80 40 b3 83 46 99 02 03 46 89 02 03 47 a9 02 83 47 b9 02 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 93 97 87 01 b3 e7 e7 00 13 07 00 10 63 8a e7 00 93 05 90 00 13 05 00 04 97 00 00 00 e7 80 40 af 83 46 19 03 03 46 09 03 03 47 29 03 83 47 39 03 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 93 97 87 01 b3 e7 e7 00 37 f7 00 00 93 87 c7 f9 13 07 c7 9f 63 6c f7 22 03 47 59 03 83 46 49 03 83 47 69 03 83 4d 79 03 13 17 87 00 33 67 d7 00 93 97 07 01 b3 e7 e7 00 93 9d 8d 01 b3 ed fd 00 63 9e 0d 20 93 07 09 04 93 0c 89 07 93 06 00 00 03 c7 07 00 93 87 17 00 b3 e6 e6 00 e3 9a 97 ff 63 92 06 4e 93 09 80 02 b3 09 3a 03 13 06 00 00 93 89 89 07 63 e0 3b 4d 93 0a 01 13 13 8d 0a 00 13 0e 00 00 13 85 0c 00 23 2e c1 00 23 2c d1 00 23 2a c1 01 97 00 00 00 e7 80 c0 9b 13 07 05 00 23 20 ed 00 13 85 8c 00 23 22 bd 00 97 00 00 00 e7 80 40 9a 93 08 05 00 23 24 1d 01 13 85 0c 01 23 26 bd 00 23 26 11 01 23 28 b1 00 97 00 00 00 e7 80 40 98 13 07 05 00 23 28 ed 00 13 85 8c 01 23 2a bd 00 97 00 00 00 e7 80 c0 96 83 c7 1c 02 03 cf 0c 02 83 28 c1 00 93 97 87 00 83 ce 2c 02 13 07 05 00 33 e5 e7 01 83 c7 3c 02 33 8f e8 00 03 23 01 01 93 9e 0e 01 b3 ee ae 00 13 95 87 01 b3 37 1f 01 83 c8 4c 02 33 03 b3 00 33 65 d5 01 23 2e bd 00 23 2c ed 00 b3 87 67 00 23 20 ad 02 23 02 1d 03 63 9e 07 42 63 ec eb 43 63 9a 05 42 03 2e 41 01 83 26 81 01 03 26 c1 01 63 62 ef 42 13 0e 1e 00 93 8c 8c 02 13 0d 0d 03 e3 6c 4e f1 93 87 0a 00 13 05 00 00 03 a7 87 01 03 a8 c7 01 93 75 77 00 63 94 05 0e b3 65 07 01 63 84 05 00 63 00 06 41 03 a7 87 00 03 a8 c7 00 b3 35 b0 00 33 67 07 01 33 37 e0 00 63 9a e5 0c 13 05 15 00 93 87 07 03 e3 62 45 fd 93 85 0a 00 13 08 00 00 6f 00 c0 00 93 85 05 03 63 72 48 0d 83 a7 85 00 03 a7 c5 00 13 08 18 00 33 e5 e7 00 e3 04 05 fe 03 ae 85 01 83 a8 c5 01 13 85 05 00 33 83 c7 01 b3 37 f3 00 33 07 17 01 33 87 e7 00 93 0e 08 00 e3 f2 4e fd 83 27 85 03 83 23 c5 03 33 ef 77 00 63 08 0f 02 83 2f 85 04 03 2f c5 04 b3 82 f7 01 b3 b7 f2 00 b3 83 e3 01 b3 87 77 00 63 e6 f8 00 63 98 17 01 63 76 5e 00 63 64 ef 10 63 00 e7 11 93 8e 1e 00 13 05 05 03 6f f0 9f fb 93 05 a0 00 13 05 00 04 97 00 00 00 e7 80 80 87 93 05 b0 00 13 05 00 04 97 00 00 00 e7 80 80 86 93 05 f0 00 13 05 00 04 97 00 00 00 e7 80 80 85 93 05 10 01 13 05 00 04 97 00 00 00 e7 80 80 84 13 87 0a 00 93 08 00 00 6f 00 00 01 93 88 18 00 13 07 07 03 63 fa 48 05 83 25 87 00 03 25 c7 00 b3 e7 a5 00 e3 84 07 fe 03 23 87 01 93 87 79 00 33 b8 37 01 93 f7 87 ff 33 06 c8 00 63 16 f3 28 03 28 c7 01 63 12 c8 28 b3 09 39 01 33 03 f9 00 13 08 00 00 63 82 69 28 03 ce 09 00 93 89 19 00 33 68 c8 01 6f f0 1f ff 93 87 79 00 33 b7 37 01 93 f7 87 ff 33 07 c7 00 63 96 fb 00 83 27 81 00 63 8a e7 00 93 05 50 01 13 05 00 04 97 f0 ff ff e7 80 40 7b b3 09 39 01 b3 0b 79 01 63 84 79 03 83 c7 09 00 93 89 19 00 b3 e6 f6 00 6f f0 1f ff e3 f2 6f f0 93 05 20 01 13 05 00 04 97 f0 ff ff e7 80 40 78 63 98 06 2c 93 87 0a 00 13 07 00 00 37 06 04 00 b7 06 10 00 83 a5 c7 00 63 94 05 2a 83 a5 87 00 63 60 b6 2a 83 a5 47 01 63 94 05 28 83 a5 07 01 63 e0 b6 28 13 07 17 00 93 87 07 03 e3 6c 47 fd 37 13 00 00 13 03 f3 ff 93 87 0a 00 93 0e 70 00 13 0e 60 00 83 a6 87 00 03 a7 07 01 83 a5 c7 00 03 a6 47 01 33 e5 e6 00 33 e8 c5 00 33 65 05 01 63 08 05 22 83 a9 07 02 63 8c 09 20 13 85 f9 ff b3 f9 a9 00 63 96 09 20 03 a8 07 00 83 a8 47 00 33 75 05 01 63 16 05 1e 03 c5 47 02 63 e2 ae 24 13 75 65 00 63 06 c5 1d 33 8f 06 01 b3 85 15 01 b3 36 df 00 33 05 e7 01 b3 86 b6 00 b3 05 d6 00 33 37 e5 00 33 06 65 00 33 07 b7 00 b3 35 a6 00 b3 85 e5 00 63 e2 e5 18 63 14 b7 00 63 6e a6 16 b7 ff ff ff 33 76 f6 01 63 e8 16 17 63 8e d8 16 63 64 d7 16 63 80 e6 16 37 f7 ff ff 33 77 e8 00 63 88 08 1e 63 9a 05 1e b7 06 10 30 63 e6 c6 1e 23 a4 e7 02 23 a6 c7 02 93 8d 1d 00 93 87 07 03 e3 ea 4d f3 13 05 09 02 97 f0 ff ff e7 80 80 5d 83 46 d9 02 03 48 c9 02 03 47 e9 02 93 0c 05 00 37 f6 ff ff 03 45 f9 02 33 f6 cc 00 93 96 86 00 b7 17 00 00 b3 e6 06 01 13 17 07 01 b3 07 f6 00 33 67 d7 00 13 15 85 01 b3 b6 c7 00 b3 6b e5 00 b3 86 b6 00 37 07 04 30 13 8d 05 00 33 87 eb 00 63 e4 b6 18 63 94 d5 00 63 e0 c7 18 93 86 0a 00 93 05 00 00 b7 08 04 30 37 f3 03 30 03 a8 86 02 03 a5 c6 02 63 74 f8 00 63 64 a6 16 63 74 e8 00 63 e8 a8 16 63 74 18 01 63 6c a3 16 93 85 15 00 93 86 06 03 e3 ec 45 fd 63 7c e6 16 37 07 04 30 63 78 f7 16 93 05 20 02 13 05 00 04 97 f0 ff ff e7 80 00 5a 93 05 d0 00 13 05 00 04 97 f0 ff ff e7 80 00 59 93 05 c0 00 13 05 00 04 97 f0 ff ff e7 80 00 58 93 05 30 01 13 05 00 04 97 f0 ff ff e7 80 00 57 63 1e 08 0a b3 87 f5 00 b3 b5 b7 00 33 05 c5 00 93 89 07 00 33 86 a5 00 6f f0 df d1 93 05 e0 00 13 05 00 04 97 f0 ff ff e7 80 40 54 e3 72 37 c1 93 05 00 01 13 05 00 04 97 f0 ff ff e7 80 00 53 e3 72 e5 eb 93 05 d0 01 13 05 00 04 97 f0 ff ff e7 80 c0 51 e3 68 0f ff 6f f0 5f e8 93 05 c0 01 13 05 00 04 97 f0 ff ff e7 80 40 50 93 05 a0 01 13 05 00 04 97 f0 ff ff e7 80 40 4f 93 05 90 01 13 05 00 04 97 f0 ff ff e7 80 40 4e 93 05 80 01 13 05 00 04 97 f0 ff ff e7 80 40 4d 93 05 70 01 13 05 00 04 97 f0 ff ff e7 80 40 4c 93 05 60 01 13 05 00 04 97 f0 ff ff e7 80 40 4b 93 05 40 01 13 05 00 04 97 f0 ff ff e7 80 40 4a 93 05 b0 01 13 05 00 04 97 f0 ff ff e7 80 40 49 b7 06 00 30 e3 78 d7 e0 93 05 e0 01 13 05 00 04 97 f0 ff ff e7 80 c0 47 93 07 f0 ff 6f f0 1f e8 93 05 f0 01 13 05 00 04 97 f0 ff ff e7 80 40 46 93 05 00 02 13 05 00 04 97 f0 ff ff e7 80 40 45 93 05 10 02 13 05 00 04 97 f0 ff ff e7 80 40 44 93 87 0a 00 93 89 19 00 93 86 09 00 13 87 07 00 63 f4 46 03 03 a8 87 02 03 25 c7 05 83 a5 c7 02 03 26 87 05 63 74 a8 00 63 60 b6 08 93 86 16 00 13 07 07 03 6f f0 df fd 93 87 07 03 e3 e4 49 fd 83 46 d9 01 03 46 c9 01 03 47 e9 01 83 47 f9 01 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 93 97 87 01 b3 e7 e7 00 63 98 07 02 03 47 59 02 83 46 49 02 83 47 69 02 83 4d 79 02 13 17 87 00 33 67 d7 00 93 97 07 01 b3 e7 e7 00 93 9d 8d 01 b3 ed fd 00 63 82 0d 02 93 05 a0 02 13 05 00 04 97 f0 ff ff e7 80 c0 39 93 05 30 02 13 05 00 04 97 f0 ff ff e7 80 c0 38 93 09 89 01 13 85 09 00 97 f0 ff ff e7 80 00 30 13 07 05 00 93 86 0a 00 13 08 00 00 83 c7 46 02 93 f7 47 00 63 82 07 04 03 a6 46 00 83 a7 06 00 63 ec c5 02 63 14 b6 00 63 68 f7 02 03 a5 86 00 03 a3 c6 00 93 08 10 00 33 85 a7 00 b3 37 f5 00 33 06 66 00 b3 87 c7 00 63 e6 f5 00 63 80 b7 08 93 08 00 00 33 68 18 01 93 8d 1d 00 93 86 06 03 e3 e6 4d fb 63 0c 08 04 13 77 37 00 63 10 07 04 b7 15 00 00 13 88 f5 ff b3 f7 0c 01 63 90 07 02 b7 07 03 30 63 94 fc 00 63 08 0d 04 93 05 70 02 13 05 00 04 97 f0 ff ff e7 80 00 2e 93 05 60 02 13 05 00 04 97 f0 ff ff e7 80 00 2d 93 05 50 02 13 05 00 04 97 f0 ff ff e7 80 00 2c 93 05 40 02 13 05 00 04 97 f0 ff ff e7 80 00 2b e3 72 a7 f8 33 68 18 01 6f f0 5f f8 93 87 fb ff 37 07 04 00 63 f6 e7 00 33 f8 0b 01 63 0a 08 00 93 05 80 02 13 05 00 04 97 f0 ff ff e7 80 00 28 13 85 0c 00 13 07 04 00 13 06 30 00 93 06 10 01 93 07 00 00 93 08 e0 0d 73 00 00 00 37 f3 ff ff 63 68 a3 2a 83 46 d9 02 03 45 c9 02 03 47 e9 02 83 45 f9 02 93 96 86 00 b3 e6 a6 00 13 17 07 01 33 67 d7 00 93 95 85 01 b3 e5 e5 00 37 05 04 30 93 06 20 03 13 07 f0 ff 73 00 00 00 63 68 a3 2e 37 f5 03 30 b7 15 00 00 13 06 00 00 93 06 20 03 13 07 f0 ff 93 07 00 00 93 08 e0 0d 73 00 00 00 b7 f7 ff ff 63 ec a7 28 13 8e 07 00 13 83 0a 00 93 0e 00 00 83 25 c3 02 03 25 83 02 13 06 30 00 93 06 20 03 b3 85 a5 40 13 07 f0 ff 93 07 00 00 93 08 e0 0d 73 00 00 00 63 64 ae 22 93 8e 1e 00 13 03 03 03 e3 e8 4e fd 93 87 0a 00 93 06 00 00 83 a5 87 00 03 a7 c7 00 33 e7 e5 00 63 1a 07 20 93 86 16 00 93 87 07 03 e3 e4 46 ff 13 83 0a 00 13 0e 00 00 b7 fe ff ff 83 47 43 02 13 f7 27 00 13 f6 17 00 63 04 07 00 13 66 26 00 93 f7 47 00 63 84 07 00 13 66 46 00 83 25 c3 02 03 25 83 02 93 06 00 00 13 07 00 00 b3 85 a5 40 93 07 00 00 93 08 20 0e 73 00 00 00 63 ea ae 1e 13 0e 1e 00 13 03 03 03 e3 6a 4e fb 37 f3 ff ff 83 c7 4a 02 93 f7 47 00 63 84 07 02 03 a5 8a 02 83 a5 ca 02 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 30 10 73 00 00 00 63 62 a3 1c 13 08 18 00 93 8a 0a 03 e3 64 48 fd 83 46 19 01 03 46 09 01 03 47 29 01 83 47 39 01 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 93 97 87 01 b3 e7 e7 00 13 d6 87 00 93 d6 07 01 13 d7 87 01 13 0a 10 00 93 0a 89 03 23 86 f4 00 a3 86 c4 00 23 87 d4 00 a3 87 e4 00 a3 88 04 00 23 88 44 01 13 85 0a 00 97 f0 ff ff e7 80 c0 01 13 87 05 00 93 07 05 00 93 58 85 00 13 58 05 01 93 d5 85 00 13 55 85 01 13 56 07 01 93 56 87 01 23 8e e4 00 23 89 04 00 a3 89 04 00 23 8c f4 00 a3 8c 14 01 23 8d 04 01 a3 8d a4 00 a3 8e b4 00 23 8f c4 00 a3 8f d4 00 0f 00 10 03 93 0b 20 00 a3 8e 04 0c 23 8f 04 0c a3 8f 04 0c 13 85 09 00 23 8e 74 0d 97 f0 ff ff e7 80 00 fb 83 46 d9 02 03 46 c9 02 03 47 e9 02 83 47 f9 02 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 03 26 0b 01 93 97 87 01 b3 e7 e7 00 b7 05 04 30 b3 85 b7 00 93 06 41 02 97 f0 ff ff e7 80 c0 dc b7 07 00 03 03 24 41 02 03 27 81 02 83 a7 47 00 13 59 f4 41 63 0a f7 0a 83 27 0b 01 23 8d 47 0d a3 8d 07 0c 0f 00 10 03 13 05 40 04 a3 8c 07 0c 23 8c a7 0c 97 f0 ff ff e7 80 80 f8 93 05 06 00 13 05 20 04 97 f0 ff ff e7 80 00 fa 93 05 60 00 13 05 20 04 97 f0 ff ff e7 80 00 f9 83 a8 07 00 03 a5 87 01 13 07 00 00 33 03 17 01 e3 80 e5 de 33 06 a7 00 33 06 c9 00 03 46 06 00 13 07 17 00 23 00 c3 00 6f f0 5f fe 93 05 50 00 13 05 20 04 97 f0 ff ff e7 80 40 f5 93 05 70 00 13 05 20 04 97 f0 ff ff e7 80 40 f4 93 05 30 00 13 05 30 04 97 f0 ff ff e7 80 40 f3 93 05 40 00 13 05 20 04 97 f0 ff ff e7 80 40 f2 83 27 c1 02 63 88 07 00 83 27 0b 01 23 8d 77 0d 6f f0 9f f4 13 55 84 00 93 55 04 01 13 56 84 01 93 56 89 00 13 57 09 01 93 57 89 01 23 8c 84 04 23 8e 24 05 23 89 44 01 a3 89 04 00 23 8a 04 00 a3 8a 04 00 23 8b 04 00 a3 8b 04 00 a3 8c a4 04 23 8d b4 04 a3 8d c4 04 a3 8e d4 04 23 8f e4 04 a3 8f f4 04 0f 00 10 03 13 85 0a 00 97 f0 ff ff e7 80 40 e3 63 0e 85 00 13 0c 30 00 23 85 84 01 a3 85 04 00 13 05 00 00 97 f0 ff ff e7 80 c0 e6 e3 94 25 ff 6f f0 9f fe 13 05 01 00 17 f3 ff ff 67 00 03 ec
          gnu  97 02 00 00 93 82 02 00 23 a0 d2 00 97 02 00 00 93 82 02 00 23 a0 22 00 97 02 00 00 93 82 02 00 23 a0 12 00 97 02 00 00 93 82 02 00 23 a0 82 00 23 a2 92 00 23 a4 22 01 23 a6 32 01 23 a8 42 01 23 aa 52 01 23 ac 62 01 23 ae 72 01 23 a0 82 03 23 a2 92 03 23 a4 a2 03 23 a6 b2 03 97 02 00 00 93 82 02 00 23 a0 b2 00 13 04 10 18 93 04 20 19 13 09 30 1a 93 09 40 1b 13 0a 50 1c 93 0a 60 1d 13 0b 70 1e 93 0b 80 1f 13 0c 90 20 93 0c a0 21 13 0d b0 22 93 0d c0 23 13 0e 05 00 13 81 05 00 93 02 10 00 23 15 56 00 0f 00 10 03 e7 00 0e 00 13 0e 05 00 93 0e 01 00 97 02 00 00 93 82 02 00 03 a3 02 00 93 02 30 00 23 2e 53 0c 0f 00 10 03 13 0f 00 00 93 02 10 18 63 1e 54 04 93 02 20 19 63 9a 54 04 93 02 30 1a 63 16 59 04 93 02 40 1b 63 92 59 04 93 02 50 1c 63 1e 5a 02 93 02 60 1d 63 9a 5a 02 93 02 70 1e 63 16 5b 02 93 02 80 1f 63 92 5b 02 93 02 90 20 63 1e 5c 00 93 02 a0 21 63 9a 5c 00 93 02 b0 22 63 16 5d 00 93 02 c0 23 63 84 5d 00 13 0f 10 00 97 02 00 00 93 82 02 00 03 a1 02 00 97 02 00 00 93 82 02 00 03 a4 02 00 83 a4 42 00 03 a9 82 00 83 a9 c2 00 03 aa 02 01 83 aa 42 01 03 ab 82 01 83 ab c2 01 03 ac 02 02 83 ac 42 02 03 ad 82 02 83 ad c2 02 97 02 00 00 93 82 02 00 83 a0 02 00 97 02 00 00 93 82 02 00 83 a2 02 00 23 a0 c2 01 23 a2 d2 01 23 a4 e2 01 67 80 00 00 03 46 15 00 83 46 55 00 03 43 05 00 03 47 25 00 83 48 45 00 83 47 65 00 83 45 75 00 03 48 35 00 13 16 86 00 93 96 86 00 33 66 66 00 b3 e6 16 01 13 17 07 01 93 97 07 01 33 67 c7 00 13 15 88 01 b3 e7 d7 00 93 95 85 01 33 65 e5 00 b3 e5 f5 00 67 80 00 00 93 05 00 00 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 e0 05 73 00 00 00 73 00 10 00 73 00 10 00 6f f0 9f ff b7 07 00 00 83 a7 07 00 13 01 01 ff 23 26 11 00 13 07 10 00 23 8d b7 0c a3 8d 07 0c a3 8e 07 0c 23 8f 07 0c a3 8f 07 0c 23 8e e7 0c 0f 00 10 03 23 8c a7 0c a3 8c 07 0c 97 00 00 00 e7 80 00 00 83 26 05 00 13 01 01 d1 23 26 11 2e 23 24 81 2e 93 07 20 00 63 f4 d7 16 93 97 26 00 93 87 87 00 b3 07 f5 00 03 a7 07 00 13 03 05 00 63 08 07 00 03 a7 47 00 93 87 47 00 e3 1c 07 fe 03 a8 47 00 13 0e 00 00 93 87 47 00 63 04 08 02 13 06 00 00 13 07 60 00 63 16 e8 00 03 a6 47 00 13 0e 10 00 03 a8 87 00 93 87 87 00 e3 16 08 fe 13 08 06 00 93 07 30 00 63 84 f6 06 03 26 03 01 b7 07 00 00 93 87 07 00 b3 05 f6 40 37 05 00 00 33 87 b7 00 03 47 07 00 83 c6 07 00 93 87 17 00 63 98 e6 00 13 07 85 00 e3 94 e7 fe 37 28 00 00 b7 07 00 00 93 87 07 00 33 06 f6 40 b7 05 00 00 33 87 c7 00 03 47 07 00 83 c6 07 00 93 87 17 00 63 98 e6 00 13 87 85 00 e3 94 e7 fe 13 0e 00 00 83 25 c3 00 13 05 c0 f9 13 06 20 0c 93 06 00 18 13 07 00 00 93 07 00 00 93 08 80 03 73 00 00 00 b7 fe ff ff 13 04 05 00 63 ec ae 06 b7 15 00 00 13 06 00 00 93 06 00 00 93 08 e0 02 73 00 00 00 63 e0 ae 06 b7 06 10 00 23 22 91 2e 93 86 26 02 37 05 00 30 b7 05 10 00 13 07 f0 ff 93 08 e0 0d 73 00 00 00 37 07 00 30 63 08 e5 06 13 05 20 04 23 20 21 2f 23 2e 31 2d 23 2c 41 2d 23 2a 51 2d 23 28 61 2d 23 26 71 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 00 00 13 05 10 04 23 22 91 2e 23 20 21 2f 23 2e 31 2d 23 2c 41 2d 23 2a 51 2d 23 28 61 2d 23 26 71 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 00 00 13 05 00 00 b7 15 00 00 13 06 30 00 93 06 10 00 13 07 04 00 73 00 00 00 93 04 05 00 e3 ec ae f6 23 28 61 2d 37 06 00 00 37 0b 00 00 23 20 21 2f 23 20 ab 00 13 06 06 00 93 07 00 00 93 05 80 00 33 07 f6 00 83 46 07 00 33 87 f4 00 93 87 17 00 23 00 d7 00 e3 96 b7 fe 13 09 20 00 a3 84 04 00 23 84 24 01 23 2e 31 2d 23 2c 41 2d 63 0e 0e 0a 23 26 71 2d b7 17 00 00 63 04 f8 02 93 05 10 00 13 05 50 04 23 2a 51 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 00 00 83 25 83 00 13 05 c0 f9 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 80 03 73 00 00 00 37 fa ff ff 93 09 05 00 63 64 aa 08 b7 05 00 00 13 06 08 00 93 85 05 00 93 06 00 20 13 07 01 03 93 08 30 12 73 00 00 00 63 66 aa 08 13 05 81 05 97 00 00 00 e7 80 00 00 23 24 b1 00 23 2a 51 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 93 0b 05 00 63 82 05 08 93 05 20 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 09 00 13 05 50 04 23 2a 51 2d 23 26 71 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 00 00 93 05 30 00 13 05 10 04 23 2a 51 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 00 00 93 05 40 00 13 05 10 04 23 2a 51 2d 23 24 81 2d 23 22 91 2d 23 20 a1 2d 23 2e b1 2b 97 00 00 00 e7 80 00 00 93 07 70 07 63 f4 a7 08 b7 07 10 00 e3 ea a7 f6 93 06 09 00 13 87 09 00 13 05 00 00 93 85 0b 00 13 06 10 00 93 07 00 00 93 08 e0 0d 73 00 00 00 13 09 05 00 63 64 aa 06 37 06 00 00 13 06 06 00 93 07 00 00 93 05 80 00 b3 06 f9 00 33 07 f6 00 83 c6 06 00 03 47 07 00 63 9a e6 04 93 87 17 00 e3 94 b7 fe 03 4c 99 00 83 46 89 00 13 07 20 00 13 1c 8c 00 33 6c dc 00 63 02 ec 04 93 05 40 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 10 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 50 00 13 05 10 04 97 00 00 00 e7 80 00 00 93 05 30 00 13 05 00 04 97 00 00 00 e7 80 00 00 03 47 b9 00 83 46 a9 00 93 05 50 00 13 17 87 00 33 67 d7 00 e3 10 b7 fe 03 46 d9 00 83 45 c9 00 83 46 e9 00 03 47 f9 00 13 16 86 00 33 66 b6 00 93 96 06 01 b3 e6 c6 00 13 17 87 01 33 67 d7 00 93 05 60 00 e3 18 77 fb 03 4a 59 01 03 47 49 01 93 05 70 00 13 1a 8a 00 33 6a ea 00 13 07 fa ff e3 ea e5 f8 03 47 79 01 03 46 69 01 93 06 10 00 13 17 87 00 33 67 c7 00 63 0a d7 00 93 85 07 00 13 05 00 04 97 00 00 00 e7 80 00 00 83 46 99 02 03 46 89 02 03 47 a9 02 83 47 b9 02 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 93 97 87 01 b3 e7 e7 00 13 07 00 10 63 8a e7 00 93 05 90 00 13 05 00 04 97 00 00 00 e7 80 00 00 83 46 19 03 03 46 09 03 03 47 29 03 83 47 39 03 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 93 97 87 01 b3 e7 e7 00 37 f7 00 00 93 87 c7 f9 13 07 c7 9f 63 6c f7 22 03 47 59 03 83 46 49 03 83 47 69 03 83 4d 79 03 13 17 87 00 33 67 d7 00 93 97 07 01 b3 e7 e7 00 93 9d 8d 01 b3 ed fd 00 63 9e 0d 20 93 07 09 04 93 0c 89 07 93 06 00 00 03 c7 07 00 93 87 17 00 b3 e6 e6 00 e3 9a 97 ff 63 92 06 4e 93 09 80 02 b3 09 3a 03 13 06 00 00 93 89 89 07 63 e0 3b 4d 93 0a 01 13 13 8d 0a 00 13 0e 00 00 13 85 0c 00 23 2e c1 00 23 2c d1 00 23 2a c1 01 97 00 00 00 e7 80 00 00 13 07 05 00 23 20 ed 00 13 85 8c 00 23 22 bd 00 97 00 00 00 e7 80 00 00 93 08 05 00 23 24 1d 01 13 85 0c 01 23 26 bd 00 23 26 11 01 23 28 b1 00 97 00 00 00 e7 80 00 00 13 07 05 00 23 28 ed 00 13 85 8c 01 23 2a bd 00 97 00 00 00 e7 80 00 00 83 c7 1c 02 03 cf 0c 02 83 28 c1 00 93 97 87 00 83 ce 2c 02 13 07 05 00 33 e5 e7 01 83 c7 3c 02 33 8f e8 00 03 23 01 01 93 9e 0e 01 b3 ee ae 00 13 95 87 01 b3 37 1f 01 83 c8 4c 02 33 03 b3 00 33 65 d5 01 23 2e bd 00 23 2c ed 00 b3 87 67 00 23 20 ad 02 23 02 1d 03 63 9e 07 42 63 ec eb 43 63 9a 05 42 03 2e 41 01 83 26 81 01 03 26 c1 01 63 62 ef 42 13 0e 1e 00 93 8c 8c 02 13 0d 0d 03 e3 6c 4e f1 93 87 0a 00 13 05 00 00 03 a7 87 01 03 a8 c7 01 93 75 77 00 63 94 05 0e b3 65 07 01 63 84 05 00 63 00 06 41 03 a7 87 00 03 a8 c7 00 b3 35 b0 00 33 67 07 01 33 37 e0 00 63 9a e5 0c 13 05 15 00 93 87 07 03 e3 62 45 fd 93 85 0a 00 13 08 00 00 6f 00 c0 00 93 85 05 03 63 72 48 0d 83 a7 85 00 03 a7 c5 00 13 08 18 00 33 e5 e7 00 e3 04 05 fe 03 ae 85 01 83 a8 c5 01 13 85 05 00 33 83 c7 01 b3 37 f3 00 33 07 17 01 33 87 e7 00 93 0e 08 00 e3 f2 4e fd 83 27 85 03 83 23 c5 03 33 ef 77 00 63 08 0f 02 83 2f 85 04 03 2f c5 04 b3 82 f7 01 b3 b7 f2 00 b3 83 e3 01 b3 87 77 00 63 e6 f8 00 63 98 17 01 63 76 5e 00 63 64 ef 10 63 00 e7 11 93 8e 1e 00 13 05 05 03 6f f0 9f fb 93 05 a0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 b0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 f0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 10 01 13 05 00 04 97 00 00 00 e7 80 00 00 13 87 0a 00 93 08 00 00 6f 00 00 01 93 88 18 00 13 07 07 03 63 fa 48 05 83 25 87 00 03 25 c7 00 b3 e7 a5 00 e3 84 07 fe 03 23 87 01 93 87 79 00 33 b8 37 01 93 f7 87 ff 33 06 c8 00 63 16 f3 28 03 28 c7 01 63 12 c8 28 b3 09 39 01 33 03 f9 00 13 08 00 00 63 82 69 28 03 ce 09 00 93 89 19 00 33 68 c8 01 6f f0 1f ff 93 87 79 00 33 b7 37 01 93 f7 87 ff 33 07 c7 00 63 96 fb 00 83 27 81 00 63 8a e7 00 93 05 50 01 13 05 00 04 97 00 00 00 e7 80 00 00 b3 09 39 01 b3 0b 79 01 63 84 79 03 83 c7 09 00 93 89 19 00 b3 e6 f6 00 6f f0 1f ff e3 f2 6f f0 93 05 20 01 13 05 00 04 97 00 00 00 e7 80 00 00 63 98 06 2c 93 87 0a 00 13 07 00 00 37 06 04 00 b7 06 10 00 83 a5 c7 00 63 94 05 2a 83 a5 87 00 63 60 b6 2a 83 a5 47 01 63 94 05 28 83 a5 07 01 63 e0 b6 28 13 07 17 00 93 87 07 03 e3 6c 47 fd 37 13 00 00 13 03 f3 ff 93 87 0a 00 93 0e 70 00 13 0e 60 00 83 a6 87 00 03 a7 07 01 83 a5 c7 00 03 a6 47 01 33 e5 e6 00 33 e8 c5 00 33 65 05 01 63 08 05 22 83 a9 07 02 63 8c 09 20 13 85 f9 ff b3 f9 a9 00 63 96 09 20 03 a8 07 00 83 a8 47 00 33 75 05 01 63 16 05 1e 03 c5 47 02 63 e2 ae 24 13 75 65 00 63 06 c5 1d 33 8f 06 01 b3 85 15 01 b3 36 df 00 33 05 e7 01 b3 86 b6 00 b3 05 d6 00 33 37 e5 00 33 06 65 00 33 07 b7 00 b3 35 a6 00 b3 85 e5 00 63 e2 e5 18 63 14 b7 00 63 6e a6 16 b7 ff ff ff 33 76 f6 01 63 e8 16 17 63 8e d8 16 63 64 d7 16 63 80 e6 16 37 f7 ff ff 33 77 e8 00 63 88 08 1e 63 9a 05 1e b7 06 10 30 63 e6 c6 1e 23 a4 e7 02 23 a6 c7 02 93 8d 1d 00 93 87 07 03 e3 ea 4d f3 13 05 09 02 97 00 00 00 e7 80 00 00 83 46 d9 02 03 48 c9 02 03 47 e9 02 93 0c 05 00 37 f6 ff ff 03 45 f9 02 33 f6 cc 00 93 96 86 00 b7 17 00 00 b3 e6 06 01 13 17 07 01 b3 07 f6 00 33 67 d7 00 13 15 85 01 b3 b6 c7 00 b3 6b e5 00 b3 86 b6 00 37 07 04 30 13 8d 05 00 33 87 eb 00 63 e4 b6 18 63 94 d5 00 63 e0 c7 18 93 86 0a 00 93 05 00 00 b7 08 04 30 37 f3 03 30 03 a8 86 02 03 a5 c6 02 63 74 f8 00 63 64 a6 16 63 74 e8 00 63 e8 a8 16 63 74 18 01 63 6c a3 16 93 85 15 00 93 86 06 03 e3 ec 45 fd 63 7c e6 16 37 07 04 30 63 78 f7 16 93 05 20 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 d0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 c0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 30 01 13 05 00 04 97 00 00 00 e7 80 00 00 63 1e 08 0a b3 87 f5 00 b3 b5 b7 00 33 05 c5 00 93 89 07 00 33 86 a5 00 6f f0 df d1 93 05 e0 00 13 05 00 04 97 00 00 00 e7 80 00 00 e3 72 37 c1 93 05 00 01 13 05 00 04 97 00 00 00 e7 80 00 00 e3 72 e5 eb 93 05 d0 01 13 05 00 04 97 00 00 00 e7 80 00 00 e3 68 0f ff 6f f0 5f e8 93 05 c0 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 a0 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 90 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 80 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 70 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 60 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 40 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 b0 01 13 05 00 04 97 00 00 00 e7 80 00 00 b7 06 00 30 e3 78 d7 e0 93 05 e0 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 07 f0 ff 6f f0 1f e8 93 05 f0 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 00 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 10 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 87 0a 00 93 89 19 00 93 86 09 00 13 87 07 00 63 f4 46 03 03 a8 87 02 03 25 c7 05 83 a5 c7 02 03 26 87 05 63 74 a8 00 63 60 b6 08 93 86 16 00 13 07 07 03 6f f0 df fd 93 87 07 03 e3 e4 49 fd 83 46 d9 01 03 46 c9 01 03 47 e9 01 83 47 f9 01 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 93 97 87 01 b3 e7 e7 00 63 98 07 02 03 47 59 02 83 46 49 02 83 47 69 02 83 4d 79 02 13 17 87 00 33 67 d7 00 93 97 07 01 b3 e7 e7 00 93 9d 8d 01 b3 ed fd 00 63 82 0d 02 93 05 a0 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 30 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 09 89 01 13 85 09 00 97 00 00 00 e7 80 00 00 13 07 05 00 93 86 0a 00 13 08 00 00 83 c7 46 02 93 f7 47 00 63 82 07 04 03 a6 46 00 83 a7 06 00 63 ec c5 02 63 14 b6 00 63 68 f7 02 03 a5 86 00 03 a3 c6 00 93 08 10 00 33 85 a7 00 b3 37 f5 00 33 06 66 00 b3 87 c7 00 63 e6 f5 00 63 80 b7 08 93 08 00 00 33 68 18 01 93 8d 1d 00 93 86 06 03 e3 e6 4d fb 63 0c 08 04 13 77 37 00 63 10 07 04 b7 15 00 00 13 88 f5 ff b3 f7 0c 01 63 90 07 02 b7 07 03 30 63 94 fc 00 63 08 0d 04 93 05 70 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 60 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 50 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 40 02 13 05 00 04 97 00 00 00 e7 80 00 00 e3 72 a7 f8 33 68 18 01 6f f0 5f f8 93 87 fb ff 37 07 04 00 63 f6 e7 00 33 f8 0b 01 63 0a 08 00 93 05 80 02 13 05 00 04 97 00 00 00 e7 80 00 00 13 85 0c 00 13 07 04 00 13 06 30 00 93 06 10 01 93 07 00 00 93 08 e0 0d 73 00 00 00 37 f3 ff ff 63 68 a3 2a 83 46 d9 02 03 45 c9 02 03 47 e9 02 83 45 f9 02 93 96 86 00 b3 e6 a6 00 13 17 07 01 33 67 d7 00 93 95 85 01 b3 e5 e5 00 37 05 04 30 93 06 20 03 13 07 f0 ff 73 00 00 00 63 68 a3 2e 37 f5 03 30 b7 15 00 00 13 06 00 00 93 06 20 03 13 07 f0 ff 93 07 00 00 93 08 e0 0d 73 00 00 00 b7 f7 ff ff 63 ec a7 28 13 8e 07 00 13 83 0a 00 93 0e 00 00 83 25 c3 02 03 25 83 02 13 06 30 00 93 06 20 03 b3 85 a5 40 13 07 f0 ff 93 07 00 00 93 08 e0 0d 73 00 00 00 63 64 ae 22 93 8e 1e 00 13 03 03 03 e3 e8 4e fd 93 87 0a 00 93 06 00 00 83 a5 87 00 03 a7 c7 00 33 e7 e5 00 63 1a 07 20 93 86 16 00 93 87 07 03 e3 e4 46 ff 13 83 0a 00 13 0e 00 00 b7 fe ff ff 83 47 43 02 13 f7 27 00 13 f6 17 00 63 04 07 00 13 66 26 00 93 f7 47 00 63 84 07 00 13 66 46 00 83 25 c3 02 03 25 83 02 93 06 00 00 13 07 00 00 b3 85 a5 40 93 07 00 00 93 08 20 0e 73 00 00 00 63 ea ae 1e 13 0e 1e 00 13 03 03 03 e3 6a 4e fb 37 f3 ff ff 83 c7 4a 02 93 f7 47 00 63 84 07 02 03 a5 8a 02 83 a5 ca 02 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 30 10 73 00 00 00 63 62 a3 1c 13 08 18 00 93 8a 0a 03 e3 64 48 fd 83 46 19 01 03 46 09 01 03 47 29 01 83 47 39 01 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 93 97 87 01 b3 e7 e7 00 13 d6 87 00 93 d6 07 01 13 d7 87 01 13 0a 10 00 93 0a 89 03 23 86 f4 00 a3 86 c4 00 23 87 d4 00 a3 87 e4 00 a3 88 04 00 23 88 44 01 13 85 0a 00 97 00 00 00 e7 80 00 00 13 87 05 00 93 07 05 00 93 58 85 00 13 58 05 01 93 d5 85 00 13 55 85 01 13 56 07 01 93 56 87 01 23 8e e4 00 23 89 04 00 a3 89 04 00 23 8c f4 00 a3 8c 14 01 23 8d 04 01 a3 8d a4 00 a3 8e b4 00 23 8f c4 00 a3 8f d4 00 0f 00 10 03 93 0b 20 00 a3 8e 04 0c 23 8f 04 0c a3 8f 04 0c 13 85 09 00 23 8e 74 0d 97 00 00 00 e7 80 00 00 83 46 d9 02 03 46 c9 02 03 47 e9 02 83 47 f9 02 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 03 26 0b 00 93 97 87 01 b3 e7 e7 00 b7 05 04 30 b3 85 b7 00 93 06 41 02 97 00 00 00 e7 80 00 00 b7 07 00 00 03 24 41 02 03 27 81 02 83 a7 07 00 13 59 f4 41 63 0a f7 0a 83 27 0b 00 23 8d 47 0d a3 8d 07 0c 0f 00 10 03 13 05 40 04 a3 8c 07 0c 23 8c a7 0c 97 00 00 00 e7 80 00 00 93 05 06 00 13 05 20 04 97 00 00 00 e7 80 00 00 93 05 60 00 13 05 20 04 97 00 00 00 e7 80 00 00 83 a8 07 00 03 a5 87 01 13 07 00 00 33 03 17 01 e3 80 e5 de 33 06 a7 00 33 06 c9 00 03 46 06 00 13 07 17 00 23 00 c3 00 6f f0 5f fe 93 05 50 00 13 05 20 04 97 00 00 00 e7 80 00 00 93 05 70 00 13 05 20 04 97 00 00 00 e7 80 00 00 93 05 30 00 13 05 30 04 97 00 00 00 e7 80 00 00 93 05 40 00 13 05 20 04 97 00 00 00 e7 80 00 00 83 27 c1 02 63 88 07 00 83 27 0b 00 23 8d 77 0d 6f f0 9f f4 13 55 84 00 93 55 04 01 13 56 84 01 93 56 89 00 13 57 09 01 93 57 89 01 23 8c 84 04 23 8e 24 05 23 89 44 01 a3 89 04 00 23 8a 04 00 a3 8a 04 00 23 8b 04 00 a3 8b 04 00 a3 8c a4 04 23 8d b4 04 a3 8d c4 04 a3 8e d4 04 23 8f e4 04 a3 8f f4 04 0f 00 10 03 13 85 0a 00 97 00 00 00 e7 80 00 00 63 0e 85 00 13 0c 30 00 23 85 84 01 a3 85 04 00 13 05 00 00 97 00 00 00 e7 80 00 00 e3 94 25 ff 6f f0 9f fe 13 05 01 00 17 03 00 00 67 00 03 00
    riscv64  fixture-asm_test_entry   agree
    riscv64  helper-helper            DIFFER
          ours b7 02 00 03 93 82 02 00 23 b0 d2 00 b7 02 00 03 93 82 82 01 23 b0 22 00 b7 02 00 03 93 82 02 01 23 b0 12 00 b7 02 00 02 93 82 02 00 23 b0 82 00 23 b4 92 00 23 b8 22 01 23 bc 32 01 23 b0 42 03 23 b4 52 03 23 b8 62 03 23 bc 72 03 23 b0 82 05 23 b4 92 05 23 b8 a2 05 23 bc b2 05 b7 02 00 03 93 82 82 00 23 b0 b2 00 13 04 10 18 93 04 20 19 13 09 30 1a 93 09 40 1b 13 0a 50 1c 93 0a 60 1d 13 0b 70 1e 93 0b 80 1f 13 0c 90 20 93 0c a0 21 13 0d b0 22 93 0d c0 23 13 0e 05 00 13 81 05 00 93 02 10 00 23 15 56 00 0f 00 10 03 e7 00 0e 00 13 0e 05 00 93 0e 01 00 b7 02 00 03 93 82 02 02 03 b3 02 00 93 02 30 00 23 2e 53 0c 0f 00 10 03 13 0f 00 00 93 02 10 18 63 1e 54 04 93 02 20 19 63 9a 54 04 93 02 30 1a 63 16 59 04 93 02 40 1b 63 92 59 04 93 02 50 1c 63 1e 5a 02 93 02 60 1d 63 9a 5a 02 93 02 70 1e 63 16 5b 02 93 02 80 1f 63 92 5b 02 93 02 90 20 63 1e 5c 00 93 02 a0 21 63 9a 5c 00 93 02 b0 22 63 16 5d 00 93 02 c0 23 63 84 5d 00 13 0f 10 00 b7 02 00 03 93 82 82 01 03 b1 02 00 b7 02 00 02 93 82 02 00 03 b4 02 00 83 b4 82 00 03 b9 02 01 83 b9 82 01 03 ba 02 02 83 ba 82 02 03 bb 02 03 83 bb 82 03 03 bc 02 04 83 bc 82 04 03 bd 02 05 83 bd 82 05 b7 02 00 03 93 82 02 01 83 b0 02 00 b7 02 00 03 93 82 02 00 83 b2 02 00 23 b0 c2 01 23 b4 d2 01 23 b8 e2 01 67 80 00 00 83 47 15 00 03 47 05 00 83 45 25 00 03 46 35 00 83 46 45 00 93 97 87 00 b3 e7 e7 00 93 95 05 01 03 47 55 00 b3 e5 f5 00 13 16 86 01 83 47 65 00 33 66 b6 00 03 45 75 00 93 96 06 02 b3 e6 c6 00 13 17 87 02 33 67 d7 00 93 97 07 03 b3 e7 e7 00 13 15 85 03 33 65 f5 00 67 80 00 00 13 15 05 02 13 55 05 02 93 05 00 00 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 e0 05 73 00 00 00 73 00 10 00 73 00 10 00 6f f0 9f ff b7 07 00 03 83 b7 07 02 13 01 01 ff 23 34 11 00 13 07 10 00 23 8d b7 0c a3 8d 07 0c a3 8e 07 0c 23 8f 07 0c a3 8f 07 0c 23 8e e7 0c 0f 00 10 03 23 8c a7 0c a3 8c 07 0c 97 00 00 00 e7 80 80 f9 83 36 05 00 13 01 01 c9 23 34 11 36 23 30 81 36 93 07 20 00 63 f4 d7 16 93 97 36 00 93 87 07 01 b3 07 f5 00 03 b7 07 00 13 03 05 00 63 08 07 00 03 b7 87 00 93 87 87 00 e3 1c 07 fe 03 b8 87 00 13 0e 00 00 93 87 87 00 63 04 08 02 13 06 00 00 13 07 60 00 63 16 e8 00 03 b6 87 00 13 0e 10 00 03 b8 07 01 93 87 07 01 e3 16 08 fe 13 08 06 00 93 07 30 00 63 84 f6 06 03 36 03 02 b7 07 00 01 93 87 07 00 b3 05 f6 40 37 05 00 01 33 87 b7 00 03 47 07 00 83 c6 07 00 93 87 17 00 63 98 e6 00 13 07 85 00 e3 94 e7 fe 37 28 00 00 b7 07 00 01 93 87 07 01 33 06 f6 40 b7 05 00 01 33 87 c7 00 03 47 07 00 83 c6 07 00 93 87 17 00 63 98 e6 00 13 87 85 01 e3 94 e7 fe 13 0e 00 00 83 35 83 01 13 05 c0 f9 13 06 20 0c 93 06 00 18 13 07 00 00 93 07 00 00 93 08 80 03 73 00 00 00 b7 fe ff ff 13 04 05 00 63 ec ae 06 b7 15 00 00 13 06 00 00 93 06 00 00 93 08 e0 02 73 00 00 00 63 e0 ae 06 b7 06 10 00 23 3c 91 34 93 86 26 02 37 05 00 40 b7 05 10 00 13 07 f0 ff 93 08 e0 0d 73 00 00 00 37 07 00 40 63 08 e5 06 13 05 20 04 23 38 21 35 23 34 31 35 23 30 41 35 23 3c 51 33 23 38 61 33 23 34 71 33 23 30 81 33 23 3c 91 31 23 38 a1 31 23 34 b1 31 97 00 00 00 e7 80 c0 e1 13 05 10 04 23 3c 91 34 23 38 21 35 23 34 31 35 23 30 41 35 23 3c 51 33 23 38 61 33 23 34 71 33 23 30 81 33 23 3c 91 31 23 38 a1 31 23 34 b1 31 97 00 00 00 e7 80 40 de 13 05 00 00 b7 15 00 00 13 06 30 00 93 06 10 00 13 07 04 00 73 00 00 00 93 04 05 00 e3 ec ae f6 23 38 61 33 37 06 00 04 37 0b 00 03 23 38 21 35 23 30 ab 02 13 06 06 01 93 07 00 00 93 05 80 00 33 07 f6 00 83 46 07 00 33 87 f4 00 93 87 17 00 23 00 d7 00 e3 96 b7 fe 13 09 20 00 a3 84 04 00 23 84 24 01 23 34 31 35 23 30 41 35 23 3c 51 33 23 34 71 33 23 30 81 33 23 3c 91 31 23 38 a1 31 23 34 b1 31 63 04 0e 10 b7 17 00 00 63 0a f8 00 93 05 10 00 13 05 50 04 97 00 00 00 e7 80 40 d7 83 35 03 01 13 05 c0 f9 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 80 03 73 00 00 00 37 fa ff ff 93 09 05 00 63 6a aa 0c b7 05 00 04 13 06 08 00 93 85 85 00 93 06 00 20 13 07 01 04 93 08 30 12 73 00 00 00 63 62 aa 0c 13 05 81 06 97 00 00 00 e7 80 00 c9 93 07 70 07 93 0c 05 00 63 fe a7 0a b7 07 10 00 63 e2 a7 0c 93 06 09 00 13 87 09 00 13 05 00 00 93 85 0c 00 13 06 10 00 93 07 00 00 93 08 e0 0d 73 00 00 00 13 09 05 00 63 66 aa 0a 37 06 00 04 13 06 06 00 93 06 00 00 93 05 80 00 33 07 d9 00 b3 07 d6 00 03 47 07 00 83 c7 07 00 63 1c f7 08 93 86 16 00 e3 94 b6 fe 03 4c 99 00 83 47 89 00 13 07 20 00 13 1c 8c 00 33 66 fc 00 13 0c 06 00 63 02 e6 08 93 05 40 00 13 05 00 04 97 00 00 00 e7 80 80 c8 93 05 09 00 13 05 50 04 97 00 00 00 e7 80 80 c7 93 05 30 00 13 05 10 04 97 00 00 00 e7 80 80 c6 93 05 40 00 13 05 10 04 97 00 00 00 e7 80 80 c5 93 05 10 00 13 05 00 04 97 00 00 00 e7 80 80 c4 93 05 09 00 13 05 00 04 97 00 00 00 e7 80 80 c3 93 05 50 00 13 05 10 04 97 00 00 00 e7 80 80 c2 93 05 30 00 13 05 00 04 97 00 00 00 e7 80 80 c1 83 45 b9 00 03 47 a9 00 93 07 60 00 93 95 85 00 b3 e5 e5 00 63 8a f5 00 93 05 50 00 13 05 00 04 97 00 00 00 e7 80 00 bf 03 46 d9 00 03 45 c9 00 03 47 e9 00 83 47 f9 00 13 16 86 00 33 66 a6 00 13 17 07 01 33 67 c7 00 93 97 87 01 b3 e7 e7 00 e3 92 97 fb 03 4a 59 01 03 47 49 01 93 05 70 00 13 1a 8a 00 b3 67 ea 00 1b 86 f7 ff 13 8a 07 00 e3 e2 c5 f8 03 47 79 01 83 45 69 01 13 06 10 00 13 17 87 00 33 67 b7 00 63 0a c7 00 93 85 06 00 13 05 00 04 97 00 00 00 e7 80 c0 b7 03 46 99 02 83 45 89 02 83 46 a9 02 03 47 b9 02 13 16 86 00 33 66 b6 00 93 96 06 01 b3 e6 c6 00 13 17 87 01 33 67 d7 00 1b 07 07 00 93 06 00 10 63 0a d7 00 93 05 90 00 13 05 00 04 97 00 00 00 e7 80 80 b3 03 46 19 03 83 45 09 03 83 46 29 03 03 47 39 03 13 16 86 00 33 66 b6 00 93 96 06 01 b3 e6 c6 00 13 17 87 01 33 67 d7 00 b7 f6 00 00 1b 07 c7 f9 93 86 c6 9f 63 ee e6 1a 03 46 59 03 83 45 49 03 83 46 69 03 03 47 79 03 13 16 86 00 33 66 b6 00 93 96 06 01 b3 e6 c6 00 13 17 87 01 33 67 d7 00 1b 06 07 00 63 1e 07 18 93 06 09 04 93 0b 89 07 13 07 00 00 83 c5 06 00 93 86 16 00 33 67 b7 00 e3 9a 76 ff 63 1c 07 22 93 06 80 02 bb 87 d7 02 93 89 87 07 63 ec 3c 21 93 0a 01 14 13 8d 0a 00 13 08 00 00 13 85 0b 00 23 3c e1 00 23 38 01 01 23 34 c1 00 97 00 00 00 e7 80 00 9f 93 07 05 00 23 30 fd 00 13 85 8b 00 97 00 00 00 e7 80 c0 9d 93 0d 05 00 23 34 bd 01 13 85 0b 01 97 00 00 00 e7 80 80 9c 93 07 05 00 23 38 fd 00 13 85 8b 01 97 00 00 00 e7 80 40 9b 93 07 05 00 03 c5 1b 02 83 c6 0b 02 83 c5 2b 02 13 15 85 00 33 65 d5 00 83 c6 3b 02 93 95 05 01 b3 e5 a5 00 03 c5 4b 02 93 96 86 01 b3 e6 b6 00 23 3c fd 00 23 20 dd 02 b3 87 fd 00 23 02 ad 02 b3 bd b7 01 63 e8 fc 18 03 36 81 00 03 38 01 01 03 37 81 01 63 90 0d 18 1b 08 18 00 93 8b 8b 02 13 0d 8d 03 e3 64 48 f5 93 86 0a 00 93 05 00 00 83 b7 86 01 13 f5 77 00 63 10 05 0a 63 84 07 00 63 e2 37 17 03 b5 86 00 b3 37 f0 00 33 35 a0 00 63 9c a7 08 9b 85 15 00 93 86 86 03 e3 ea 45 fd 93 87 0a 00 93 06 00 00 6f 00 c0 00 93 87 87 03 63 f4 46 09 83 b5 87 00 9b 86 16 00 e3 88 05 fe 03 b3 87 01 13 85 07 00 93 88 06 00 b3 85 65 00 e3 fe 48 fd 03 38 05 04 63 0a 08 00 03 3e 05 05 33 08 c8 01 63 74 03 01 63 6e be 2c 9b 88 18 00 13 05 85 03 6f f0 df fd 93 05 a0 00 13 05 00 04 97 00 00 00 e7 80 80 93 93 05 b0 00 13 05 00 04 97 00 00 00 e7 80 80 92 93 05 f0 00 13 05 00 04 97 00 00 00 e7 80 80 91 93 05 10 01 13 05 00 04 97 00 00 00 e7 80 80 90 93 86 0a 00 93 05 00 00 6f 00 00 01 9b 85 15 00 93 86 86 03 63 f6 45 05 03 b8 86 00 e3 08 08 fe 03 b5 86 01 93 87 79 00 93 f7 87 ff 63 18 f5 08 b3 09 39 01 13 05 00 00 bb 88 27 01 6f 00 00 01 03 c3 09 00 93 89 19 00 33 65 65 00 1b 83 09 00 e3 18 13 ff 63 1c 05 06 b3 09 f8 00 6f f0 1f fb 93 87 79 00 93 f7 87 ff 63 8a fc 06 93 05 50 01 13 05 00 04 97 00 00 00 e7 80 c0 88 93 05 d0 00 13 05 00 04 97 00 00 00 e7 80 c0 87 93 05 c0 00 13 05 00 04 97 00 00 00 e7 80 c0 86 93 05 e0 00 13 05 00 04 97 00 00 00 e7 80 c0 85 93 05 00 01 13 05 00 04 97 00 00 00 e7 80 c0 84 93 05 30 01 13 05 00 04 97 00 00 00 e7 80 c0 83 93 05 40 01 13 05 00 04 97 00 00 00 e7 80 c0 82 b3 09 39 01 bb 8c 2c 01 6f 00 00 01 83 c7 09 00 93 89 19 00 33 67 f7 00 9b 87 09 00 e3 98 97 ff e3 18 07 fc 93 87 0a 00 b7 05 04 00 b7 06 10 00 03 b5 87 00 63 e6 a5 18 03 b5 07 01 63 ea a6 18 1b 07 17 00 93 87 87 03 e3 64 47 ff 37 18 00 00 13 08 f8 ff 93 87 0a 00 13 03 70 00 93 08 60 00 83 be 87 00 83 b5 07 01 33 e7 be 00 63 08 07 22 03 ad 07 02 63 0c 0d 20 1b 07 fd ff 33 7d ed 00 63 16 0d 20 93 16 07 02 03 b7 07 00 93 d6 06 02 b3 f6 e6 00 63 9c 06 20 83 c6 47 02 63 60 d3 1e 93 f6 66 00 63 86 16 17 33 85 ee 00 33 8e a5 00 b3 06 0e 01 33 35 d5 01 b3 35 be 00 63 e2 c6 15 b7 f9 ff ff b3 f6 36 01 63 1c 05 12 63 9a 05 12 33 77 37 01 b7 05 00 40 63 64 b7 10 b7 05 10 40 63 e0 d5 10 23 b4 e7 02 23 b8 d7 02 1b 06 16 00 93 87 87 03 e3 66 46 f7 13 05 09 02 97 f0 ff ff e7 80 c0 69 83 46 d9 02 03 46 c9 02 03 47 e9 02 83 47 f9 02 93 96 86 00 13 17 07 01 b3 e6 c6 00 b3 66 d7 00 93 97 87 01 33 e8 d7 00 b7 06 04 40 33 77 35 01 b7 17 00 00 bb 86 06 01 93 96 06 02 b3 07 f7 00 93 0c 05 00 93 d6 06 02 9b 0b 08 00 63 f4 e7 00 93 07 f0 ff 13 86 0a 00 93 05 00 00 b7 08 04 40 37 f3 03 40 03 38 86 02 03 35 06 03 63 74 f8 00 63 6e a7 06 63 74 d8 00 63 e2 a8 0a 63 74 18 01 63 66 a3 0a 9b 85 15 00 13 06 86 03 e3 ec 45 fd 63 76 d7 0a 37 07 04 40 63 72 f7 0a 93 05 20 02 13 05 00 04 97 f0 ff ff e7 80 c0 67 93 05 20 01 13 05 00 04 97 f0 ff ff e7 80 c0 66 93 05 60 01 13 05 00 04 97 f0 ff ff e7 80 c0 65 93 05 70 01 13 05 00 04 97 f0 ff ff e7 80 c0 64 93 05 e0 01 13 05 00 04 97 f0 ff ff e7 80 c0 63 93 05 f0 01 13 05 00 04 97 f0 ff ff e7 80 c0 62 93 05 d0 01 13 05 00 04 97 f0 ff ff e7 80 c0 61 93 05 c0 01 13 05 00 04 97 f0 ff ff e7 80 c0 60 93 05 00 02 13 05 00 04 97 f0 ff ff e7 80 c0 5f 93 05 10 02 13 05 00 04 97 f0 ff ff e7 80 c0 5e 93 87 0a 00 13 07 00 00 1b 07 17 00 13 06 07 00 93 86 07 00 63 74 46 07 83 b8 87 02 03 b8 86 06 03 b5 07 03 83 b5 06 06 63 f4 08 01 63 ee a5 0c 1b 06 16 00 93 86 86 03 6f f0 df fd 93 05 b0 01 13 05 00 04 97 f0 ff ff e7 80 00 5a 93 05 90 01 13 05 00 04 97 f0 ff ff e7 80 00 59 93 05 80 01 13 05 00 04 97 f0 ff ff e7 80 00 58 93 05 a0 01 13 05 00 04 97 f0 ff ff e7 80 00 57 93 87 87 03 e3 64 47 f9 93 09 89 01 13 85 09 00 97 f0 ff ff e7 80 c0 4c 93 07 05 00 13 86 0a 00 93 06 00 00 03 47 46 02 13 77 47 00 63 00 07 02 03 37 06 00 63 ec e7 00 83 35 86 00 33 07 b7 00 33 b7 e7 00 b3 e6 e6 00 9b 86 06 00 1b 0d 1d 00 13 06 86 03 e3 68 4d fd 63 82 06 04 93 f7 37 00 63 92 07 34 37 1e 00 00 13 07 fe ff b3 f7 ec 00 63 92 07 32 b7 07 03 40 63 8a fc 02 93 05 70 02 13 05 00 04 97 f0 ff ff e7 80 40 4e 93 05 30 02 13 05 00 04 97 f0 ff ff e7 80 40 4d 93 05 40 02 13 05 00 04 97 f0 ff ff e7 80 40 4c 9b 87 fb ff b7 06 04 00 63 f6 d7 00 33 f8 eb 00 63 0a 08 00 93 05 80 02 13 05 00 04 97 f0 ff ff e7 80 00 4a 13 85 0c 00 13 07 04 00 93 05 0e 00 13 06 30 00 93 06 10 01 93 07 00 00 93 08 e0 0d 73 00 00 00 37 f3 ff ff 63 68 a3 2e 83 46 d9 02 03 45 c9 02 03 47 e9 02 83 45 f9 02 93 96 86 00 b3 e6 a6 00 13 17 07 01 33 67 d7 00 93 95 85 01 b3 e5 e5 00 37 05 04 40 93 06 20 03 13 07 f0 ff 73 00 00 00 63 62 a3 2a 93 05 0e 00 37 f5 03 40 13 06 00 00 73 00 00 00 63 60 a3 28 13 83 0a 00 13 0e 00 00 b7 fe ff ff 83 35 03 03 03 35 83 02 13 06 30 00 93 06 20 03 b3 85 a5 40 13 07 f0 ff 93 07 00 00 93 08 e0 0d 73 00 00 00 63 ee ae 22 1b 0e 1e 00 13 03 83 03 e3 68 4e fd 93 87 0a 00 13 06 00 00 03 b7 87 00 63 10 07 26 1b 06 16 00 93 87 87 03 e3 68 46 ff 13 83 0a 00 13 0e 00 00 b7 fe ff ff 83 47 43 02 13 f7 27 00 13 f6 17 00 63 04 07 00 13 66 26 00 93 f7 47 00 63 84 07 00 13 66 46 00 83 35 03 03 03 35 83 02 93 06 00 00 13 07 00 00 b3 85 a5 40 93 07 00 00 93 08 20 0e 73 00 00 00 63 ec ae 22 1b 0e 1e 00 13 03 83 03 e3 6a 4e fb 37 f3 ff ff 83 c7 4a 02 93 f7 47 00 63 84 07 02 03 b5 8a 02 83 b5 0a 03 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 30 10 73 00 00 00 63 64 a3 20 1b 08 18 00 93 8a 8a 03 e3 64 48 fd 83 46 19 01 03 46 09 01 03 47 29 01 83 47 39 01 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 93 97 87 01 b3 e7 e7 00 9b 87 07 00 1b d6 87 00 9b d6 07 01 1b d7 87 01 13 0a 10 00 93 0a 89 03 23 86 f4 00 a3 86 c4 00 23 87 d4 00 a3 87 e4 00 a3 88 04 00 23 88 44 01 13 85 0a 00 97 f0 ff ff e7 80 00 24 93 07 05 00 9b 58 85 00 1b 58 05 01 93 d5 07 02 1b 55 85 01 13 d6 87 02 93 d6 07 03 13 d7 87 03 23 89 04 00 a3 89 04 00 23 8c f4 00 a3 8c 14 01 23 8d 04 01 a3 8d a4 00 23 8e b4 00 a3 8e c4 00 23 8f d4 00 a3 8f e4 00 0f 00 10 03 93 0b 20 00 a3 8e 04 0c 23 8f 04 0c a3 8f 04 0c 13 85 09 00 23 8e 74 0d 97 f0 ff ff e7 80 40 1d 03 47 d9 02 83 46 c9 02 83 47 e9 02 83 45 f9 02 13 17 87 00 33 67 d7 00 93 97 07 01 b3 e7 e7 00 93 95 85 01 b3 e5 f5 00 b7 07 04 40 bb 85 f5 00 03 36 0b 02 93 95 05 02 93 06 81 02 93 d5 05 02 97 f0 ff ff e7 80 80 fe b7 07 00 03 03 37 81 02 83 36 01 03 83 b7 87 00 1b 04 07 00 63 8e f6 0c 83 37 0b 02 23 8d 47 0d a3 8d 07 0c 0f 00 10 03 13 05 40 04 a3 8c 07 0c 23 8c a7 0c 97 f0 ff ff e7 80 c0 1a 93 05 60 02 13 05 00 04 97 f0 ff ff e7 80 c0 1c 93 05 50 02 13 05 00 04 97 f0 ff ff e7 80 c0 1b 93 05 60 00 13 05 20 04 97 f0 ff ff e7 80 c0 1a 93 05 50 00 13 05 20 04 97 f0 ff ff e7 80 c0 19 93 05 40 00 13 05 20 04 97 f0 ff ff e7 80 c0 18 93 05 06 00 13 05 20 04 97 f0 ff ff e7 80 c0 17 83 b8 07 00 03 b5 87 01 9b 06 07 00 13 07 00 00 9b 05 07 00 33 83 e8 00 e3 86 b6 d8 b3 05 e5 00 b3 05 b9 00 83 c5 05 00 13 07 17 00 23 00 b3 00 6f f0 1f fe 93 05 70 00 13 05 20 04 97 f0 ff ff e7 80 80 13 93 05 30 00 13 05 30 04 97 f0 ff ff e7 80 80 12 83 37 81 03 63 88 07 00 83 37 0b 02 23 8d 77 0d 6f f0 1f f2 93 57 f4 43 9b d8 87 00 1b d8 07 01 1b 55 84 00 9b d7 87 01 9b 55 04 01 1b 56 84 01 93 56 04 02 23 8c e4 04 23 89 44 01 a3 89 04 00 23 8a 04 00 a3 8a 04 00 23 8b 04 00 a3 8b 04 00 a3 8e 14 05 23 8f 04 05 a3 8f f4 04 a3 8c a4 04 23 8d b4 04 a3 8d c4 04 23 8e d4 04 0f 00 10 03 13 85 0a 00 97 f0 ff ff e7 80 00 02 63 04 85 00 13 0c 30 00 23 85 84 01 a3 85 04 00 13 05 00 00 97 f0 ff ff e7 80 00 06 13 05 01 00 17 f3 ff ff 67 00 43 0c
          gnu  97 02 00 00 93 82 02 00 23 b0 d2 00 97 02 00 00 93 82 02 00 23 b0 22 00 97 02 00 00 93 82 02 00 23 b0 12 00 97 02 00 00 93 82 02 00 23 b0 82 00 23 b4 92 00 23 b8 22 01 23 bc 32 01 23 b0 42 03 23 b4 52 03 23 b8 62 03 23 bc 72 03 23 b0 82 05 23 b4 92 05 23 b8 a2 05 23 bc b2 05 97 02 00 00 93 82 02 00 23 b0 b2 00 13 04 10 18 93 04 20 19 13 09 30 1a 93 09 40 1b 13 0a 50 1c 93 0a 60 1d 13 0b 70 1e 93 0b 80 1f 13 0c 90 20 93 0c a0 21 13 0d b0 22 93 0d c0 23 13 0e 05 00 13 81 05 00 93 02 10 00 23 15 56 00 0f 00 10 03 e7 00 0e 00 13 0e 05 00 93 0e 01 00 97 02 00 00 93 82 02 00 03 b3 02 00 93 02 30 00 23 2e 53 0c 0f 00 10 03 13 0f 00 00 93 02 10 18 63 1e 54 04 93 02 20 19 63 9a 54 04 93 02 30 1a 63 16 59 04 93 02 40 1b 63 92 59 04 93 02 50 1c 63 1e 5a 02 93 02 60 1d 63 9a 5a 02 93 02 70 1e 63 16 5b 02 93 02 80 1f 63 92 5b 02 93 02 90 20 63 1e 5c 00 93 02 a0 21 63 9a 5c 00 93 02 b0 22 63 16 5d 00 93 02 c0 23 63 84 5d 00 13 0f 10 00 97 02 00 00 93 82 02 00 03 b1 02 00 97 02 00 00 93 82 02 00 03 b4 02 00 83 b4 82 00 03 b9 02 01 83 b9 82 01 03 ba 02 02 83 ba 82 02 03 bb 02 03 83 bb 82 03 03 bc 02 04 83 bc 82 04 03 bd 02 05 83 bd 82 05 97 02 00 00 93 82 02 00 83 b0 02 00 97 02 00 00 93 82 02 00 83 b2 02 00 23 b0 c2 01 23 b4 d2 01 23 b8 e2 01 67 80 00 00 83 47 15 00 03 47 05 00 83 45 25 00 03 46 35 00 83 46 45 00 93 97 87 00 b3 e7 e7 00 93 95 05 01 03 47 55 00 b3 e5 f5 00 13 16 86 01 83 47 65 00 33 66 b6 00 03 45 75 00 93 96 06 02 b3 e6 c6 00 13 17 87 02 33 67 d7 00 93 97 07 03 b3 e7 e7 00 13 15 85 03 33 65 f5 00 67 80 00 00 13 15 05 02 13 55 05 02 93 05 00 00 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 e0 05 73 00 00 00 73 00 10 00 73 00 10 00 6f f0 9f ff b7 07 00 00 83 b7 07 00 13 01 01 ff 23 34 11 00 13 07 10 00 23 8d b7 0c a3 8d 07 0c a3 8e 07 0c 23 8f 07 0c a3 8f 07 0c 23 8e e7 0c 0f 00 10 03 23 8c a7 0c a3 8c 07 0c 97 00 00 00 e7 80 00 00 83 36 05 00 13 01 01 c9 23 34 11 36 23 30 81 36 93 07 20 00 63 f4 d7 16 93 97 36 00 93 87 07 01 b3 07 f5 00 03 b7 07 00 13 03 05 00 63 08 07 00 03 b7 87 00 93 87 87 00 e3 1c 07 fe 03 b8 87 00 13 0e 00 00 93 87 87 00 63 04 08 02 13 06 00 00 13 07 60 00 63 16 e8 00 03 b6 87 00 13 0e 10 00 03 b8 07 01 93 87 07 01 e3 16 08 fe 13 08 06 00 93 07 30 00 63 84 f6 06 03 36 03 02 b7 07 00 00 93 87 07 00 b3 05 f6 40 37 05 00 00 33 87 b7 00 03 47 07 00 83 c6 07 00 93 87 17 00 63 98 e6 00 13 07 85 00 e3 94 e7 fe 37 28 00 00 b7 07 00 00 93 87 07 00 33 06 f6 40 b7 05 00 00 33 87 c7 00 03 47 07 00 83 c6 07 00 93 87 17 00 63 98 e6 00 13 87 85 00 e3 94 e7 fe 13 0e 00 00 83 35 83 01 13 05 c0 f9 13 06 20 0c 93 06 00 18 13 07 00 00 93 07 00 00 93 08 80 03 73 00 00 00 b7 fe ff ff 13 04 05 00 63 ec ae 06 b7 15 00 00 13 06 00 00 93 06 00 00 93 08 e0 02 73 00 00 00 63 e0 ae 06 b7 06 10 00 23 3c 91 34 93 86 26 02 37 05 00 40 b7 05 10 00 13 07 f0 ff 93 08 e0 0d 73 00 00 00 37 07 00 40 63 08 e5 06 13 05 20 04 23 38 21 35 23 34 31 35 23 30 41 35 23 3c 51 33 23 38 61 33 23 34 71 33 23 30 81 33 23 3c 91 31 23 38 a1 31 23 34 b1 31 97 00 00 00 e7 80 00 00 13 05 10 04 23 3c 91 34 23 38 21 35 23 34 31 35 23 30 41 35 23 3c 51 33 23 38 61 33 23 34 71 33 23 30 81 33 23 3c 91 31 23 38 a1 31 23 34 b1 31 97 00 00 00 e7 80 00 00 13 05 00 00 b7 15 00 00 13 06 30 00 93 06 10 00 13 07 04 00 73 00 00 00 93 04 05 00 e3 ec ae f6 23 38 61 33 37 06 00 00 37 0b 00 00 23 38 21 35 23 30 ab 00 13 06 06 00 93 07 00 00 93 05 80 00 33 07 f6 00 83 46 07 00 33 87 f4 00 93 87 17 00 23 00 d7 00 e3 96 b7 fe 13 09 20 00 a3 84 04 00 23 84 24 01 23 34 31 35 23 30 41 35 23 3c 51 33 23 34 71 33 23 30 81 33 23 3c 91 31 23 38 a1 31 23 34 b1 31 63 04 0e 10 b7 17 00 00 63 0a f8 00 93 05 10 00 13 05 50 04 97 00 00 00 e7 80 00 00 83 35 03 01 13 05 c0 f9 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 80 03 73 00 00 00 37 fa ff ff 93 09 05 00 63 6a aa 0c b7 05 00 00 13 06 08 00 93 85 05 00 93 06 00 20 13 07 01 04 93 08 30 12 73 00 00 00 63 62 aa 0c 13 05 81 06 97 00 00 00 e7 80 00 00 93 07 70 07 93 0c 05 00 63 fe a7 0a b7 07 10 00 63 e2 a7 0c 93 06 09 00 13 87 09 00 13 05 00 00 93 85 0c 00 13 06 10 00 93 07 00 00 93 08 e0 0d 73 00 00 00 13 09 05 00 63 66 aa 0a 37 06 00 00 13 06 06 00 93 06 00 00 93 05 80 00 33 07 d9 00 b3 07 d6 00 03 47 07 00 83 c7 07 00 63 1c f7 08 93 86 16 00 e3 94 b6 fe 03 4c 99 00 83 47 89 00 13 07 20 00 13 1c 8c 00 33 66 fc 00 13 0c 06 00 63 02 e6 08 93 05 40 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 09 00 13 05 50 04 97 00 00 00 e7 80 00 00 93 05 30 00 13 05 10 04 97 00 00 00 e7 80 00 00 93 05 40 00 13 05 10 04 97 00 00 00 e7 80 00 00 93 05 10 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 09 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 50 00 13 05 10 04 97 00 00 00 e7 80 00 00 93 05 30 00 13 05 00 04 97 00 00 00 e7 80 00 00 83 45 b9 00 03 47 a9 00 93 07 60 00 93 95 85 00 b3 e5 e5 00 63 8a f5 00 93 05 50 00 13 05 00 04 97 00 00 00 e7 80 00 00 03 46 d9 00 03 45 c9 00 03 47 e9 00 83 47 f9 00 13 16 86 00 33 66 a6 00 13 17 07 01 33 67 c7 00 93 97 87 01 b3 e7 e7 00 e3 92 97 fb 03 4a 59 01 03 47 49 01 93 05 70 00 13 1a 8a 00 b3 67 ea 00 1b 86 f7 ff 13 8a 07 00 e3 e2 c5 f8 03 47 79 01 83 45 69 01 13 06 10 00 13 17 87 00 33 67 b7 00 63 0a c7 00 93 85 06 00 13 05 00 04 97 00 00 00 e7 80 00 00 03 46 99 02 83 45 89 02 83 46 a9 02 03 47 b9 02 13 16 86 00 33 66 b6 00 93 96 06 01 b3 e6 c6 00 13 17 87 01 33 67 d7 00 1b 07 07 00 93 06 00 10 63 0a d7 00 93 05 90 00 13 05 00 04 97 00 00 00 e7 80 00 00 03 46 19 03 83 45 09 03 83 46 29 03 03 47 39 03 13 16 86 00 33 66 b6 00 93 96 06 01 b3 e6 c6 00 13 17 87 01 33 67 d7 00 b7 f6 00 00 1b 07 c7 f9 93 86 c6 9f 63 ee e6 1a 03 46 59 03 83 45 49 03 83 46 69 03 03 47 79 03 13 16 86 00 33 66 b6 00 93 96 06 01 b3 e6 c6 00 13 17 87 01 33 67 d7 00 1b 06 07 00 63 1e 07 18 93 06 09 04 93 0b 89 07 13 07 00 00 83 c5 06 00 93 86 16 00 33 67 b7 00 e3 9a 76 ff 63 1c 07 22 93 06 80 02 bb 87 d7 02 93 89 87 07 63 ec 3c 21 93 0a 01 14 13 8d 0a 00 13 08 00 00 13 85 0b 00 23 3c e1 00 23 38 01 01 23 34 c1 00 97 00 00 00 e7 80 00 00 93 07 05 00 23 30 fd 00 13 85 8b 00 97 00 00 00 e7 80 00 00 93 0d 05 00 23 34 bd 01 13 85 0b 01 97 00 00 00 e7 80 00 00 93 07 05 00 23 38 fd 00 13 85 8b 01 97 00 00 00 e7 80 00 00 93 07 05 00 03 c5 1b 02 83 c6 0b 02 83 c5 2b 02 13 15 85 00 33 65 d5 00 83 c6 3b 02 93 95 05 01 b3 e5 a5 00 03 c5 4b 02 93 96 86 01 b3 e6 b6 00 23 3c fd 00 23 20 dd 02 b3 87 fd 00 23 02 ad 02 b3 bd b7 01 63 e8 fc 18 03 36 81 00 03 38 01 01 03 37 81 01 63 90 0d 18 1b 08 18 00 93 8b 8b 02 13 0d 8d 03 e3 64 48 f5 93 86 0a 00 93 05 00 00 83 b7 86 01 13 f5 77 00 63 10 05 0a 63 84 07 00 63 e2 37 17 03 b5 86 00 b3 37 f0 00 33 35 a0 00 63 9c a7 08 9b 85 15 00 93 86 86 03 e3 ea 45 fd 93 87 0a 00 93 06 00 00 6f 00 c0 00 93 87 87 03 63 f4 46 09 83 b5 87 00 9b 86 16 00 e3 88 05 fe 03 b3 87 01 13 85 07 00 93 88 06 00 b3 85 65 00 e3 fe 48 fd 03 38 05 04 63 0a 08 00 03 3e 05 05 33 08 c8 01 63 74 03 01 63 6e be 2c 9b 88 18 00 13 05 85 03 6f f0 df fd 93 05 a0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 b0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 f0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 10 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 86 0a 00 93 05 00 00 6f 00 00 01 9b 85 15 00 93 86 86 03 63 f6 45 05 03 b8 86 00 e3 08 08 fe 03 b5 86 01 93 87 79 00 93 f7 87 ff 63 18 f5 08 b3 09 39 01 13 05 00 00 bb 88 27 01 6f 00 00 01 03 c3 09 00 93 89 19 00 33 65 65 00 1b 83 09 00 e3 18 13 ff 63 1c 05 06 b3 09 f8 00 6f f0 1f fb 93 87 79 00 93 f7 87 ff 63 8a fc 06 93 05 50 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 d0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 c0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 e0 00 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 00 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 30 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 40 01 13 05 00 04 97 00 00 00 e7 80 00 00 b3 09 39 01 bb 8c 2c 01 6f 00 00 01 83 c7 09 00 93 89 19 00 33 67 f7 00 9b 87 09 00 e3 98 97 ff e3 18 07 fc 93 87 0a 00 b7 05 04 00 b7 06 10 00 03 b5 87 00 63 e6 a5 18 03 b5 07 01 63 ea a6 18 1b 07 17 00 93 87 87 03 e3 64 47 ff 37 18 00 00 13 08 f8 ff 93 87 0a 00 13 03 70 00 93 08 60 00 83 be 87 00 83 b5 07 01 33 e7 be 00 63 08 07 22 03 ad 07 02 63 0c 0d 20 1b 07 fd ff 33 7d ed 00 63 16 0d 20 93 16 07 02 03 b7 07 00 93 d6 06 02 b3 f6 e6 00 63 9c 06 20 83 c6 47 02 63 60 d3 1e 93 f6 66 00 63 86 16 17 33 85 ee 00 33 8e a5 00 b3 06 0e 01 33 35 d5 01 b3 35 be 00 63 e2 c6 15 b7 f9 ff ff b3 f6 36 01 63 1c 05 12 63 9a 05 12 33 77 37 01 b7 05 00 40 63 64 b7 10 b7 05 10 40 63 e0 d5 10 23 b4 e7 02 23 b8 d7 02 1b 06 16 00 93 87 87 03 e3 66 46 f7 13 05 09 02 97 00 00 00 e7 80 00 00 83 46 d9 02 03 46 c9 02 03 47 e9 02 83 47 f9 02 93 96 86 00 13 17 07 01 b3 e6 c6 00 b3 66 d7 00 93 97 87 01 33 e8 d7 00 b7 06 04 40 33 77 35 01 b7 17 00 00 bb 86 06 01 93 96 06 02 b3 07 f7 00 93 0c 05 00 93 d6 06 02 9b 0b 08 00 63 f4 e7 00 93 07 f0 ff 13 86 0a 00 93 05 00 00 b7 08 04 40 37 f3 03 40 03 38 86 02 03 35 06 03 63 74 f8 00 63 6e a7 06 63 74 d8 00 63 e2 a8 0a 63 74 18 01 63 66 a3 0a 9b 85 15 00 13 06 86 03 e3 ec 45 fd 63 76 d7 0a 37 07 04 40 63 72 f7 0a 93 05 20 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 20 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 60 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 70 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 e0 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 f0 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 d0 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 c0 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 00 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 10 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 87 0a 00 13 07 00 00 1b 07 17 00 13 06 07 00 93 86 07 00 63 74 46 07 83 b8 87 02 03 b8 86 06 03 b5 07 03 83 b5 06 06 63 f4 08 01 63 ee a5 0c 1b 06 16 00 93 86 86 03 6f f0 df fd 93 05 b0 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 90 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 80 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 a0 01 13 05 00 04 97 00 00 00 e7 80 00 00 93 87 87 03 e3 64 47 f9 93 09 89 01 13 85 09 00 97 00 00 00 e7 80 00 00 93 07 05 00 13 86 0a 00 93 06 00 00 03 47 46 02 13 77 47 00 63 00 07 02 03 37 06 00 63 ec e7 00 83 35 86 00 33 07 b7 00 33 b7 e7 00 b3 e6 e6 00 9b 86 06 00 1b 0d 1d 00 13 06 86 03 e3 68 4d fd 63 82 06 04 93 f7 37 00 63 92 07 34 37 1e 00 00 13 07 fe ff b3 f7 ec 00 63 92 07 32 b7 07 03 40 63 8a fc 02 93 05 70 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 30 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 40 02 13 05 00 04 97 00 00 00 e7 80 00 00 9b 87 fb ff b7 06 04 00 63 f6 d7 00 33 f8 eb 00 63 0a 08 00 93 05 80 02 13 05 00 04 97 00 00 00 e7 80 00 00 13 85 0c 00 13 07 04 00 93 05 0e 00 13 06 30 00 93 06 10 01 93 07 00 00 93 08 e0 0d 73 00 00 00 37 f3 ff ff 63 68 a3 2e 83 46 d9 02 03 45 c9 02 03 47 e9 02 83 45 f9 02 93 96 86 00 b3 e6 a6 00 13 17 07 01 33 67 d7 00 93 95 85 01 b3 e5 e5 00 37 05 04 40 93 06 20 03 13 07 f0 ff 73 00 00 00 63 62 a3 2a 93 05 0e 00 37 f5 03 40 13 06 00 00 73 00 00 00 63 60 a3 28 13 83 0a 00 13 0e 00 00 b7 fe ff ff 83 35 03 03 03 35 83 02 13 06 30 00 93 06 20 03 b3 85 a5 40 13 07 f0 ff 93 07 00 00 93 08 e0 0d 73 00 00 00 63 ee ae 22 1b 0e 1e 00 13 03 83 03 e3 68 4e fd 93 87 0a 00 13 06 00 00 03 b7 87 00 63 10 07 26 1b 06 16 00 93 87 87 03 e3 68 46 ff 13 83 0a 00 13 0e 00 00 b7 fe ff ff 83 47 43 02 13 f7 27 00 13 f6 17 00 63 04 07 00 13 66 26 00 93 f7 47 00 63 84 07 00 13 66 46 00 83 35 03 03 03 35 83 02 93 06 00 00 13 07 00 00 b3 85 a5 40 93 07 00 00 93 08 20 0e 73 00 00 00 63 ec ae 22 1b 0e 1e 00 13 03 83 03 e3 6a 4e fb 37 f3 ff ff 83 c7 4a 02 93 f7 47 00 63 84 07 02 03 b5 8a 02 83 b5 0a 03 13 06 00 00 93 06 00 00 13 07 00 00 93 07 00 00 93 08 30 10 73 00 00 00 63 64 a3 20 1b 08 18 00 93 8a 8a 03 e3 64 48 fd 83 46 19 01 03 46 09 01 03 47 29 01 83 47 39 01 93 96 86 00 b3 e6 c6 00 13 17 07 01 33 67 d7 00 93 97 87 01 b3 e7 e7 00 9b 87 07 00 1b d6 87 00 9b d6 07 01 1b d7 87 01 13 0a 10 00 93 0a 89 03 23 86 f4 00 a3 86 c4 00 23 87 d4 00 a3 87 e4 00 a3 88 04 00 23 88 44 01 13 85 0a 00 97 00 00 00 e7 80 00 00 93 07 05 00 9b 58 85 00 1b 58 05 01 93 d5 07 02 1b 55 85 01 13 d6 87 02 93 d6 07 03 13 d7 87 03 23 89 04 00 a3 89 04 00 23 8c f4 00 a3 8c 14 01 23 8d 04 01 a3 8d a4 00 23 8e b4 00 a3 8e c4 00 23 8f d4 00 a3 8f e4 00 0f 00 10 03 93 0b 20 00 a3 8e 04 0c 23 8f 04 0c a3 8f 04 0c 13 85 09 00 23 8e 74 0d 97 00 00 00 e7 80 00 00 03 47 d9 02 83 46 c9 02 83 47 e9 02 83 45 f9 02 13 17 87 00 33 67 d7 00 93 97 07 01 b3 e7 e7 00 93 95 85 01 b3 e5 f5 00 b7 07 04 40 bb 85 f5 00 03 36 0b 00 93 95 05 02 93 06 81 02 93 d5 05 02 97 00 00 00 e7 80 00 00 b7 07 00 00 03 37 81 02 83 36 01 03 83 b7 07 00 1b 04 07 00 63 8e f6 0c 83 37 0b 00 23 8d 47 0d a3 8d 07 0c 0f 00 10 03 13 05 40 04 a3 8c 07 0c 23 8c a7 0c 97 00 00 00 e7 80 00 00 93 05 60 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 50 02 13 05 00 04 97 00 00 00 e7 80 00 00 93 05 60 00 13 05 20 04 97 00 00 00 e7 80 00 00 93 05 50 00 13 05 20 04 97 00 00 00 e7 80 00 00 93 05 40 00 13 05 20 04 97 00 00 00 e7 80 00 00 93 05 06 00 13 05 20 04 97 00 00 00 e7 80 00 00 83 b8 07 00 03 b5 87 01 9b 06 07 00 13 07 00 00 9b 05 07 00 33 83 e8 00 e3 86 b6 d8 b3 05 e5 00 b3 05 b9 00 83 c5 05 00 13 07 17 00 23 00 b3 00 6f f0 1f fe 93 05 70 00 13 05 20 04 97 00 00 00 e7 80 00 00 93 05 30 00 13 05 30 04 97 00 00 00 e7 80 00 00 83 37 81 03 63 88 07 00 83 37 0b 00 23 8d 77 0d 6f f0 1f f2 93 57 f4 43 9b d8 87 00 1b d8 07 01 1b 55 84 00 9b d7 87 01 9b 55 04 01 1b 56 84 01 93 56 04 02 23 8c e4 04 23 89 44 01 a3 89 04 00 23 8a 04 00 a3 8a 04 00 23 8b 04 00 a3 8b 04 00 a3 8e 14 05 23 8f 04 05 a3 8f f4 04 a3 8c a4 04 23 8d b4 04 a3 8d c4 04 23 8e d4 04 0f 00 10 03 13 85 0a 00 97 00 00 00 e7 80 00 00 63 04 85 00 13 0c 30 00 23 85 84 01 a3 85 04 00 13 05 00 00 97 00 00 00 e7 80 00 00 13 05 01 00 17 03 00 00 67 00 03 00

    6 agree, 2 differ, 4 beyond M1, 0 not assembled by GNU as |}]
