(* The Phase 4 parsers, held to the bytes that are already committed.

   Two argv arguments: the recorded binutils output (asm/tools/test/data/gnu)
   and the fixture corpus. Every recorded pair is parsed and compared with the
   corresponding committed oracle artifact, so this suite proves byte-equality
   against the shell's awk WITHOUT needing a cross toolchain - the differential
   over all 36 trees, which does run the real tools, is a separate gate.

   The recorded inputs were produced by the same binutils the corpus was
   generated with. If they are ever re-recorded with a different binutils and
   the committed artifacts are not regenerated in the same commit, this suite
   fails - which is the correct outcome, not a nuisance: the two must move
   together. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let check_eq name ~expected ~actual =
  incr checks;
  if String.equal expected actual then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n    expected: %S\n    actual:   %S\n" name expected actual;
    incr failures)

let read p =
  match Tool_fs.read p with
  | Ok s -> s
  | Error e ->
      prerr_endline (Tool_error.to_fatal_line (Err.Error.kind e));
      exit 1

let unwrap = function
  | Ok v -> v
  | Error e ->
      prerr_endline (Tool_error.to_fatal_line (Err.Error.kind e));
      exit 1

(* {1 Against the committed artifacts} *)

let pairs =
  [
    ("x86_32", "global_ldst");
    ("x86_32", "direct_call");
    ("x86_64", "global_ldst");
    ("x86_64", "direct_call");
    ("arm", "global_ldst");
    ("arm", "direct_call");
    ("aarch64", "global_ldst");
    ("aarch64", "direct_call");
    ("riscv64", "global_ldst");
    ("riscv64", "direct_call");
  ]

let test_against_corpus data corpus =
  List.iter
    (fun (target, case) ->
      let recorded kind = read Fpath.(data / Printf.sprintf "%s.%s.%s.txt" kind target case) in
      let committed name =
        read Fpath.(corpus / "compcert-3.17" / case / target / "oracle" / name)
      in

      (* reloc.txt: the REL/RELA decision comes from readelf -SW, never from
         objdump -r, whose banner names the section the records apply to. *)
      let rela = Gnu_output.has_rela (recorded "readelf-SW") in
      let relocs = unwrap (Gnu_output.relocations ~objdump_r:(recorded "objdump-r") ~rela) in
      check_eq
        (Printf.sprintf "reloc.txt: %s/%s" case target)
        ~expected:(committed "reloc.txt") ~actual:relocs;

      (* The kind is itself asserted, not just the rendered rows: a target with
         no relocations would render an empty file under either kind, so
         equality alone would not catch the decision going wrong there. *)
      let expect_rela = match target with "x86_32" | "arm" -> false | _ -> true in
      check
        (Printf.sprintf "kind: %s/%s is %s" case target (if expect_rela then "rela" else "rel"))
        (rela = expect_rela);

      let syms = unwrap (Gnu_output.symbols ~readelf_s:(recorded "readelf-sW")) in
      check_eq
        (Printf.sprintf "symbols.txt: %s/%s" case target)
        ~expected:(committed "symbols.txt") ~actual:syms;

      (* alloc_sections over the object: .text is always allocated, and the
         non-alloc sections the assembler emits must not appear. *)
      let secs = unwrap (Gnu_output.alloc_sections (recorded "objdump-h")) in
      check (Printf.sprintf "alloc: %s/%s has .text" case target) (List.mem ".text" secs);
      check
        (Printf.sprintf "alloc: %s/%s excludes .comment/.note.GNU-stack" case target)
        ((not (List.mem ".comment" secs)) && not (List.mem ".note.GNU-stack" secs));
      check
        (Printf.sprintf "section_size: %s/%s .text present" case target)
        (Gnu_output.section_size (recorded "objdump-h") ~name:".text" <> None);
      check
        (Printf.sprintf "section_size: %s/%s absent section is None" case target)
        (Gnu_output.section_size (recorded "objdump-h") ~name:".nonesuch" = None))
    pairs

(* {2 Adversarial cases the corpus does not contain}

   The corpus exercises one addend shape per target. These are the shapes that
   would silently mis-parse, stated directly, because the whole risk of this
   phase is an awk rewrite that agrees on the sample and diverges elsewhere. *)

let reloc_of ~rela body =
  unwrap
    (Gnu_output.relocations
       ~objdump_r:("RELOCATION RECORDS FOR [.text]:\nOFFSET   TYPE              VALUE\n" ^ body)
       ~rela)

let test_addends () =
  (* A symbol whose own name contains "+0x". The addend is the FINAL such
     group; a leftmost scan would split the name in half. *)
  check_eq "addend: symbol containing +0x keeps its name"
    ~expected:".text\t0x00000004\trela\tR_X\ta+0xb\t+0x10\n"
    ~actual:(reloc_of ~rela:true "00000004 R_X               a+0xb+0x10\n");

  (* No addend at all: RELA renders 0x0, REL renders '-'. *)
  check_eq "addend: absent under rela" ~expected:".text\t0x00000004\trela\tR_X\tsym\t0x0\n"
    ~actual:(reloc_of ~rela:true "00000004 R_X               sym\n");
  check_eq "addend: absent under rel" ~expected:".text\t0x00000004\trel\tR_X\tsym\t-\n"
    ~actual:(reloc_of ~rela:false "00000004 R_X               sym\n");

  (* Negative, and leading zeros stripped from the addend but NOT from a value
     that is all zeros, which becomes "0". *)
  check_eq "addend: negative, zeros stripped" ~expected:".text\t0x00000004\trela\tR_X\tsym\t-0x4\n"
    ~actual:(reloc_of ~rela:true "00000004 R_X               sym-0x0004\n");
  check_eq "addend: all-zero renders 0x0" ~expected:".text\t0x00000004\trela\tR_X\tsym\t+0x0\n"
    ~actual:(reloc_of ~rela:true "00000004 R_X               sym+0x0000\n");

  (* A trailing group that is NOT a valid addend must stay part of the symbol:
     "+0x" with no digits, and "+0xg" with a non-hex digit. *)
  check_eq "addend: +0x with no digits is not an addend"
    ~expected:".text\t0x00000004\trela\tR_X\tsym+0x\t0x0\n"
    ~actual:(reloc_of ~rela:true "00000004 R_X               sym+0x\n");
  check_eq "addend: non-hex digit is not an addend"
    ~expected:".text\t0x00000004\trela\tR_X\tsym+0xg\t0x0\n"
    ~actual:(reloc_of ~rela:true "00000004 R_X               sym+0xg\n");

  (* Offsets zero-pad to 8 so a bytewise sort agrees with address order, and a
     wide 64-bit offset is not truncated. *)
  check_eq "offset: padded to 8" ~expected:".text\t0x0000000f\trela\tR_X\ts\t0x0\n"
    ~actual:(reloc_of ~rela:true "f R_X               s\n");
  check_eq "offset: wider than 8 is preserved" ~expected:".text\t0x123456789\trela\tR_X\ts\t0x0\n"
    ~actual:(reloc_of ~rela:true "0000000123456789 R_X               s\n");

  (* Sorting is bytewise over the rendered row, so records arrive in address
     order regardless of the order objdump printed them. *)
  check_eq "records sort by rendered row"
    ~expected:(".text\t0x00000001\trela\tR_X\ta\t0x0\n" ^ ".text\t0x00000002\trela\tR_X\tb\t0x0\n")
    ~actual:(reloc_of ~rela:true "00000002 R_X               b\n00000001 R_X               a\n")

let test_section_scope () =
  (* .eh_frame records are dropped by the allowlist: the .cfi_* directives that
     produce them are discarded at parse and never reproduced. *)
  let body =
    "RELOCATION RECORDS FOR [.text]:\n\
     OFFSET   TYPE              VALUE\n\
     00000004 R_X               keep\n\n\
     RELOCATION RECORDS FOR [.eh_frame]:\n\
     OFFSET   TYPE              VALUE\n\
     0000001c R_Y               drop\n"
  in
  check_eq "scope: .eh_frame records dropped" ~expected:".text\t0x00000004\trela\tR_X\tkeep\t0x0\n"
    ~actual:(unwrap (Gnu_output.relocations ~objdump_r:body ~rela:true))

let test_has_rela () =
  (* Transcribes the shell's anchored grep. The negative cases are the ones an
     unanchored scan would wrongly accept - this predicate decides whether the
     addend is 0 or lives in the instruction field. *)
  check "has_rela: bracketed index" (Gnu_output.has_rela "  [ 2] .rela.text  RELA  0000\n");
  check "has_rela: two-digit index" (Gnu_output.has_rela "  [12] .rela.eh_frame RELA\n");
  check "has_rela: plain .rel is not .rela" (not (Gnu_output.has_rela "  [ 2] .rel.text REL\n"));
  check "has_rela: no sections at all" (not (Gnu_output.has_rela "Section Headers:\n"));
  check "has_rela: name not after a bracket"
    (not (Gnu_output.has_rela "  some text .rela.text elsewhere\n"));
  check "has_rela: non-numeric index rejected" (not (Gnu_output.has_rela "  [ x] .rela.text\n"));
  check "has_rela: bracket must start the line" (not (Gnu_output.has_rela "x [ 2] .rela.text\n"))

let test_symbols_shape () =
  let table =
    "Symbol table '.symtab' contains 4 entries:\n\
    \   Num:    Value          Size Type    Bind   Vis      Ndx Name\n\
    \     0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND \n\
    \     1: 0000000000000000     0 FILE    LOCAL  DEFAULT  ABS foo.c\n\
    \     2: 0000000000000000     0 NOTYPE  GLOBAL DEFAULT  UND undef_sym\n\
    \     3: 0000000000000000     4 OBJECT  GLOBAL HIDDEN     3 g\n"
  in
  check_eq "symbols: index 0 and FILE dropped, UND classified, case lowered"
    ~expected:"g\tglobal\thidden\t3\tdefined\nundef_sym\tglobal\tdefault\tUND\tundefined\n"
    ~actual:(unwrap (Gnu_output.symbols ~readelf_s:table));

  (* sort -u dedups identical rows but must NOT merge two entries differing
     only in ndx - the aarch64 corpus really does carry $d at two indices. *)
  let dup =
    "   Num:    Value          Size Type    Bind   Vis      Ndx Name\n\
    \     1: 0 0 NOTYPE  LOCAL  DEFAULT    3 $d\n\
    \     2: 0 0 NOTYPE  LOCAL  DEFAULT    6 $d\n\
    \     3: 0 0 NOTYPE  LOCAL  DEFAULT    3 $d\n"
  in
  check_eq "symbols: dedup keeps distinct ndx"
    ~expected:"$d\tlocal\tdefault\t3\tdefined\n$d\tlocal\tdefault\t6\tdefined\n"
    ~actual:(unwrap (Gnu_output.symbols ~readelf_s:dup))

let test_alloc_flags () =
  (* The flag is a whole word: a section flagged NOALLOC must not be reported
     as allocated by a substring match. *)
  let h =
    "Sections:\n\
     Idx Name          Size      VMA       LMA       File off  Algn\n\
    \  0 .text         00000022  00000000  00000000  00000040  2**4\n\
    \                  CONTENTS, ALLOC, LOAD, READONLY, CODE\n\
    \  1 .weird        00000004  00000000  00000000  00000064  2**2\n\
    \                  CONTENTS, NOALLOC\n\
    \  2 .data         00000004  00000000  00000000  00000068  2**2\n\
    \                  CONTENTS, ALLOC, LOAD, DATA\n"
  in
  check_eq "alloc: whole-word flag match" ~expected:".text .data"
    ~actual:(String.concat " " (unwrap (Gnu_output.alloc_sections h)));
  check_eq "section_size: reads the size column" ~expected:"00000022"
    ~actual:(Option.value ~default:"" (Gnu_output.section_size h ~name:".text"));
  check "alloc: empty input is an error" (Result.is_error (Gnu_output.alloc_sections ""))

let () =
  if Array.length Sys.argv < 3 then (
    prerr_endline "usage: test_gnu_output.exe <data/gnu> <fixtures>";
    exit 2);
  let data = Fpath.v Sys.argv.(1) and corpus = Fpath.v Sys.argv.(2) in
  print_endline "gnu_output:";
  test_against_corpus data corpus;
  test_addends ();
  test_section_scope ();
  test_has_rela ();
  test_symbols_shape ();
  test_alloc_flags ();
  if !failures > 0 then (
    Printf.printf "gnu_output: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "gnu_output: all %d checks passed\n" !checks
