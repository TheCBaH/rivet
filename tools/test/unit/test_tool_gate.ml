(* The tool gate's pure parts.

   Two groups, and neither needs a shell - which is why they are here rather
   than in the repository-level suite that retired with the shell in Phase 8.
   The extraction-fidelity half of gate_snippet_tests genuinely died with its
   subject; these checks never had one and were only living there because that
   is where they were written.

   The transcriptions are the reason this file exists. A passing gate run
   exercises only two of the four, and the two it does not are on the diagnostic
   paths - where being quietly wrong costs exactly the information the gate is
   there to produce. *)

open Compcert_tools

let contains hay needle =
  let n = String.length hay and m = String.length needle in
  let rec go i = i + m <= n && (String.sub hay i m = needle || go (i + 1)) in
  go 0

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
    Printf.printf "  FAIL %s\n    want: %S\n    got:  %S\n" name expected actual;
    incr failures)

(* {1 The snippets}

   Their bytes are no longer compared against the shell they were lifted from -
   that comparison retired with the shell. What survives is the PROPERTY those
   bytes exist to carry, which the byte comparison never asserted directly: six
   distinct statuses, clear of the range the host runner owns. A duplicated
   status would have passed every byte comparison and still destroyed the
   discrimination the gate depends on.

   The bytes themselves stay covered behaviourally: a wrong snippet makes its
   target exit with something other than the status the gate demands. *)
let test_snippets () =
  let statuses = List.map Gate_snippet.expected_status Target.all in
  check "expected statuses are pairwise distinct"
    (List.length (List.sort_uniq compare statuses) = List.length statuses);
  (* 124-127 belong to the host runner: the gate treats 124 as its own timeout,
     so a snippet exiting there would be indistinguishable from a hang. *)
  check "expected statuses stay clear of the timeout range"
    (List.for_all (fun n -> n >= 0 && n <= 123) statuses);
  List.iter
    (fun t ->
      let src = Gate_snippet.source t in
      let name = Target.to_string t in
      check
        (Printf.sprintf "%s: snippet declares _start" name)
        (String.length src > 0 && String.ends_with ~suffix:"\n" src && contains src ".globl _start"))
    Target.all

let test_squeeze () =
  let s i w =
    check_eq
      (Printf.sprintf "squeeze_spaces %S" i)
      ~expected:w ~actual:(Tool_gate_cmd.squeeze_spaces i)
  in
  s "a  b" "a b";
  s "a     b" "a b";
  s "a b" "a b";
  (* sed 's/  */ /g' touches SPACES only - a tab is not a space to it, and the
     -M help columns are tab-free, so collapsing tabs would be inventing a
     normalization the shell never did. *)
  s "a\t\tb" "a\t\tb";
  s "  a  " " a ";
  s "" ""

let test_first_field () =
  let s i w =
    check_eq (Printf.sprintf "first_field %S" i) ~expected:w ~actual:(Tool_gate_cmd.first_field i)
  in
  s "virt  QEMU 10.0 ARM Virtual Machine (alias of virt-10.0)" "virt";
  (* awk ignores leading blanks and splits on tabs as well as spaces. *)
  s "   virt  x" "virt";
  s "\tvirt\tx" "virt";
  s "" "";
  s "   " "";
  (* The anchoring that makes the alias assertion mean anything: `$1 == "virt"`
     must NOT match the versioned machine the alias points at, or a retired
     alias would keep passing on the strength of its own target. *)
  check "first_field distinguishes virt from virt-10.0"
    (Tool_gate_cmd.first_field "virt-10.0  QEMU 10.0 ARM Virtual Machine" <> "virt")

let test_flatten () =
  let s i w =
    check_eq (Printf.sprintf "flatten %S" i) ~expected:w ~actual:(Tool_gate_cmd.flatten i)
  in
  (* tr '\n' ' ' converts the FINAL newline too, so a one-line diagnostic renders
     with a trailing space - and that space was in the shell's output. *)
  s "one line\n" "one line ";
  s "a\nb\n" "a b ";
  s "" "";
  s "\n" " ";
  s "no newline" "no newline"

let test_last_bytes () =
  let s n i w =
    check_eq
      (Printf.sprintf "last_bytes %d %S" n i)
      ~expected:w ~actual:(Tool_gate_cmd.last_bytes n i)
  in
  s 3 "abcdef" "def";
  s 6 "abcdef" "abcdef";
  s 9 "abcdef" "abcdef";
  s 0 "abcdef" ""

let () =
  print_endline "tool-gate:";
  test_snippets ();
  test_squeeze ();
  test_first_field ();
  test_flatten ();
  test_last_bytes ();
  if !failures > 0 then (
    Printf.printf "tool-gate: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "tool-gate: all %d checks passed\n" !checks
