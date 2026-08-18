(* Manifest and Corpus.

   Every divergence this module deliberately introduces gets a test that EXPECTS
   the difference, so none of them can be mistaken for compatibility. *)

open Compcert_tools

let failures = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let check_eq name ~expected ~actual =
  if String.equal expected actual then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n    expected: %S\n    actual:   %S\n" name expected actual;
    incr failures)

let is_ok = function Ok _ -> true | Error _ -> false
let is_err = function Ok _ -> false | Error _ -> true
let h = String.make 64 'a'
let h2 = String.make 64 'b'

(* {1 Escaping - the shell's order, and its exact inverse (D7)} *)

let test_escaping () =
  check_eq "escape: backslash first" ~expected:"\\\\" ~actual:(Manifest.escape "\\");
  check_eq "escape: tab" ~expected:"\\t" ~actual:(Manifest.escape "\t");
  check_eq "escape: newline" ~expected:"\\n" ~actual:(Manifest.escape "\n");
  (* Order matters: escaping the tab first would then re-escape its own
     introduced backslash. *)
  check_eq "escape: a backslash followed by a tab" ~expected:"\\\\\\t"
    ~actual:(Manifest.escape "\\\t");
  List.iter
    (fun s ->
      match Manifest.unescape (Manifest.escape s) with
      | Ok back -> check_eq (Printf.sprintf "escape: round-trips %S" s) ~expected:s ~actual:back
      | Error _ -> check (Printf.sprintf "escape: round-trips %S" s) false)
    [ "plain"; "\\"; "\t"; "\n"; "a\\tb"; "\\\\"; "" ];
  (* D7: the shell's reader never unescapes at all, so it accepts sequences it
     could not have produced. Accepting them here would mask a corrupted file. *)
  check "unescape: an unknown escape is rejected (D7)" (is_err (Manifest.unescape "a\\qb"));
  check "unescape: a lone trailing backslash is rejected" (is_err (Manifest.unescape "a\\"))

(* {2 Parsing} *)

let parse_ok text = match Manifest.parse text with Ok m -> Some m | Error _ -> None

let test_parse () =
  check "parse: empty input is an empty manifest"
    (match Manifest.parse "" with Ok m -> Manifest.records m = [] | Error _ -> false);
  (* D4: the shell's `while read` silently drops an unterminated final record,
     which is how a truncated manifest reads as a shorter one. *)
  check "parse: a missing final newline is rejected (D4)"
    (is_err (Manifest.parse (Printf.sprintf "sha256:a.s\t%s" h)));
  check "parse: the same input WITH a newline is accepted"
    (is_ok (Manifest.parse (Printf.sprintf "sha256:a.s\t%s\n" h)));
  (* D5 *)
  check "parse: a duplicate sha256 path is rejected (D5)"
    (is_err (Manifest.parse (Printf.sprintf "sha256:a.s\t%s\nsha256:a.s\t%s\n" h h2)));
  check "parse: a short hash is rejected (D5)" (is_err (Manifest.parse "sha256:a.s\tabc\n"));
  check "parse: an uppercase hash is rejected (D5)"
    (is_err (Manifest.parse (Printf.sprintf "sha256:a.s\t%s\n" (String.make 64 'A'))));
  (* D12: the shell's carry-forward grep emits EVERY match, so duplicates are
     carried forward verbatim today and the manifest grows on each rehash. *)
  check "parse: a duplicate ccomp-version is rejected (D12)"
    (is_err (Manifest.parse "ccomp-version:arm\tv1\nccomp-version:arm\tv2\n"));
  check "parse: two DIFFERENT targets are fine"
    (is_ok (Manifest.parse "ccomp-version:arm\tv1\nccomp-version:x86_64\tv2\n"));
  (* D6: the key side has no escape layer, so an unusable path cannot
     round-trip - it would become a different record, or two. *)
  check "parse: an absolute recorded path is rejected (D6)"
    (is_err (Manifest.parse (Printf.sprintf "sha256:/etc/passwd\t%s\n" h)));
  check "parse: a recorded path escaping the case root is rejected (D6)"
    (is_err (Manifest.parse (Printf.sprintf "sha256:../../x\t%s\n" h)));
  (* A bare key and an empty value are DIFFERENT records. *)
  (match parse_ok "ccomp-args:arm\n" with
  | Some m ->
      check "parse: a bare key has value None"
        (Manifest.find m (Manifest.Ccomp_args Target.Arm) = Some None)
  | None -> check "parse: a bare key has value None" false);
  (match parse_ok "ccomp-args:arm\t\n" with
  | Some m ->
      check "parse: key + empty value has value (Some \"\")"
        (Manifest.find m (Manifest.Ccomp_args Target.Arm) = Some (Some ""))
  | None -> check "parse: key + empty value has value (Some \"\")" false);
  (* An unknown target in a targeted key is a parse error, not an Unknown
     record: silently keeping it would let a typo survive a rehash. *)
  check "parse: an unknown target in a key is rejected"
    (is_err (Manifest.parse "ccomp-version:sparc\tv\n"));
  (* P3: first wins. *)
  match parse_ok "source\tsource/a.c\nsource\tsource/b.c\n" with
  | Some m ->
      check "parse: duplicate source - FIRST wins (P3)"
        (Manifest.source_rel m = Ok (Some "source/a.c"))
  | None -> check "parse: duplicate source - FIRST wins (P3)" false

(* Accumulation: EVERY complaint, not just the first. This is the one shape
   Err.Accum fits, and it is why parse's error type is Accum.errors. *)
let test_parse_accumulates () =
  match Manifest.parse "sha256:a.s\tshort\nsha256:b.s\talsoshort\n" with
  | Ok _ -> check "parse: accumulates every malformed record" false
  | Error e ->
      let n = List.length (Err.Error.kind e) in
      check "parse: accumulates every malformed record (2 of them)" (n = 2)

(* {3 Serialization} *)

let test_serialize () =
  let m =
    Manifest.of_records
      [
        { Manifest.key = Manifest.Sha256 "b.s"; value = Some h2 };
        { Manifest.key = Manifest.Generator; value = Some "tools/asm-fixture-gen.sh" };
        { Manifest.key = Manifest.Sha256 "a.s"; value = Some h };
        { Manifest.key = Manifest.Ccomp_args Target.Arm; value = None };
      ]
  in
  let out = Manifest.serialize m in
  check_eq "serialize: bytewise sorted, bare key keeps no tab"
    ~expected:
      (Printf.sprintf
         "ccomp-args:arm\ngenerator\ttools/asm-fixture-gen.sh\nsha256:a.s\t%s\nsha256:b.s\t%s\n" h
         h2)
    ~actual:out;
  (* The sort must be String.compare, which is bytewise, because that is what
     LC_ALL=C sort does - a locale-aware sort would order these differently. *)
  (* Drop the trailing "" that splitting a newline-terminated string produces;
     it sorts first and would make this assertion fail for the wrong reason. *)
  let lines = List.filter (fun l -> l <> "") (String.split_on_char '\n' out) in
  check "serialize: order equals its own bytewise sort" (lines = List.sort String.compare lines)

(* {4 The real committed manifests round-trip} *)

(* Only the manifests write_manifest actually produces: the per-case one at a
   corpus case root, and the gas-xref one.

   The corpus ALSO contains */oracle/linked/manifest.txt, which is a different
   format entirely - asm-fixture-oracle.sh writes three-field records like
   ".text<TAB>0x40000000<TAB>linked/text.hex", a section-to-address-to-file
   table with no escaping layer. Pointing this parser at one would be a category
   error, and it is why the fixture scripts exclude manifest.txt* from traversal
   at every depth rather than only at the case root. *)
let rec find_manifests dir acc =
  Array.fold_left
    (fun acc entry ->
      let p = Filename.concat dir entry in
      match Unix.lstat p with
      | { Unix.st_kind = Unix.S_DIR; _ } when entry <> "oracle" -> find_manifests p acc
      | { Unix.st_kind = Unix.S_REG; _ } when entry = "manifest.txt" -> p :: acc
      | _ -> acc)
    acc (Sys.readdir dir)

let test_real_manifests fixtures =
  let ms = List.sort compare (find_manifests fixtures []) in
  check "round-trip: manifests were found" (ms <> []);
  List.iter
    (fun p ->
      let text = match Tool_fs.read (Fpath.v p) with Ok s -> s | Error _ -> "" in
      match Manifest.parse text with
      | Error e ->
          check
            (Printf.sprintf "round-trip: %s parses" (Filename.basename (Filename.dirname p)))
            false;
          List.iter
            (fun x -> print_endline ("       " ^ (Err.Error.kind x).Tool_error.detail))
            (Err.Error.kind e)
      | Ok m ->
          (* Byte-exact: a parser that normalized anything - dropped a bare
             key's distinction, re-sorted differently, re-escaped differently -
             would show up here and nowhere else. *)
          check_eq
            (Printf.sprintf "round-trip: %s/%s is byte-identical"
               (Filename.basename (Filename.dirname (Filename.dirname p)))
               (Filename.basename (Filename.dirname p)))
            ~expected:text ~actual:(Manifest.serialize m))
    ms

(* {5 Corpus} *)

let test_corpus fixtures =
  let corpus = Fpath.(v fixtures / "compcert-3.17") in
  (match Corpus.discover corpus with
  | Ok cases ->
      check "corpus: discovery finds the committed cases" (List.length cases >= 6);
      check "corpus: cases are in bytewise order"
        (let names = List.map (fun c -> c.Corpus.name) cases in
         names = List.sort String.compare names)
  | Error _ -> check "corpus: discovery finds the committed cases" false);
  check "corpus: an empty corpus is an ERROR, not an empty success"
    (is_err (Corpus.discover Fpath.(v fixtures / "does-not-exist")));
  check "corpus: an unknown case name is rejected" (is_err (Corpus.resolve corpus "no_such_case"));
  check "corpus: a case name with a separator is rejected" (is_err (Corpus.resolve corpus "a/b"));
  check "corpus: a leading-dash case name is rejected" (is_err (Corpus.resolve corpus "-rf"));
  check "corpus: return42 resolves" (is_ok (Corpus.resolve corpus "return42"));
  (* stem_of_source is why return42 keeps the asm_test_entry.c spelling. *)
  check "corpus: stem comes from the source path"
    (Corpus.stem_of_source "source/asm_test_entry.c" = Ok "asm_test_entry");
  check "corpus: a non-.c source is rejected" (is_err (Corpus.stem_of_source "source/a.txt"));
  check "corpus: an escaping source is rejected (D6)"
    (is_err (Corpus.stem_of_source "../../etc/passwd.c"));
  (* A clean case reports zero findings and a positive count. *)
  match Corpus.resolve corpus "return42" with
  | Error _ -> check "corpus: a clean case has no findings" false
  | Ok case -> (
      match Tool_fs.read Fpath.(case.Corpus.root / "manifest.txt") with
      | Error _ -> check "corpus: a clean case has no findings" false
      | Ok text -> (
          match Manifest.parse text with
          | Error _ -> check "corpus: a clean case has no findings" false
          | Ok m -> (
              match Corpus.check case.Corpus.root m with
              | Error _ -> check "corpus: a clean case has no findings" false
              | Ok (seen, findings) ->
                  check "corpus: a clean case has no findings" (findings = []);
                  check "corpus: and a positive record count" (seen > 0))))

let () =
  let fixtures = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  print_endline "manifest:";
  test_escaping ();
  test_parse ();
  test_parse_accumulates ();
  test_serialize ();
  if fixtures <> "" then (
    test_real_manifests fixtures;
    test_corpus fixtures);
  if !failures > 0 then (
    Printf.printf "manifest: %d failures\n" !failures;
    exit 1)
  else print_endline "manifest: all checks passed"
