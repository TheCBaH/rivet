(* Identifier, Repo, Hex_dump and Tool_fs.

   Each test uses its own temporary parent and asserts that the specific named
   child is absent afterwards. Counting entries in /tmp would pass on a machine
   where something else happened to clean up. *)

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

let with_tmp f =
  let dir = Filename.temp_file "tools_fs" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let rec rm p =
    match Unix.lstat p with
    | { Unix.st_kind = Unix.S_DIR; _ } ->
        Array.iter (fun e -> rm (Filename.concat p e)) (Sys.readdir p);
        Unix.rmdir p
    | _ -> Unix.unlink p
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in
  Fun.protect ~finally:(fun () -> rm dir) (fun () -> f (Fpath.v dir))

let write p s =
  let oc = open_out_bin (Fpath.to_string p) in
  output_string oc s;
  close_out oc

(* {1 Identifier} *)

let test_identifier () =
  let ok s = is_ok (Identifier.parse s) and no s = is_err (Identifier.parse s) in
  check "identifier: plain name accepted" (ok "return42");
  check "identifier: empty rejected" (no "");
  check "identifier: . rejected" (no ".");
  check "identifier: .. rejected" (no "..");
  check "identifier: separator rejected" (no "a/b");
  check "identifier: tab rejected" (no "a\tb");
  check "identifier: newline rejected" (no "a\nb");
  check "identifier: NUL rejected" (no "a\000b");
  (* A leading dash is reinterpreted as an option by essentially every program
     the tools invoke, which is why it is a validation error and not a quoting
     problem. *)
  check "identifier: leading dash rejected" (no "-case");
  check "identifier: case name allows [A-Za-z0-9_-]"
    (is_ok (Identifier.parse_case_name "args_arith-2"));
  check "identifier: case name rejects a dot" (is_err (Identifier.parse_case_name "a.b"));
  (* D6: source is used directly as a copy and compile path, so today
     source=../../x escapes the case root. Every component is checked, not just
     the first - "source/../../x" is the escape that matters. *)
  check "path: nested relative accepted"
    (is_ok (Identifier.relative_path "source/asm_test_entry.c"));
  check "path: .. anywhere rejected" (is_err (Identifier.relative_path "source/../../x"));
  check "path: leading .. rejected" (is_err (Identifier.relative_path "../x"));
  check "path: absolute rejected" (is_err (Identifier.relative_path "/etc/passwd"));
  check "path: empty rejected" (is_err (Identifier.relative_path ""));
  check "path: tab rejected" (is_err (Identifier.relative_path "a\tb/c"));
  check "path: component with leading dash rejected" (is_err (Identifier.relative_path "a/-b"))

(* {2 Repo} *)

let make_repo root =
  write Fpath.(root / "Makefile") "";
  Unix.mkdir Fpath.(to_string (root / "tools")) 0o700;
  write Fpath.(root / "tools" / "target-matrix.sh") "";
  Unix.mkdir Fpath.(to_string (root / "asm")) 0o700;
  write Fpath.(root / "asm" / "dune-project") ""

let no_env _ = None

let test_repo () =
  with_tmp (fun root ->
      make_repo root;
      let r = Repo.resolve ~cli:(Some root) ~env:no_env ~cwd:root in
      check "repo: a root with all three sentinels resolves" (is_ok r);
      (match r with
      | Ok r ->
          check "repo: fixture corpus path"
            (Fpath.to_string (Repo.fixture_corpus r)
            = Fpath.to_string root ^ "/asm/fixtures/compcert-3.17")
      | Error _ -> check "repo: fixture corpus path" false);
      (* THE %{workspace_root} TEST. asm/ is the dune workspace root but not the
         repository root, and feeding it here must be rejected by name. *)
      let asm = Fpath.(root / "asm") in
      (match Repo.resolve ~cli:(Some asm) ~env:no_env ~cwd:asm with
      | Ok _ -> check "repo: asm/ is REJECTED as a repository root" false
      | Error e ->
          let d = (Err.Error.kind e).Tool_error.detail in
          let contains needle =
            let n = String.length needle and m = String.length d in
            let rec go i = i + n <= m && (String.sub d i n = needle || go (i + 1)) in
            go 0
          in
          check "repo: asm/ is REJECTED as a repository root" true;
          check "repo: the rejection names the missing sentinel" (contains "Makefile"));
      (* Precedence: cli beats the environment. *)
      let env = function "COMPCERT_REPO_ROOT" -> Some (Fpath.to_string asm) | _ -> None in
      check "repo: cli argument beats COMPCERT_REPO_ROOT"
        (is_ok (Repo.resolve ~cli:(Some root) ~env ~cwd:asm));
      (* And the environment beats the cwd search. *)
      let env_ok = function "COMPCERT_REPO_ROOT" -> Some (Fpath.to_string root) | _ -> None in
      check "repo: COMPCERT_REPO_ROOT is used when there is no cli argument"
        (is_ok (Repo.resolve ~cli:None ~env:env_ok ~cwd:asm));
      (* Upward search finds the root from a nested directory. *)
      check "repo: upward search from a nested cwd"
        (is_ok (Repo.resolve ~cli:None ~env:no_env ~cwd:asm)))

(* {3 Hex_dump} *)

let test_hex_dump () =
  check_eq "hex: empty input yields a ZERO-BYTE result, not a bare newline" ~expected:""
    ~actual:(Hex_dump.of_bytes "");
  check_eq "hex: one byte" ~expected:"2a\n" ~actual:(Hex_dump.of_bytes "\x2a");
  let sixteen = String.init 16 (fun i -> Char.chr i) in
  check_eq "hex: exactly 16 bytes is one line"
    ~expected:"00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f\n"
    ~actual:(Hex_dump.of_bytes sixteen);
  let seventeen = sixteen ^ "\xff" in
  check_eq "hex: 17 bytes wraps after 16"
    ~expected:"00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f\nff\n"
    ~actual:(Hex_dump.of_bytes seventeen);
  (* The shape the committed corpus actually uses: a full line then a short one. *)
  let corpus_line = "48 83 ec 08 48 8d 44 24 10 48 89 04 24 b8 2a 00\n00 00 48 83 c4 08 c3\n" in
  (match Hex_dump.parse corpus_line with
  | Ok bytes ->
      check_eq "hex: round-trips a real corpus dump" ~expected:corpus_line
        ~actual:(Hex_dump.of_bytes bytes)
  | Error _ -> check "hex: round-trips a real corpus dump" false);
  check "hex: uppercase rejected" (is_err (Hex_dump.parse "FF\n"));
  check "hex: odd-length token rejected" (is_err (Hex_dump.parse "f\n"));
  check "hex: non-hex rejected" (is_err (Hex_dump.parse "zz\n"));
  check "hex: missing final newline rejected" (is_err (Hex_dump.parse "ff"));
  check "hex: over-long line rejected"
    (is_err (Hex_dump.parse (String.concat " " (List.init 17 (fun _ -> "00")) ^ "\n")));
  check "hex: empty string parses to empty" (Hex_dump.parse "" = Ok "")

(* {4 Tool_fs.files} *)

let test_files () =
  with_tmp (fun root ->
      Unix.mkdir Fpath.(to_string (root / "sub")) 0o700;
      write Fpath.(root / "b.txt") "b";
      write Fpath.(root / "a.txt") "a";
      write Fpath.(root / "sub" / "c.txt") "c";
      write Fpath.(root / "manifest.txt") "m";
      write Fpath.(root / "manifest.txt.new") "m";
      (* A symlink to a regular file is NOT a file: `find -type f` does not
         follow symlinks, and treating it as one would record the target's bytes
         under the link's name. *)
      Unix.symlink "a.txt" Fpath.(to_string (root / "link.txt"));
      (* A symlinked directory must not be descended, or a cycle would hang the
         traversal and an out-of-tree link would silently pull in foreign bytes. *)
      Unix.symlink "sub" Fpath.(to_string (root / "linkdir"));
      let exclude p = String.length p >= 12 && String.sub p 0 12 = "manifest.txt" in
      match Tool_fs.files ~root ~exclude with
      | Error _ -> check "files: traversal succeeds" false
      | Ok fs ->
          check "files: traversal succeeds" true;
          check_eq "files: sorted, root-relative, symlinks excluded, manifest.txt* excluded"
            ~expected:"a.txt,b.txt,sub/c.txt" ~actual:(String.concat "," fs);
          (* String.compare is byte-wise, which is exactly LC_ALL=C sort. *)
          check "files: order equals its own sort" (fs = List.sort String.compare fs))

(* {5 Tool_fs staging} *)

let read_file p =
  let ic = open_in_bin (Fpath.to_string p) in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let exists p = Sys.file_exists (Fpath.to_string p)

let staging_test name f =
  with_tmp (fun root ->
      let final = Fpath.(root / "manifest.txt") in
      let new_ = Fpath.(root / "manifest.txt.new") in
      let diff = Fpath.(root / "manifest.txt.diff") in
      f ~root ~final ~new_ ~diff);
  ignore name

let test_staging () =
  (* Success: install committed, BOTH siblings gone. *)
  staging_test "commit" (fun ~root:_ ~final ~new_ ~diff ->
      write final "old\n";
      Unix.chmod (Fpath.to_string final) 0o640;
      let r =
        Tool_fs.with_staging ~final ~install_suffix:".new" ~auxiliary_suffixes:[ ".diff" ]
          (fun ~install:_ ~auxiliary ~write_install ~write_auxiliary ~commit ->
            let ( let* ) = Result.bind in
            let* () = write_install "new\n" in
            let* d = auxiliary ".diff" in
            let* () = write_auxiliary d "the diff\n" in
            commit ())
      in
      check "staging: commit succeeds" (is_ok r);
      check_eq "staging: the install stage was installed" ~expected:"new\n"
        ~actual:(read_file final);
      check "staging: .new removed" (not (exists new_));
      check "staging: .diff removed" (not (exists diff));
      (* D13: the existing file's mode is preserved, where the shell's mv resets
         it to whatever the umask produced. *)
      check "staging: existing mode preserved (D13)"
        ((Unix.stat (Fpath.to_string final)).Unix.st_perm = 0o640));

  (* Failure writing the install stage: nothing installed, nothing left. *)
  staging_test "install-fails" (fun ~root:_ ~final ~new_ ~diff ->
      write final "old\n";
      let r =
        Tool_fs.with_staging ~final ~install_suffix:".new" ~auxiliary_suffixes:[ ".diff" ]
          (fun ~install:_ ~auxiliary:_ ~write_install:_ ~write_auxiliary:_ ~commit:_ ->
            Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
              (Tool_error.v Tool_error.Write_file "render failed"))
      in
      check "staging: a render failure propagates" (is_err r);
      check_eq "staging: the live file is untouched" ~expected:"old\n" ~actual:(read_file final);
      check "staging: no .new left behind" (not (exists new_));
      check "staging: no .diff left behind" (not (exists diff)));

  (* An exception mid-body still cleans up. *)
  staging_test "exception" (fun ~root:_ ~final ~new_ ~diff ->
      write final "old\n";
      (try
         ignore
           (Tool_fs.with_staging ~final ~install_suffix:".new" ~auxiliary_suffixes:[ ".diff" ]
              (fun ~install:_ ~auxiliary:_ ~write_install ~write_auxiliary:_ ~commit:_ ->
                ignore (write_install "half\n");
                failwith "boom"))
       with Failure _ -> ());
      check "staging: cleanup runs on an exception" ((not (exists new_)) && not (exists diff));
      check_eq "staging: the live file survives an exception" ~expected:"old\n"
        ~actual:(read_file final));

  (* A new file gets 0644 rather than inheriting nothing. *)
  staging_test "new-file-mode" (fun ~root:_ ~final ~new_:_ ~diff:_ ->
      let r =
        Tool_fs.with_staging ~final ~install_suffix:".new" ~auxiliary_suffixes:[]
          (fun ~install:_ ~auxiliary:_ ~write_install ~write_auxiliary:_ ~commit ->
            Result.bind (write_install "fresh\n") commit)
      in
      check "staging: a first write succeeds" (is_ok r);
      check "staging: a new file is 0644" ((Unix.stat (Fpath.to_string final)).Unix.st_perm = 0o644));

  (* Suffix declarations are validated, so two aliases for one sibling - or an
     escape from the final's directory - cannot be declared at all. *)
  staging_test "bad-suffixes" (fun ~root:_ ~final ~new_:_ ~diff:_ ->
      let attempt install_suffix auxiliary_suffixes =
        Tool_fs.with_staging ~final ~install_suffix ~auxiliary_suffixes
          (fun ~install:_ ~auxiliary:_ ~write_install:_ ~write_auxiliary:_ ~commit:_ -> Ok ())
      in
      check "staging: empty suffix rejected (it would alias the final itself)"
        (is_err (attempt "" []));
      check "staging: separator in suffix rejected" (is_err (attempt "/x" []));
      check "staging: duplicate suffixes rejected" (is_err (attempt ".new" [ ".new" ]));
      check "staging: an undeclared auxiliary name is rejected"
        (is_err
           (Tool_fs.with_staging ~final ~install_suffix:".new" ~auxiliary_suffixes:[ ".diff" ]
              (fun ~install:_ ~auxiliary ~write_install:_ ~write_auxiliary:_ ~commit:_ ->
                Result.map (fun _ -> ()) (auxiliary ".other")))))

let test_read_and_hash () =
  with_tmp (fun root ->
      let p = Fpath.(root / "x.txt") in
      write p "abc";
      check "read: round-trips" (Tool_fs.read p = Ok "abc");
      (* The value the manifest records, compared against the known SHA-256 of
         "abc" rather than against another call to the same function. *)
      check "sha256: matches the known digest of \"abc\""
        (Tool_fs.sha256 p = Ok "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
      check "read: a missing file is an error, not an exception"
        (is_err (Tool_fs.read Fpath.(root / "absent"))))

let () =
  print_endline "fs:";
  test_identifier ();
  test_repo ();
  test_hex_dump ();
  test_files ();
  test_staging ();
  test_read_and_hash ();
  if !failures > 0 then (
    Printf.printf "fs: %d failures\n" !failures;
    exit 1)
  else print_endline "fs: all checks passed"
