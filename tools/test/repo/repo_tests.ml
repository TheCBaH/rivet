(* Tests that need files outside the dune workspace. Run by Make, with the
   repository root as argv.

   The one documented exception to "production code never invokes a shell": this
   test runs a fixed repository script in order to compare Target against the
   shell matrix. Production code does not do this.

   Phase 7 moved ownership, so what this proves changed and the test stayed.
   tools/target-matrix.sh is now GENERATED from Target by Target_emit, which
   makes the comparison a round trip - OCaml to shell text, sourced by bash,
   back to a comparison. That is not a tautology and it is not weaker in the way
   that matters: it is exactly what catches a quoting bug, a lost array element,
   or a default the renderer forgot to emit, none of which any other check would
   see. What it no longer proves is that OCaml agrees with an INDEPENDENTLY
   written shell - and it cannot, because nobody writes that shell any more.
   `make tools-matrix-diff` is what holds the committed file to the source. *)

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
    Printf.printf "  FAIL %s\n    shell:  %S\n    ocaml:  %S\n" name expected actual;
    incr failures)

let run_capture root args =
  match
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_inherit
         ~parsed_output:true ~accepted:Process_status.Zero_only ~label:"shell"
         (Fpath.to_string root) args)
  with
  | Ok { Tool_process.stdout = Some s; _ } -> Some s
  | _ -> None

let lines s = List.filter (fun l -> l <> "") (String.split_on_char '\n' s)

let tsv s =
  List.filter_map
    (fun line ->
      match String.index_opt line '\t' with
      | Some i -> Some (String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1))
      | None -> Some (line, ""))
    (lines s)

(* {1 All sixteen shell values, verbatim}

   Including HAS_SYSROOT, which the OCaml side DERIVES. Comparing only the
   derived form would let a stale HAS_SYSROOT assignment in the shell hide
   behind an equivalent computed value - and the shell is what the other seven
   scripts still read. *)
let ocaml_view t =
  let c = Target.config t in
  let opt = Option.value ~default:"" in
  [
    ("CONFIGURE_TARGET", c.Target.configure_target);
    ("TOOLPREFIX", c.Target.toolprefix);
    ("QEMU_BIN", c.Target.qemu_bin);
    ("QEMU_SYSROOT", opt c.Target.qemu_sysroot);
    ("CCOMP_EXTRA_ARGS", String.concat " " c.Target.ccomp_args);
    ("COMPCERT_CONFIGURE_ARGS", String.concat " " c.Target.compcert_configure_args);
    ("AS_FLAGS", String.concat " " c.Target.as_args);
    ("LD_FLAGS", String.concat " " c.Target.ld_args);
    ("LINKER_EMULATION", opt c.Target.linker_emulation);
    ("ELF_CLASS", c.Target.elf_class);
    ("WORD_SIZE", string_of_int c.Target.word_size);
    ("HAS_SYSROOT", if Target.has_sysroot t then "true" else "false");
    ("READELF_MACHINE", c.Target.readelf_machine);
    ("LINK_TEXT_ADDR", Target.hex c.Target.link.Target.text);
    ("LINK_RODATA_ADDR", Target.hex c.Target.link.Target.rodata);
    ("LINK_DATA_ADDR", Target.hex c.Target.link.Target.data);
  ]

let test_target_db_agrees root =
  let dump = Fpath.(root / "tools" / "dev" / "dump-target-config.sh") in
  List.iter
    (fun t ->
      let name = Target.to_string t in
      match run_capture dump [ name ] with
      | None -> check (Printf.sprintf "target_db: %s: dump script ran" name) false
      | Some out ->
          let shell = tsv out in
          let ours = ocaml_view t in
          check
            (Printf.sprintf "target_db: %s: all sixteen values present" name)
            (List.length shell = 16 && List.length ours = 16);
          List.iter
            (fun (k, v) ->
              match List.assoc_opt k shell with
              | Some sv ->
                  check_eq (Printf.sprintf "target_db: %s: %s" name k) ~expected:sv ~actual:v
              | None -> check (Printf.sprintf "target_db: %s: %s present in shell" name k) false)
            ours)
    Target.all

(* {2 The three target SETS, against the enumerator} *)

let test_target_sets root =
  let matrix = Fpath.(root / "tools" / "target-matrix.sh") in
  List.iter
    (fun (arg, cap) ->
      match run_capture matrix [ arg ] with
      | None -> check (Printf.sprintf "target sets: %s ran" arg) false
      | Some out ->
          check_eq
            (Printf.sprintf "target sets: %s" arg)
            ~expected:(String.concat " " (lines out))
            ~actual:(String.concat " " (List.map Target.to_string (Target.set cap))))
    [ ("fixture", Target.Fixture); ("assembler", Target.Assembler); ("libc", Target.Libc_smoke) ]

(* {3 The derived invariants, stated against the shell rather than assumed} *)

let test_derived_invariants root =
  let dump = Fpath.(root / "tools" / "dev" / "dump-target-config.sh") in
  List.iter
    (fun t ->
      let name = Target.to_string t in
      match run_capture dump [ name ] with
      | None -> check (Printf.sprintf "derived: %s" name) false
      | Some out ->
          let shell = tsv out in
          let get k = Option.value ~default:"" (List.assoc_opt k shell) in
          (* qemu_sysroot = None IFF not HAS_SYSROOT - the equivalence that
             lets the OCaml side omit the flag entirely. *)
          check
            (Printf.sprintf "derived: %s: HAS_SYSROOT <-> QEMU_SYSROOT non-empty" name)
            (get "HAS_SYSROOT" = "true" = (get "QEMU_SYSROOT" <> ""));
          (* And the link family really is base, +0x10000, +0x20000. *)
          let addr k = int_of_string (get k) in
          check
            (Printf.sprintf "derived: %s: link addresses are one arithmetic family" name)
            (addr "LINK_RODATA_ADDR" = addr "LINK_TEXT_ADDR" + 0x10000
            && addr "LINK_DATA_ADDR" = addr "LINK_TEXT_ADDR" + 0x20000))
    Target.all

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: repo_tests.exe <repository-root>";
    exit 2);
  let root = Fpath.v Sys.argv.(1) in
  (* The root is validated by the same sentinels every command uses, so a
     mis-passed argument fails here rather than producing confusing findings. *)
  (match Repo.resolve ~cli:(Some root) ~env:(fun _ -> None) ~cwd:root with
  | Ok _ -> ()
  | Error e ->
      prerr_endline (Tool_error.to_fatal_line (Err.Error.kind e));
      exit 1);
  print_endline "repo_tests:";
  test_target_db_agrees root;
  test_target_sets root;
  test_derived_invariants root;
  if !failures > 0 then (
    Printf.printf "repo_tests: %d failures\n" !failures;
    exit 1)
  else print_endline "repo_tests: all checks passed"
