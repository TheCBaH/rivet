(* Every sha256: record in every committed manifest, recomputed.

   This is the test that would catch a wrong digest backend, a text-mode read,
   or a traversal that followed a symlink: 800-odd real files, compared against
   values the shell produced with sha256sum. *)

open Compcert_tools

let failures = ref 0
let checked = ref 0

let fail fmt =
  Printf.ksprintf
    (fun s ->
      incr failures;
      print_endline ("  FAIL " ^ s))
    fmt

(* The manifest key side has no escape layer, so a sha256: record is exactly
   "sha256:<path>\t<hex>" and splitting on the first tab is correct. *)
let record line =
  match String.index_opt line '\t' with
  | None -> None
  | Some i ->
      let key = String.sub line 0 i in
      let value = String.sub line (i + 1) (String.length line - i - 1) in
      if String.length key > 7 && String.sub key 0 7 = "sha256:" then
        Some (String.sub key 7 (String.length key - 7), value)
      else None

let check_manifest manifest =
  let root = Fpath.parent manifest in
  match Tool_fs.read manifest with
  | Error _ -> fail "cannot read %s" (Fpath.to_string manifest)
  | Ok text ->
      let lines = String.split_on_char '\n' text in
      List.iter
        (fun line ->
          match record line with
          | None -> ()
          | Some (rel, expected) -> (
              incr checked;
              let p = Fpath.(root // v rel) in
              match Tool_fs.sha256 p with
              | Error _ -> fail "cannot hash %s" (Fpath.to_string p)
              | Ok got ->
                  if got <> expected then
                    fail "%s: manifest says %s, computed %s" (Fpath.to_string p) expected got))
        lines

let rec manifests dir acc =
  Array.fold_left
    (fun acc entry ->
      let p = Filename.concat dir entry in
      match Unix.lstat p with
      | { Unix.st_kind = Unix.S_DIR; _ } -> manifests p acc
      | { Unix.st_kind = Unix.S_REG; _ } when entry = "manifest.txt" -> Fpath.v p :: acc
      | _ -> acc)
    acc (Sys.readdir dir)

let () =
  let fixtures = Sys.argv.(1) in
  print_endline "corpus_hashes:";
  let ms = List.sort compare (manifests fixtures []) in
  (* An empty discovery would make this test pass while proving nothing, which
     is the same failure mode the fixture scripts guard against. *)
  if ms = [] then (
    print_endline "  FAIL no manifests found";
    exit 1);
  List.iter check_manifest ms;
  Printf.printf "  %d manifests, %d sha256 records\n" (List.length ms) !checked;
  if !failures > 0 then exit 1 else print_endline "corpus_hashes: all checks passed"
