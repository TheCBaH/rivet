let fatal ?path op detail =
  Command.of_error
    (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail))

(* asm/docs/isa-inventory.md's manifest.txt: one [key:value<TAB>...] row per
   line, plus a five-line [key:value] header (schema-version, target, source,
   source-commit, source-license) this check does not need. Every data row
   starts with "mnemonic:" - matching isa-db/tests/manifest_fixture.py's own
   read_manifest_pairs, which the same rows are already regression-tested
   against. No row in any of the four current fixtures uses a backslash
   escape (verified when isa-db/tests/manifest_fixture.py was written), so
   this does not implement unescaping either. *)
let manifest_pairs text =
  String.split_on_char '\n' text
  |> List.filter_map (fun line ->
      if not (String.length line > 9 && String.sub line 0 9 = "mnemonic:") then None
      else
        let field key line =
          let parts = String.split_on_char '\t' line in
          List.find_map
            (fun part ->
              match String.index_opt part ':' with
              | Some i when String.sub part 0 i = key ->
                  Some (String.sub part (i + 1) (String.length part - i - 1))
              | _ -> None)
            parts
        in
        match (field "mnemonic" line, field "extension" line) with
        | Some m, Some e -> Some (m, e)
        | _ -> None)
  |> List.sort_uniq compare

module Pair_set = Set.Make (struct
  type t = string * string

  let compare = compare
end)

(* Which isa-db record fields key a pair, per source: riscv-opcodes carries
   its extension in provenance.extension and keeps native_name's own case
   (manifest mnemonics are the file's literal first token); XED lowercases
   ICLASS for its manifest mnemonic (Isa_inventory_xed.entry.mnemonic) but the
   isa-db XED adapter does not lowercase native_name (see
   isa-db/adapters/xed/reader.py), and carries its group in
   provenance.group, not provenance.extension. A record whose relevant
   provenance field is absent contributes no pair - by construction every
   isa-db record from either adapter always sets it, so this only guards
   against a malformed export, not an expected case. *)
type source_spec = {
  name : string;
  lowercase_native_name : bool;
  provenance_field : Isa_db_jsonl.provenance -> string option;
}

let riscv_opcodes_spec =
  {
    name = "riscv_opcodes";
    lowercase_native_name = false;
    provenance_field = (fun p -> p.Isa_db_jsonl.extension);
  }

let xed_spec =
  { name = "xed"; lowercase_native_name = true; provenance_field = (fun p -> p.Isa_db_jsonl.group) }

let spec_for_target (target : Target.t) =
  match target with
  | Target.Riscv32 | Target.Riscv64 -> Some riscv_opcodes_spec
  | Target.X86_32 | Target.X86_64 -> Some xed_spec
  | Target.Arm | Target.Aarch64 -> None

let isa_db_pairs spec (records : Isa_db_jsonl.record list) =
  records
  |> List.filter_map (fun (r : Isa_db_jsonl.record) ->
      if r.kind = "import" then None
      else
        match spec.provenance_field r.provenance with
        | None -> None
        | Some key ->
            let name =
              if spec.lowercase_native_name then String.lowercase_ascii r.native_name
              else r.native_name
            in
            Some (name, key))
  |> Pair_set.of_list

let check_target repo (target : Target.t) =
  match spec_for_target target with
  | None -> Command.ok []
  | Some spec -> (
      let manifest_path = Fpath.(Repo.isa_inventory repo target / "manifest.txt") in
      let jsonl_path = Repo.isa_db_export repo ~source:spec.name target in
      match Tool_fs.read manifest_path with
      | Error e -> Command.of_error e
      | Ok text -> (
          let wanted = manifest_pairs text in
          if wanted = [] then
            fatal ~path:manifest_path Tool_error.Validate "manifest records no mnemonics"
          else
            match Isa_db_jsonl.read_file jsonl_path with
            | Error e -> Command.of_error e
            | Ok records ->
                let have = isa_db_pairs spec records in
                let missing = List.filter (fun p -> not (Pair_set.mem p have)) wanted in
                if missing = [] then
                  Command.ok
                    [
                      Diagnostic.stdout
                        (Printf.sprintf "isa-db-cross-validate: %s: %d manifest rows all matched"
                           (Target.to_string target) (List.length wanted));
                    ]
                else
                  Command.fail
                    (Diagnostic.stderr
                       (Printf.sprintf
                          "isa-db-cross-validate: %s: %d of %d manifest rows have no isa-db match"
                          (Target.to_string target) (List.length missing) (List.length wanted))
                    :: List.map
                         (fun (m, e) ->
                           Diagnostic.stderr
                             (Printf.sprintf "  missing: mnemonic:%s extension:%s" m e))
                         missing)))

let check repo = Command.accumulate Target.all ~f:(check_target repo)
