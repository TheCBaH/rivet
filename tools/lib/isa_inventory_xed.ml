let ( let* ) = Result.bind

let err ?path op detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail)

type entry = { mnemonic : string; extension : string; applies_32 : bool; applies_64 : bool }

let is_dir p = try Sys.is_directory (Fpath.to_string p) with Sys_error _ -> false

let list_extension_dirs repo =
  let dir = Repo.isa_data_xed repo in
  match Sys.readdir (Fpath.to_string dir) with
  | exception Sys_error msg -> err ~path:dir Tool_error.Read_file msg
  | names ->
      Array.to_list names
      |> List.filter (fun name -> is_dir Fpath.(dir / name))
      |> List.sort String.compare |> Result.ok

(* XED's own tables use runs of spaces for the field gutter ("ICLASS    :"
   and "ICLASS:" both occur), not tabs - the same tolerance
   Isa_inventory_riscv's tokens gives riscv-opcodes. *)
let tokens line =
  String.split_on_char ' ' line
  |> List.concat_map (String.split_on_char '\t')
  |> List.filter (fun s -> s <> "")

(* [line] is a [field] assignment iff it starts with [field] and the next
   character (if any) is whitespace or ':' - which is what rules out a
   longer identifier merely sharing the prefix. No XED field name is a
   prefix of another today, but the check is free. *)
let field_value line ~field =
  let flen = String.length field in
  if String.length line < flen || String.sub line 0 flen <> field then None
  else
    let next_ok =
      String.length line = flen || match line.[flen] with ' ' | '\t' | ':' -> true | _ -> false
    in
    if not next_ok then None
    else
      match String.index_opt line ':' with
      | None -> None
      | Some i -> Some (String.trim (String.sub line (i + 1) (String.length line - i - 1)))

(* One [{ ... }] block's accumulator. A block can carry several PATTERN
   lines (XED's hand-authored dialect) as well as just one (the generated
   dialect) - see the .mli and asm/docs/isa-inventory.md. *)
type block_state = {
  mutable iclass : string option;
  mutable saw_pattern : bool;
  mutable any_32_ok : bool;
  mutable any_64_ok : bool;
}

let fresh_block () = { iclass = None; saw_pattern = false; any_32_ok = false; any_64_ok = false }

let close_block ~extension (b : block_state) acc =
  match b.iclass with
  | None -> acc (* a block with no ICLASS field is not an instruction *)
  | Some name ->
      let applies_32 = if b.saw_pattern then b.any_32_ok else true in
      let applies_64 = if b.saw_pattern then b.any_64_ok else true in
      { mnemonic = String.lowercase_ascii name; extension; applies_32; applies_64 } :: acc

let parse_datafile ~extension text =
  let lines = String.split_on_char '\n' text in
  let rec go lines in_block block acc =
    match lines with
    | [] ->
        if in_block then
          err Tool_error.Parse (Printf.sprintf "%s: unterminated '{' at end of file" extension)
        else Ok (List.rev acc)
    | line :: rest ->
        let trimmed = String.trim line in
        if trimmed = "" || trimmed.[0] = '#' then go rest in_block block acc
        else if trimmed = "{" then
          if in_block then err Tool_error.Parse (Printf.sprintf "%s: nested '{'" extension)
          else go rest true (fresh_block ()) acc
        else if trimmed = "}" then
          if not in_block then err Tool_error.Parse (Printf.sprintf "%s: unmatched '}'" extension)
          else go rest false (fresh_block ()) (close_block ~extension block acc)
        else if not in_block then
          (* Text between blocks: section headers like "AVX_INSTRUCTIONS()::"
             and stray comments/tables that never form a standalone "{"/"}"
             line - see the .mli. Not a parse error. *)
          go rest in_block block acc
        else (
          (match field_value line ~field:"ICLASS" with
          | Some v -> if block.iclass = None then block.iclass <- Some v
          | None -> ());
          (match field_value line ~field:"PATTERN" with
          | Some v ->
              block.saw_pattern <- true;
              let toks = tokens v in
              (* MODE is a three-way field (mode16/mode32/mode64), not a
                 64-bit bit-flag: a PATTERN tagged mode16 or mode32 is just as
                 excluded from x86_64 as one tagged not64 (PUSHA/POPA/BOUND
                 have only mode16/mode32 forms and no not64 tag at all - see
                 asm/docs/isa-inventory.md). Don't confuse this MODE token
                 with the separate EAMODE one (eamode16/eamode32/eamode64,
                 address-size, not operating-mode). *)
              if not (List.mem "mode64" toks) then block.any_32_ok <- true;
              if not (List.mem "not64" toks || List.mem "mode16" toks || List.mem "mode32" toks)
              then block.any_64_ok <- true
          | None -> ());
          go rest in_block block acc)
  in
  go lines false (fresh_block ()) []

let sort_paths paths =
  List.sort (fun a b -> String.compare (Fpath.to_string a) (Fpath.to_string b)) paths

(* Some extensions nest their instruction tables a level (or two) below the
   extension directory itself - "amd" (this repo's own top-level `datafiles`
   entry, not XED's separate "amd" fork) has AMD's XOP/FMA4 forms under
   amd/amdxop/, and a couple of directories carry a "tests" subdirectory of
   golden-output fixtures rather than instruction data. Recursing rather than
   reading only [dir]'s immediate files is what {!parse_datafile}'s
   content-based recognition needs to actually reach amd/amdxop's ICLASS
   blocks; a fixtures-only subdirectory like avx512f/tests simply contributes
   nothing once scanned, the same way a non-instruction .txt file already
   does. *)
let rec list_txt_files_rec dir =
  match Sys.readdir (Fpath.to_string dir) with
  | exception Sys_error msg -> err ~path:dir Tool_error.Read_file msg
  | names ->
      Array.to_list names
      |> List.fold_left
           (fun acc name ->
             let* acc = acc in
             let p = Fpath.(dir / name) in
             if is_dir p then
               let* nested = list_txt_files_rec p in
               Ok (List.rev_append nested acc)
             else if Filename.check_suffix name ".txt" then Ok (p :: acc)
             else Ok acc)
           (Ok [])

let read_and_parse ~extension files =
  List.fold_left
    (fun acc f ->
      let* entries = acc in
      let* text = Tool_fs.read f in
      let* file_entries = parse_datafile ~extension text in
      Ok (List.rev_append file_entries entries))
    (Ok []) files

let entries_in_dir repo extension =
  let dir = Fpath.(Repo.isa_data_xed repo / extension) in
  let* files = list_txt_files_rec dir in
  read_and_parse ~extension (sort_paths files)

(* datafiles/ itself carries the base ISA (xed-isa.txt: MOV, PUSH, CPUID,
   ...) as loose files alongside the extension subdirectories - not inside
   any of them, so {!list_extension_dirs}/{!entries_in_dir} never reach it.
   Filed under the synthetic extension "base", the same label these root
   blocks' own EXTENSION field overwhelmingly uses. Only [dir]'s immediate
   files are read here; its subdirectories are each their own extension,
   already covered by {!entries_in_dir}. *)
let entries_at_root repo =
  let dir = Repo.isa_data_xed repo in
  match Sys.readdir (Fpath.to_string dir) with
  | exception Sys_error msg -> err ~path:dir Tool_error.Read_file msg
  | names ->
      let files =
        Array.to_list names
        |> List.filter (fun name ->
            (not (is_dir Fpath.(dir / name))) && Filename.check_suffix name ".txt")
        |> List.map (fun name -> Fpath.(dir / name))
        |> sort_paths
      in
      read_and_parse ~extension:"base" files

let compare_entry a b =
  match String.compare a.mnemonic b.mnemonic with
  | 0 -> String.compare a.extension b.extension
  | c -> c

(* Multiple blocks can share one (mnemonic, extension) - the same
   operand-form multiplicity Isa_inventory_riscv collapses - but here a
   later block can also have DIFFERENT mode applicability than an earlier
   one for the very same pair (see monitorx's not64-only forms alongside a
   plain BASE entry elsewhere). A plain sort_uniq would keep both as
   "different" records since their applies_* fields differ; this ORs them
   into one record instead; a mnemonic usable in a mode by ANY block counts
   as usable in that mode. *)
let merge_entries entries =
  let tbl = Hashtbl.create 8192 in
  List.iter
    (fun e ->
      let key = (e.mnemonic, e.extension) in
      match Hashtbl.find_opt tbl key with
      | None -> Hashtbl.replace tbl key (e.applies_32, e.applies_64)
      | Some (a32, a64) -> Hashtbl.replace tbl key (a32 || e.applies_32, a64 || e.applies_64))
    entries;
  Hashtbl.fold
    (fun (mnemonic, extension) (applies_32, applies_64) acc ->
      { mnemonic; extension; applies_32; applies_64 } :: acc)
    tbl []
  |> List.sort compare_entry

let entries_for_target repo (target : Target.t) =
  match target with
  | Target.Arm | Target.Aarch64 | Target.Riscv32 | Target.Riscv64 -> Ok []
  | Target.X86_32 | Target.X86_64 ->
      let* dirs = list_extension_dirs repo in
      let* root_entries = entries_at_root repo in
      let* all_entries =
        List.fold_left
          (fun acc extension ->
            let* entries = acc in
            let* dir_entries = entries_in_dir repo extension in
            Ok (List.rev_append dir_entries entries))
          (Ok root_entries) dirs
      in
      let merged = merge_entries all_entries in
      let applies (e : entry) =
        match target with
        | Target.X86_32 -> e.applies_32
        | Target.X86_64 -> e.applies_64
        | _ -> false
      in
      Ok (List.filter applies merged)

let render_manifest ~source_commit (target : Target.t) entries =
  let b = Buffer.create 65536 in
  Buffer.add_string b "schema-version:1\n";
  Buffer.add_string b (Printf.sprintf "target:%s\n" (Target.to_string target));
  Buffer.add_string b "source:xed\n";
  Buffer.add_string b (Printf.sprintf "source-commit:%s\n" source_commit);
  Buffer.add_string b "source-license:Apache-2.0\n";
  List.iter
    (fun e ->
      Buffer.add_string b
        (Printf.sprintf
           "mnemonic:%s\textension:%s\tcompcert-emitted:unknown\tgcc-emitted:unknown\tstate:unknown\tdeferral-reason:-\tpromotion-trigger:-\n"
           (Manifest.escape e.mnemonic) (Manifest.escape e.extension)))
    entries;
  Buffer.contents b

let render_summary (target : Target.t) entries =
  let b = Buffer.create 4096 in
  Buffer.add_string b (Printf.sprintf "target:%s\n" (Target.to_string target));
  Buffer.add_string b (Printf.sprintf "total:%d\n" (List.length entries));
  let by_extension = Hashtbl.create 128 in
  List.iter
    (fun e ->
      Hashtbl.replace by_extension e.extension
        (1 + Option.value (Hashtbl.find_opt by_extension e.extension) ~default:0))
    entries;
  Hashtbl.to_seq by_extension |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.iter (fun (ext, n) -> Buffer.add_string b (Printf.sprintf "by-extension:%s:%d\n" ext n));
  Buffer.add_string b (Printf.sprintf "by-state:unknown:%d\n" (List.length entries));
  Buffer.add_string b "compcert-emitted:0\n";
  Buffer.add_string b "gcc-emitted:0\n";
  Buffer.contents b
