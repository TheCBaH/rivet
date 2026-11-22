(* The differential gate (docs/design.md M1.5, M2's O1-O3), as a library over a corpus.

   Every case of a compiler corpus, on every target, compared against the
   committed GNU oracle three ways:

   - **bytes**, per allocatable section, bound at the checked-in per-case
     addresses and compared against the controlled reference link. Exact and
     unnormalized: it is the claim that this assembler and GNU produce the same
     machine code, and it is the final truth when the other two disagree.
   - **spelling**, from the *diagnostic* dump, against [objdump] after a
     declared set of normalizations. Weaker by construction - what it can
     establish is that the two disagree about nothing except spellings this file
     enumerates - so every rule below is a pure syntax rewrite listed with the
     difference it absorbs. It reads the diagnostic dump rather than the
     canonical one because §1.6 assigns objdump agreement there; the canonical
     dump is pinned to reassemble byte-exactly and carries form-forcing
     spellings that objdump has no reason to print.
   - **reassembly**, from the canonical dump, back to identical bytes.

   Cases are discovered from the corpus rather than listed, so adding a fixture
   directory adds a row. A case that this assembler cannot yet assemble
   prints the diagnostic that blocks it instead of being omitted: the transcript
   is the progress record, and a gate that silently skipped what it could not
   do would report the same success at every stage.

   The corpus is a parameter ({!CORPUS}): this repository's gcc fixtures are one
   instance (test/differential), and another project with its own compiler's
   fixtures in the same layout is another. The checks print the transcript; the
   instance's expect tests pin it. *)

let read path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let hex_of s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

(* [text.hex] is a whitespace-separated byte list in memory order, wrapped at
   sixteen bytes a line. Reading it back as a byte list rather than comparing
   the rendered text means a difference in line wrapping is not reported as a
   difference in code. *)
let bytes_of_hex_file path =
  let text = read path in
  let buf = Buffer.create 64 in
  String.split_on_char '\n' text
  |> List.iter (fun line ->
      String.split_on_char ' ' line
      |> List.iter (fun tok ->
          let tok = String.trim tok in
          if tok <> "" then Buffer.add_char buf (Char.chr (int_of_string ("0x" ^ tok)))));
  Buffer.contents buf

(* {1 The declared normalizations}

   Each entry says what GNU prints, what this assembler prints, and why they
   differ. Nothing here may change which register, which immediate or which
   operation a line names. *)

let strip_comment target line =
  (* objdump appends the decimal or hexadecimal value of an immediate after the
     target's comment introducer: [mov r0, #42 @ 0x2a] on ARM and
     [mov w0, #0x2a // #42] on AArch64. It is a second rendering of an operand
     that is already on the line, so removing it removes no information. *)
  let intro = match target with "arm" -> "@" | "aarch64" -> "//" | _ -> "#" in
  match
    let rec find i =
      if i + String.length intro > String.length line then None
      else if String.sub line i (String.length intro) = intro then Some i
      else find (i + 1)
    in
    find 0
  with
  | Some i -> String.sub line 0 i
  | None -> line

let collapse_space s =
  let b = Buffer.create (String.length s) in
  let prev_space = ref true in
  String.iter
    (fun c ->
      let c = if c = '\t' then ' ' else c in
      if c = ' ' then (
        if not !prev_space then Buffer.add_char b c;
        prev_space := true)
      else (
        Buffer.add_char b c;
        prev_space := false))
    s;
  String.trim (Buffer.contents b)

(* objdump writes numbers in hexadecimal - [#0x10] on AArch64, [$0x2a] and
   [0x10(%esp)] on x86 - and this assembler writes decimal everywhere. Rewriting
   every [0xN] literal to its decimal value is a base change, not a value
   change, and it is applied to both sides so a decimal literal on either is
   left alone. *)
let decimalize s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') in
  let i = ref 0 in
  while !i < n do
    if !i + 2 < n && s.[!i] = '0' && (s.[!i + 1] = 'x' || s.[!i + 1] = 'X') && is_hex s.[!i + 2]
    then (
      let j = ref (!i + 2) in
      while !j < n && is_hex s.[!j] do
        incr j
      done;
      let digits = String.sub s (!i + 2) (!j - !i - 2) in
      Buffer.add_string b (Int64.to_string (Int64.of_string ("0x" ^ digits)));
      i := !j)
    else (
      Buffer.add_char b s.[!i];
      incr i)
  done;
  Buffer.contents b

(* objdump separates operands with a bare comma and this assembler with a comma
   and a space. Whitespace only. *)
let tighten_commas s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    Buffer.add_char b s.[!i];
    if s.[!i] = ',' then
      while !i + 1 < n && s.[!i + 1] = ' ' do
        incr i
      done;
    incr i
  done;
  Buffer.contents b

(* AT&T size suffixes. objdump drops the [b]/[w]/[l]/[q] whenever a register
   operand already fixes the operand size, which is every x86 line in these
   fixtures; this assembler always prints it, because a canonical dump must be
   re-parseable and GAS needs the suffix on forms where no register appears.

   The rewrite therefore fires only when the line *has* a register operand -
   that is precisely objdump's own condition - so it cannot erase a suffix that
   carries the only size information on the line. *)
let drop_size_suffix target line =
  if target <> "x86_32" && target <> "x86_64" then line
  else if not (String.contains line '%') then line
  else
    match String.index_opt line ' ' with
    | None -> line
    | Some sp ->
        let m = String.sub line 0 sp in
        let n = String.length m in
        let stem = if n >= 2 then String.sub m 0 (n - 1) else m in
        (* Only for mnemonics measured to lose their suffix in the committed
           objdump output, and only when the *stem* is one of them. A blind
           "drop the last letter" would turn objdump's already-suffixless [sub]
           into [su]. *)
        if
          List.mem stem [ "add"; "sub"; "mov"; "lea"; "imul"; "cmp"; "xor" ]
          && match m.[n - 1] with 'b' | 'w' | 'l' | 'q' -> true | _ -> false
        then stem ^ String.sub line sp (String.length line - sp)
        else line

(* AArch64's [movz wD, #imm] with a zero shift is spelled [mov wD, #imm] by
   objdump, which is the MOV (wide immediate) alias. This assembler prints the
   underlying form, because resolving the alias in the *other* direction would
   require a surface [mov] with an immediate that M1 does not accept. The
   rewrite maps objdump's spelling onto this one and is the only rule here that
   touches a mnemonic; it is safe because it fires only for a [mov] whose second
   operand is an immediate, which on A64 has exactly one encoding. *)
let movz_alias target line =
  if target <> "aarch64" then line
  else
    match String.split_on_char ' ' line with
    | "mov" :: rest when String.contains line '#' -> String.concat " " ("movz" :: rest)
    | _ -> line

(* objdump annotates a branch or call target with the symbol it lands in and the
   offset from it: [jl 32 <asm_test_entry+50>]. The address itself is already on
   the line, and this assembler has no symbol table at disassembly time to write
   the second rendering with. Dropping the parenthetical removes a restatement,
   not a fact. *)
let drop_symbol_annotation line =
  match String.index_opt line '<' with
  | Some i when String.length line > 0 && line.[String.length line - 1] = '>' ->
      String.trim (String.sub line 0 i)
  | _ -> line

(* B10's form-forcing suffix. A canonical dump writes [jl.d8] because it must
   reassemble to the same bytes, and objdump writes [jl] because it has no such
   obligation - so the difference is between two contracts rather than between
   two readings of the machine, and this is where it is absorbed. It is removed
   only from *our* side, and only from a mnemonic that is a branch, so it cannot
   quietly erase a suffix that carried operand-size information. *)
let drop_branch_form_suffix target line =
  if target <> "x86_32" && target <> "x86_64" then line
  else
    match String.index_opt line ' ' with
    | None -> line
    | Some sp ->
        let m = String.sub line 0 sp in
        let rest = String.sub line sp (String.length line - sp) in
        let strip suffix =
          let n = String.length m and k = String.length suffix in
          if n > k && String.sub m (n - k) k = suffix then Some (String.sub m 0 (n - k)) else None
        in
        (match strip ".d8" with
          | Some s -> s
          | None -> ( match strip ".d32" with Some s -> s | None -> m))
        ^ rest

(* The one number objdump writes whose base is not marked: a branch or call
   target is a bare hexadecimal address, while every other number it prints is
   either 0x-prefixed or decimal. This assembler writes decimal everywhere, so
   the rule is asymmetric - it applies to objdump's side alone - and [~side] is
   how that is said out loud rather than hidden in a heuristic. It fires only on
   a branch or call mnemonic with a single all-hex-digit operand.

   The mnemonic set is per-dialect and listed rather than pattern-matched,
   because on A32 the guess "starts with b" also catches [bx lr] and [bic]. Only
   [b] and [bl] plus a condition suffix take a target; [bx] takes a register,
   and its operand [lr] is not all hex digits anyway - but relying on that would
   be relying on a spelling accident. *)
let arm_conditions =
  [ "eq"; "ne"; "cs"; "cc"; "mi"; "pl"; "vs"; "vc"; "hi"; "ls"; "ge"; "lt"; "gt"; "le"; "al" ]

let is_branch_mnemonic target m =
  match target with
  | "arm" ->
      let stems = "b" :: "bl" :: List.concat_map (fun c -> [ "b" ^ c; "bl" ^ c ]) arm_conditions in
      List.mem m stems
  | "aarch64" ->
      (* A64 spells the condition after a dot, so there is no [bic]-shaped
         collision - but [br] and [blr] take registers, so the list is still a
         list. *)
      List.mem m ("b" :: "bl" :: List.map (fun c -> "b." ^ c) arm_conditions)
  | "riscv32" | "riscv64" -> List.mem m [ "beq"; "bne"; "blt"; "bge"; "bltu"; "bgeu"; "jal" ]
  | _ -> (String.length m > 0 && m.[0] = 'j') || String.equal m "call"

let branch_target_is_hex target side line =
  if side <> `Objdump then line
  else if target = "riscv32" || target = "riscv64" then
    match String.index_opt line ' ' with
    | None -> line
    | Some space ->
        let m = String.sub line 0 space in
        if not (is_branch_mnemonic target m) then line
        else
          let comma = String.rindex_opt line ',' in
          let start = match comma with Some i -> i + 1 | None -> space + 1 in
          let operand = String.sub line start (String.length line - start) in
          if
            operand <> ""
            && String.for_all (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) operand
          then String.sub line 0 start ^ Int64.to_string (Int64.of_string ("0x" ^ operand))
          else line
  else
    match String.split_on_char ' ' line with
    | [ m; operand ] when is_branch_mnemonic target m ->
        if
          operand <> ""
          && String.for_all (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) operand
        then m ^ " " ^ Int64.to_string (Int64.of_string ("0x" ^ operand))
        else line
    | _ -> line

let normalize ?(side = `Ours) target line =
  (* [collapse_space] runs before [drop_symbol_annotation], not after: stripping
     objdump's trailing [// b.tcont] comment leaves whitespace behind, and the
     annotation rule anchors on the line ending in [>]. *)
  line |> strip_comment target |> collapse_space |> drop_symbol_annotation |> decimalize
  |> tighten_commas |> drop_size_suffix target |> drop_branch_form_suffix target
  |> movz_alias target |> branch_target_is_hex target side

(* objdump's body lines look like [   4:\te24dd008 \tsub\tsp, sp, #8]. The
   encoding word is dropped, because the byte comparison already covers it
   exactly and covers it better; the address is kept, because it is how
   alignment padding is excluded from both sides without a filter that reads
   spellings. *)
let objdump_instructions target path =
  read path |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      match String.index_opt line ':' with
      | None -> None
      | Some i ->
          let before = String.sub line 0 i in
          if String.trim before = "" then None
          else if
            String.for_all
              (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || c = ' ')
              before
          then
            let after = String.sub line (i + 1) (String.length line - i - 1) in
            (* the encoding word is the first tab-separated field after the colon *)
            match String.split_on_char '\t' after with
            | _ :: _word :: rest when rest <> [] ->
                Some
                  ( int_of_string ("0x" ^ String.trim before),
                    normalize ~side:`Objdump target (String.concat " " rest) )
            | _ -> None
          else None)

(* {1 The corpus} *)

module type CORPUS = sig
  val root : string
  (** the fixture directory, one subdirectory per case and per target below it *)

  val targets : string list
  (** the targets every case is checked on, unless its manifest restricts them *)

  val is_case : string -> bool
  (** whether a directory under [root], given as its path, is a case *)
end

type record = string * int * string * string * int64 option

module Make (C : CORPUS) = struct
  let corpus_root = C.root
  let targets = C.targets

  let cases () =
    Sys.readdir corpus_root |> Array.to_list
    |> List.filter (fun c -> C.is_case (Filename.concat corpus_root c))
    |> List.sort compare

  let case_dir case target = Filename.concat (Filename.concat corpus_root case) target

  (* M4 (docs/design.md §12): a trivial, read-only, single-field scanner - not
   the fuller [Fixture_meta] reader [test/oracle/exec.ml] has, since the
   differential gate has no ABI/observation concept at all, only "does this
   case's committed manifest restrict which targets it has directories for."
   [tools]'s own [Manifest]/[Corpus] remain the sole writer/rehasher of
   this record; this is a second reader of the same manifest.txt format, not
   a second grammar authority. Absent manifest, or no such record, means
   every one of [targets] - exactly today's behavior for every
   pre-M4 case. *)
  let supported_targets case =
    let path = Filename.concat (Filename.concat corpus_root case) "manifest.txt" in
    if not (Sys.file_exists path) then targets
    else
      let text = read path in
      let prefix = "supported-targets\t" in
      let plen = String.length prefix in
      let matching =
        String.split_on_char '\n' text
        |> List.find_opt (fun line -> String.length line > plen && String.sub line 0 plen = prefix)
      in
      match matching with
      | None -> targets
      | Some line -> String.split_on_char ' ' (String.sub line plen (String.length line - plen))

  let targets_for case = List.filter (fun t -> List.mem t (supported_targets case)) targets

  (* the compiler writes its source path into the assembly banner, so the stem is the
   case's own and return42 keeps the [asm_test_entry] spelling its committed
   bytes were generated with. Every [.s] under a target directory is one of the
   case's own compilation units - one for the single-source cases M1/M2 added,
   more than one for M3's - paired with its own stem, sorted so build order is
   deterministic. This is also the unit name a multi-source case's own oracle
   evidence is filed under (tools/lib/oracle_cmd.ml's [unit_dir]), since both
   sides derive it the same way, from the same file. *)
  let unit_paths case target =
    let dir = case_dir case target in
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".s")
    |> List.sort compare
    |> List.map (fun f -> (Filename.chop_suffix f ".s", Filename.concat dir f))

  (* [oracle/linked/manifest.txt] is the checked-in address policy: one line per
   allocatable section giving the address the reference link used, its kind,
   its logical size and - for a PROGBITS section - the file holding its
   post-link bytes. Our image is bound at exactly these addresses, which is
   what makes a post-link comparison meaningful for a section that carries
   relocations.

   Manifest v2 (M3 §11, docs/design.md §12): five columns, not three - a
   NOBITS section (.bss) has no byte artifact at all, so [ls_file] is [None]
   rather than a path to a file that was never written, and [ls_size] is its
   only evidence, since {!bytes_of_hex_file} has nothing to read for it. *)
  type section_kind = Progbits | Nobits

  type linked_section = {
    ls_name : string;
    ls_kind : section_kind;
    ls_addr : int64;
    ls_size : int64;
    ls_file : string option;
  }

  let linked_sections case target =
    let path = Filename.concat (case_dir case target) "oracle/linked/manifest.txt" in
    read path |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
        match String.split_on_char '\t' (String.trim line) with
        | [ sec; kind; addr; size; file ] when sec <> "" ->
            Some
              {
                ls_name = sec;
                ls_kind = (if String.equal kind "nobits" then Nobits else Progbits);
                ls_addr = Int64.of_string addr;
                ls_size = Int64.of_string size;
                ls_file = (if String.equal file "-" then None else Some file);
              }
        | _ -> None)

  (* [oracle/reloc.txt] is the measured record set: one normalized line per ELF
   relocation GNU actually emitted, section, offset, REL-or-RELA, type, symbol
   and addend. It is the authority on what a linker is expected to see - nothing
   in this file hardcodes a guess about GNU's choice - and a REL target writes
   [-] for the addend because there is no addend field to read. *)
  type reloc_record = {
    ro_section : string;
    ro_offset : int;
    ro_rela : bool;
    ro_type : string;
    ro_symbol : string;
    ro_addend : int64 option;
  }

  let reloc_records_at path =
    if not (Sys.file_exists path) then []
    else
      read path |> String.split_on_char '\n'
      |> List.filter_map (fun line ->
          match String.split_on_char '\t' (String.trim line) with
          | [ sec; off; kind; ty; sym; addend ] when sec <> "" ->
              Some
                {
                  ro_section = sec;
                  ro_offset = int_of_string off;
                  ro_rela = String.equal kind "rela";
                  ro_type = ty;
                  ro_symbol = sym;
                  ro_addend =
                    (if String.equal addend "-" then None else Some (Int64.of_string addend));
                }
          | _ -> None)

  let target_of_name name =
    match Driver.Registry.find name with Some d -> d | None -> failwith ("no such target: " ^ name)

  (* X1's convention: every case's entry is a zero-argument [asm_test_entry].
   Naming it explicitly rather than inferring it is the point - three of the M2
   cases declare a callee or a data object beside the entry, and cardinality
   then names nothing. *)
  let entry_symbol = "asm_test_entry"

  (* One line, not a rendered diagnostic with its source excerpt. A blocked case
   is a row in a progress table; the full text is one [dune exec] away, and
   pasting it here would bury the twenty rows that do pass. *)
  let brief e =
    match Rivet_foundation.Diag.diagnostics e with
    | [] -> "failed with no diagnostic"
    | d :: rest ->
        Printf.sprintf "%s: %s%s"
          (Rivet_foundation.Diagnostic.code d)
          (Rivet_foundation.Diagnostic.message d)
          (if rest = [] then "" else Printf.sprintf " (+%d more)" (List.length rest))

  type built = {
    laid_out : Rivet_image.laid_out;
    bound : Rivet_image.t;
    addresses : (string * int64) list;
  }

  (* One unit reads exactly like every M1/M2 case always has: [D.assemble] with
   the frozen entry name doubling as its unit name. More than one goes through
   M3's [D.assemble_many] instead, each unit named after its own stem - the
   same name its own oracle evidence is filed under. *)
  let build case target =
    let (module D : Rivet_target_intf.Target.DRIVER) = target_of_name target in
    let units = unit_paths case target in
    let sourced =
      List.map
        (fun (stem, path) -> (stem, Rivet_foundation.Span.source ~name:path ~contents:(read path)))
        units
    in
    let result =
      match sourced with
      | [ (_, source) ] -> D.assemble ~entry:entry_symbol ~unit_name:entry_symbol ~source ()
      | many -> D.assemble_many ~entry:entry_symbol many ()
    in
    match result with
    | Error ds -> Error ("ASSEMBLE " ^ brief ds)
    | Ok laid_out -> (
        let addresses =
          List.map
            (fun (ls : linked_section) -> (ls.ls_name, ls.ls_addr))
            (linked_sections case target)
        in
        match Rivet_image.bind_image laid_out ~addresses with
        | Error ds -> Error ("BIND " ^ brief ds)
        | Ok bound -> Ok { laid_out; bound; addresses })

  (* {1 Per-unit oracle evidence, in merged coordinates}

   [oracle/<unit>/objdump.txt] and [oracle/<unit>/reloc.txt] (a multi-source
   case) are one unlinked object's own disassembly/relocations at ITS OWN,
   object-relative offsets - not the merged, padded coordinates our single
   diagnostic dump and single fixup-observation set describe. The base a
   unit's contribution was placed at within the merged section is not a
   guess: it is this assembler's own layout decision, already merge-padding-
   aware (image.ml's per-contribution [bases], §3/§5), recorded per label as
   [Rivet_image.Symtab.definition.d_offset] - the same number GNU's own linker
   used to place the identical bytes there, which [check_bytes] already
   establishes byte-for-byte. A unit's base is the lowest [d_offset] among
   its own definitions in that section: the compiler always opens a unit's
   contribution with the label the definition names (no unlabeled lead-in),
   the same reason GNU's own per-unit [readelf.txt] shows that symbol at
   [Value] 0. *)
  let unit_base_offset (b : built) ~section ~unit =
    List.filter_map
      (fun (d : Rivet_image.Symtab.definition) ->
        if
          String.equal d.Rivet_image.Symtab.d_section section
          && String.equal d.Rivet_image.Symtab.d_unit unit
        then Some d.Rivet_image.Symtab.d_offset
        else None)
      (Rivet_image.definitions b.laid_out)
    |> function
    | [] -> None
    | o :: os -> Some (List.fold_left min o os)

  (* The measured record set, generalized to a multi-source case: every unit
   that never appears in [reloc.txt] contributes nothing there, so this is
   the union of each unit's own pre-link records, translated into the one
   merged section's coordinates via {!unit_base_offset}. A single-source
   case is the base case where this is the identity - one unit, base 0 -
   which is why its behavior is unchanged. A record whose unit has no
   definition in its own section (should not occur: a relocation is always
   sited inside bytes some label in that unit anchors) is dropped rather
   than mis-based, since a wrong base would be a silent false mismatch. *)
  let reloc_records case target (b : built) =
    match unit_paths case target with
    | [ _ ] -> reloc_records_at (Filename.concat (case_dir case target) "oracle/reloc.txt")
    | units ->
        List.concat_map
          (fun (unit, _) ->
            reloc_records_at
              (Filename.concat (case_dir case target) ("oracle/" ^ unit ^ "/reloc.txt"))
            |> List.filter_map (fun r ->
                match unit_base_offset b ~section:r.ro_section ~unit with
                | None -> None
                | Some base -> Some { r with ro_offset = r.ro_offset + base }))
          units

  (* The disassembled instruction set, generalized the same way: a unit that
   contributes no bytes to [.text] (a data-only unit, e.g. [cross_data]'s
   [data] or [cross_bss]'s [data]) has no base to find and is skipped, since
   its own [oracle/<unit>/objdump.txt] is empty for exactly that reason.
   Sorted by offset after translation - the source of the offsets, not
   [unit_paths]'s alphabetical order, is what has to agree with our own
   merged dump's row order. *)
  let disasm_records case target (b : built) =
    match unit_paths case target with
    | [ _ ] ->
        objdump_instructions target (Filename.concat (case_dir case target) "oracle/objdump.txt")
    | units ->
        List.concat_map
          (fun (unit, _) ->
            match unit_base_offset b ~section:".text" ~unit with
            | None -> []
            | Some base ->
                objdump_instructions target
                  (Filename.concat (case_dir case target) ("oracle/" ^ unit ^ "/objdump.txt"))
                |> List.map (fun (off, text) -> (off + base, text)))
          units
        |> List.sort (fun (a, _) (c, _) -> compare a c)

  let find_segment b name =
    List.find_opt
      (fun (s : Rivet_image.segment) -> String.equal s.Rivet_image.name name)
      b.Rivet_image.segments

  let segment_bytes b name =
    Option.map (fun (s : Rivet_image.segment) -> s.Rivet_image.bytes) (find_segment b name)

  (* {1 Bytes, per section, after the controlled link} *)

  (* A NOBITS section (.bss) has no bytes to compare - the manifest's only
   evidence for it is its logical size, which must equal our own segment's
   address-space extent (§6's standing invariant, init_size + zero_fill) with
   no initialized bytes at all: a NOBITS section that emitted real bytes would
   be exactly the "initialized data inside .bss" case the assembler rejects at
   simplify time, so seeing one here would mean this comparison, not the
   assembler, missed it. *)
  let check_nobits sec b (ls : linked_section) =
    match find_segment b sec with
    | None -> Printf.sprintf "%s MISSING from our image" sec
    | Some (s : Rivet_image.segment) ->
        let extent = Int64.of_int (String.length s.Rivet_image.bytes + s.Rivet_image.zero_fill) in
        if String.length s.Rivet_image.bytes = 0 && Int64.equal extent ls.ls_size then
          Printf.sprintf "%s %Ld bytes (nobits)" sec ls.ls_size
        else
          Printf.sprintf
            "%s NOBITS DIFFERS\n    ours:   %d init + %d zero_fill\n    oracle: %Ld bytes" sec
            (String.length s.Rivet_image.bytes)
            s.Rivet_image.zero_fill ls.ls_size

  let check_bytes case target =
    match build case target with
    | Error e -> Printf.printf "%-12s %-8s %s\n" case target e
    | Ok b ->
        let results =
          List.map
            (fun (ls : linked_section) ->
              let sec = ls.ls_name in
              match (ls.ls_kind, ls.ls_file) with
              | Nobits, _ -> (sec, check_nobits sec b.bound ls)
              | Progbits, None ->
                  ( sec,
                    Printf.sprintf "%s: progbits record with no artifact (malformed manifest)" sec
                  )
              | Progbits, Some file -> (
                  let theirs =
                    bytes_of_hex_file (Filename.concat (case_dir case target) ("oracle/" ^ file))
                  in
                  match segment_bytes b.bound sec with
                  | None -> (sec, Printf.sprintf "%s MISSING from our image" sec)
                  | Some ours when String.equal ours theirs ->
                      (sec, Printf.sprintf "%s %d" sec (String.length ours))
                  | Some ours ->
                      ( sec,
                        Printf.sprintf "%s DIFFERS\n    ours:   %s\n    oracle: %s" sec
                          (hex_of ours) (hex_of theirs) )))
            (linked_sections case target)
        in
        Printf.printf "%-12s %-8s %s\n" case target (String.concat ", " (List.map snd results))

  (* {1 Spelling, against objdump}

   The diagnostic dump's third column, which §1.6 makes the one that answers to
   objdump. Padding lines are dropped from both sides: objdump decodes filler as
   instructions and this assembler prints the directive that produced it, and
   the bytes of that filler are already compared exactly above - a text
   comparison of nop spellings would add nothing and would have to invent a
   normalization that could hide a real difference. *)

  (* The diagnostic dump's columns are separated by runs of two or more spaces,
   which is what lets the spelling column contain single spaces (and brackets:
   [str ip, [sp]] would defeat a split at the first '['). *)
  let columns line =
    let n = String.length line in
    let out = ref [] and buf = Buffer.create 32 and i = ref 0 in
    let flush () =
      if Buffer.length buf > 0 then out := Buffer.contents buf :: !out;
      Buffer.clear buf
    in
    while !i < n do
      if line.[!i] = ' ' && !i + 1 < n && line.[!i + 1] = ' ' then (
        flush ();
        while !i < n && line.[!i] = ' ' do
          incr i
        done)
      else (
        Buffer.add_char buf line.[!i];
        incr i)
    done;
    flush ();
    List.rev !out

  type row = { off : int; len : int; text : string; padding : bool }

  let rows_of_dump ~base text =
    String.split_on_char '\n' text
    |> List.filter_map (fun line ->
        match columns line with
        | [ addr; bytes; text; form ] ->
            Some
              {
                off = Int64.to_int (Int64.sub (Int64.of_string ("0x" ^ addr)) base);
                len = List.length (String.split_on_char ' ' (String.trim bytes));
                text;
                padding = String.equal form "[padding]";
              }
        | _ -> None)

  (* [objdump.txt] and [reloc.txt] are recorded PER UNIT for a multi-source case
   (tools/lib/oracle_cmd.ml), one unlinked object's own disassembly/
   relocations at ITS OWN offsets - not the merged, padded coordinates our
   single diagnostic dump and single fixup-observation set describe.
   {!reloc_records} and {!disasm_records} do the attribution: a unit's own
   base offset within the merged section comes from this assembler's own
   layout ({!unit_base_offset}), not a guess, so [check_disasm] and
   [check_relocs] below need no multi-source branch of their own - a
   single-source case is simply the one-unit, base-0 case those two
   functions already handle as their identity. [check_bytes] - the
   strongest of the three checks by this file's own header - covers every
   case, single- or multi-source, independently of this attribution. *)
  let is_multi_source case target = List.length (unit_paths case target) > 1

  let check_disasm case target =
    let (module D : Rivet_target_intf.Target.DRIVER) = target_of_name target in
    match build case target with
    | Error _ -> Printf.printf "%-12s %-8s (does not assemble)\n" case target
    | Ok b -> (
        let bytes = Option.value ~default:"" (segment_bytes b.bound ".text") in
        (* Dumped at zero, because that is where objdump read the object. Our
         bytes are the bound ones either way - an intra-section PC-relative
         displacement is base-independent, which is the same fact that lets
         relaxation run at plan time - so the only fields that can disagree are
         the relocated ones, and those are handled below. *)
        match D.dump_disasm_diagnostic ~address:0L bytes with
        | Error ds -> Printf.printf "%-12s %-8s DISASM %s\n" case target (brief ds)
        | Ok text ->
            let rows = rows_of_dump ~base:0L text in
            (* Padding is dropped from *both* sides, and by address rather than by
             spelling: objdump decodes filler as instructions and this assembler
             prints the directive that produced it, so a spelling-based filter
             would be a list of nop renderings that could hide a real
             difference. The byte ranges come from our own dump, and that those
             bytes are right is what the comparison above already establishes
             exactly. A multi-source case's merge-gap fill is exactly this kind
             of padding, so it drops out here the same way M1/M2's alignment
             padding always did - no unit-slicing needed on this side either. *)
            let pads =
              List.filter_map (fun r -> if r.padding then Some (r.off, r.len) else None) rows
            in
            let in_pad off = List.exists (fun (o, l) -> off >= o && off < o + l) pads in
            (* A field GNU left for the linker holds a placeholder in the object
             and its final value in our bound image, so the two renderings of
             that *operand* cannot agree and there is nothing to learn from
             making them. The mnemonic still has to. Which fields those are is
             read from the measured record set, not guessed from the spelling,
             and the operands themselves are compared against that record set
             and against the post-link bytes - twice, and exactly. *)
            let relocated = List.map (fun r -> r.ro_offset) (reloc_records case target b) in
            let mnemonic_only s =
              match String.index_opt s ' ' with Some i -> String.sub s 0 i ^ " ..." | None -> s
            in
            let elide r t =
              if
                List.exists
                  (fun o ->
                    (o >= r.off && o < r.off + r.len)
                    || ((target = "riscv32" || target = "riscv64") && o = r.off - 4))
                  relocated
              then mnemonic_only t
              else t
            in
            let kept = List.filter (fun r -> not r.padding) rows in
            let theirs =
              disasm_records case target b |> List.filter (fun (off, _) -> not (in_pad off))
            in
            if List.length kept <> List.length theirs then
              Printf.printf "%-12s %-8s LENGTH DIFFERS: %d vs %d\n" case target (List.length kept)
                (List.length theirs)
            else
              let pairs =
                List.map2
                  (fun r (_, t) -> (elide r (normalize target r.text), elide r t))
                  kept theirs
              in
              let bad = List.filter (fun (a, b) -> not (String.equal a b)) pairs in
              let elided = List.length (List.filter (fun r -> elide r "x y" <> "x y") kept) in
              if bad = [] then
                Printf.printf "%-12s %-8s %d lines agree%s\n" case target (List.length pairs)
                  (if elided = 0 then ""
                   else
                     Printf.sprintf " (%d relocated operand%s compared as records instead)" elided
                       (if elided = 1 then "" else "s"))
              else
                List.iter
                  (fun (a, b) ->
                    Printf.printf "%-12s %-8s DIFFERS\n    ours:    %s\n    objdump: %s\n" case
                      target a b)
                  bad)

  (* {1 Reassembly}

   The canonical dump has to be exactly re-parseable: feeding it back through
   the assembler must produce the identical image. Stronger than "it decodes",
   because it goes back through the lexer, the grammar, the target's operand
   parser, simplify, lower and encode - and, for a case with a branch, through
   relaxation, which is where a decoded form that lost its rung would show. *)

  (* A multi-source case's canonical dump collapses a MERGE-inserted gap into an
   ordinary [.balign] - the only form a lexer/parser round trip has to write -
   and reassembling that as a single module fills it with {!T.nop_bytes}
   again, not {!T.merge_fill}. On every target except x86_32 those coincide,
   so this is invisible there; on x86_32 they measurably differ (M3 §5), so
   the reassembled bytes genuinely are not identical at the fill site even
   though the ENCODING on both sides is correct and check_bytes already
   proves it against real GNU evidence. Skipped uniformly for every
   multi-source case rather than only where it happens to bite today, so a
   future target with the same divergence is not silently exempted. *)
  let check_round_trip case target =
    if is_multi_source case target then
      Printf.printf
        "%-12s %-8s (multi-source: round-trip comparison not yet implemented per unit)\n" case
        target
    else
      let (module D : Rivet_target_intf.Target.DRIVER) = target_of_name target in
      match build case target with
      | Error _ -> Printf.printf "%-12s %-8s (does not assemble)\n" case target
      | Ok b -> (
          let bytes = Option.value ~default:"" (segment_bytes b.bound ".text") in
          let address = List.assoc ".text" b.addresses in
          match D.dump_disasm_canonical ~address bytes with
          | Error ds -> Printf.printf "%-12s %-8s DISASM %s\n" case target (brief ds)
          | Ok text -> (
              let source =
                Rivet_foundation.Span.source
                  ~name:(case ^ ":" ^ target ^ ":round-trip")
                  ~contents:("\t.text\n" ^ text)
              in
              match D.assemble ~unit_name:"round" ~source () with
              | Error ds -> Printf.printf "%-12s %-8s REASSEMBLE %s\n" case target (brief ds)
              | Ok l -> (
                  match Rivet_image.bind_image l ~addresses:[ (".text", address) ] with
                  | Error ds -> Printf.printf "%-12s %-8s REBIND %s\n" case target (brief ds)
                  | Ok img ->
                      let again = Option.value ~default:"" (segment_bytes img ".text") in
                      if String.equal again bytes then
                        Printf.printf "%-12s %-8s %d bytes reproduced\n" case target
                          (String.length again)
                      else
                        Printf.printf "%-12s %-8s DIFFERS\n    first:  %s\n    second: %s\n" case
                          target (hex_of bytes) (hex_of again))))

  (* {1 O1 - classify before comparing}

   Internal fixups and ELF records are not the same set, and the difference is
   not a detail. GNU resolves many same-section references while assembling,
   while this assembler needs a fixup to lay out, relax, bind and patch a branch
   that has no ELF record at all - so a gate that expected one record per fixup
   would be wrong in both directions at once.

   The measurements say the obvious rules are wrong too. A defined *global*
   same-section [jmp] or [jcc] is assembler-resolved on both x86 profiles, so
   binding is not decisive; and the record for a call to a global is
   [R_X86_64_PLT32] on x86-64 but plain [R_386_PC32] on x86-32, so the ELF
   spelling is not uniform either. Every cell below therefore comes from the
   committed oracle for a case that exercises it - [direct_call] for the call
   row, [global_ldst] for the data rows, [loop] and [cond_select] for the
   branches, whose reloc.txt files are empty on all four targets.

   Keyed on the fixup kind rather than on the role, because AArch64 splits one
   data reference into an [adrp] page record and a scaled low-12 record whose
   type names its access width: the role is the same for both and the ELF
   spelling is not. The role travels alongside and is asserted, so a kind whose
   role changed would fail here rather than quietly re-key the table. *)

  type classification = Assembler_resolved | Linker_visible of string

  let classify ~target ~kind_name ~role ~defined ~same_section =
    let expect r =
      if role <> r then
        Some
          (Printf.sprintf "kind %s has role %s, table says %s" kind_name
             (Rivet_asm_core.Lowered_ast.fixup_role_name role)
             (Rivet_asm_core.Lowered_ast.fixup_role_name r))
      else None
    in
    let call = expect Rivet_asm_core.Lowered_ast.Call in
    let branch = expect Rivet_asm_core.Lowered_ast.Branch in
    let data = expect Rivet_asm_core.Lowered_ast.Data_address in
    match (target, kind_name) with
    (* A call keeps a record on every target, whatever the binding: the linker may
     still have to route it through a stub. *)
    | "x86_32", "pcrel32-call" -> (call, Linker_visible "R_386_PC32")
    | "x86_64", "pcrel32-call" -> (call, Linker_visible "R_X86_64_PLT32")
    | "arm", "pcrel-call" -> (call, Linker_visible "R_ARM_CALL")
    | "aarch64", "pcrel-call26" -> (call, Linker_visible "R_AARCH64_CALL26")
    | ("riscv32" | "riscv64"), "call-hi20" -> (call, Linker_visible "R_RISCV_CALL_PLT")
    | ("riscv32" | "riscv64"), "call-lo12-i" -> (call, Assembler_resolved)
    (* A branch to a symbol defined in this section is resolved while assembling
     on every target, and to one outside it is not reachable in M2 at all. *)
    | _, ("pcrel8-branch" | "pcrel32-branch" | "pcrel-b26" | "pcrel-b19" | "pcrel-b13" | "pcrel-j21")
      ->
        ( branch,
          if defined && same_section then Assembler_resolved else Linker_visible "unsupported" )
    (* Data addresses. x86-32 takes the absolute address, x86-64 a RIP-relative
     displacement, and the two fixed-width targets split one reference across
     two instructions and therefore two records. *)
    | "x86_32", "abs32" -> (data, Linker_visible "R_386_32")
    | "x86_64", "pcrel32-data" -> (data, Linker_visible "R_X86_64_PC32")
    | "arm", "movw-abs-nc" -> (data, Linker_visible "R_ARM_MOVW_ABS_NC")
    | "arm", "movt-abs" -> (data, Linker_visible "R_ARM_MOVT_ABS")
    | "aarch64", "adrp-page" -> (data, Linker_visible "R_AARCH64_ADR_PREL_PG_HI21")
    | "aarch64", "add-lo12" -> (data, Linker_visible "R_AARCH64_ADD_ABS_LO12_NC")
    | "aarch64", "ldst32-lo12" -> (data, Linker_visible "R_AARCH64_LDST32_ABS_LO12_NC")
    | "aarch64", "ldst64-lo12" -> (data, Linker_visible "R_AARCH64_LDST64_ABS_LO12_NC")
    | ("riscv32" | "riscv64"), "pcrel-hi20" -> (data, Linker_visible "R_RISCV_PCREL_HI20")
    | ("riscv32" | "riscv64"), "pcrel-lo12-i" -> (data, Linker_visible "R_RISCV_PCREL_LO12_I")
    | ("riscv32" | "riscv64"), "pcrel-lo12-s" -> (data, Linker_visible "R_RISCV_PCREL_LO12_S")
    | _ -> (Some (Printf.sprintf "no table row for %s on %s" kind_name target), Assembler_resolved)

  (* O2. Only the PC-relative classes subtract a place - an absolute relocation
   has no P - so the transform is per class rather than one formula with a zero
   in it. [o_offset - o_place] is where the whole of the x86 subtlety lives:
   [o_place] is the *realized* instruction length past the fragment, so a
   REX-prefixed RIP-relative load and a plain one produce different addends from
   the same expression. *)
  let elf_addend ~pcrel ~(site : Rivet_image.site) ~addend =
    if pcrel then
      Int64.add addend (Int64.of_int (site.Rivet_image.o_offset - site.Rivet_image.o_place))
    else addend

  let is_pcrel kind_name =
    match kind_name with
    | "pcrel8-branch" | "pcrel32-branch" | "pcrel32-call" | "pcrel32-data" | "pcrel-b26"
    | "pcrel-b19" | "pcrel-call" | "pcrel-call26" | "adrp-page" ->
        true
    | "pcrel-b13" | "pcrel-j21" | "call-hi20" | "pcrel-hi20" -> true
    | _ -> false

  (* Keyed multisets, never positional: the order observations come out in is a
   traversal artifact and would change under a refactor of layout that changed
   nothing about the program. A duplicate key is reported rather than allowed to
   mask a mismatch, and missing, extra and mismatched are three different
   findings. *)
  let show_record (sec, off, ty, sym, addend) =
    Printf.sprintf "%s+0x%x %s %s%s" sec off ty sym
      (match addend with None -> "" | Some a -> Printf.sprintf " %+Ld" a)

  (* Keyed multisets, never positional: the order observations come out in is a
   traversal artifact and would change under a refactor of layout that changed
   nothing about the program. Missing, extra and mismatched are three different
   findings, and a duplicate key is reported rather than allowed to decide
   silently which of two records a lookup finds. *)
  let compare_records ~predicted ~measured =
    let key (s, o, _, _, _) = (s, o) in
    let dups what l =
      List.filteri
        (fun i x -> List.exists (fun y -> key x = key y) (List.filteri (fun j _ -> j < i) l))
        l
      |> List.map (fun d -> Printf.sprintf "duplicate key in %s: %s" what (show_record d))
    in
    let find l k = List.find_opt (fun x -> key x = k) l in
    dups "ours" predicted @ dups "GNU's" measured
    @ List.filter_map
        (fun p ->
          match find measured (key p) with
          | None -> Some ("we predict a record GNU does not have: " ^ show_record p)
          | Some m when m <> p ->
              Some (Printf.sprintf "mismatch: ours %s, GNU %s" (show_record p) (show_record m))
          | Some _ -> None)
        predicted
    @ List.filter_map
        (fun m ->
          if find predicted (key m) = None then
            Some ("GNU has a record we do not predict: " ^ show_record m)
          else None)
        measured

  let check_relocs case target =
    match build case target with
    | Error _ -> Printf.printf "%-12s %-8s (does not assemble)\n" case target
    | Ok b ->
        let problems = ref [] and predicted = ref [] and resolved = ref 0 in
        let sites = ref [] in
        let note s = problems := s :: !problems in
        let oracle_symbol s =
          if target <> "riscv32" && target <> "riscv64" then s
          else
            try Scanf.sscanf s "#L%d#%d" (fun n k -> Printf.sprintf ".L%d^B%d" n (k + 1))
            with _ -> s
        in
        (* Each classified site is *named* in the transcript, not merely counted.
         An assembler-resolved fixup is absent from reloc.txt by definition, so a
         count is the only thing the record comparison can say about it; listing
         the sites is what makes "we resolved this one ourselves" a reviewable
         claim about a place in the image rather than a number. Every column O1
         indexes on is on the line, so changing any of them independently changes
         the transcript. *)
        let site_line (site : Rivet_image.site) (r : Rivet_image.symbolic_ref) cls =
          sites :=
            Printf.sprintf "  %s+0x%-4x %-16s %-6s %-6s %-9s %s" site.Rivet_image.o_section
              site.Rivet_image.o_offset site.Rivet_image.o_kind_name
              (Rivet_asm_core.Lowered_ast.fixup_role_name site.Rivet_image.o_role)
              (match r.Rivet_image.binding with `Local -> "local" | `Global -> "global")
              (if not r.Rivet_image.defined then "undefined"
               else if r.Rivet_image.same_section then "same-sec"
               else "other-sec")
              cls
            :: !sites
        in
        List.iter
          (fun o ->
            match o with
            | Rivet_image.Non_normalizable n ->
                (* No single symbol, so no record to compare against; O2's
                 post-link byte comparison is what covers it. *)
                note
                  (Printf.sprintf "non-normalizable at %s+0x%x: %s"
                     n.Rivet_image.nn_site.Rivet_image.o_section
                     n.Rivet_image.nn_site.Rivet_image.o_offset n.Rivet_image.nn_expr)
            | Rivet_image.Symbolic_ref r -> (
                let site = r.Rivet_image.site in
                let complaint, cls =
                  classify ~target ~kind_name:site.Rivet_image.o_kind_name
                    ~role:site.Rivet_image.o_role ~defined:r.Rivet_image.defined
                    ~same_section:r.Rivet_image.same_section
                in
                Option.iter note complaint;
                match cls with
                | Assembler_resolved ->
                    incr resolved;
                    site_line site r "assembler-resolved"
                | Linker_visible ty ->
                    site_line site r ty;
                    let rela = List.exists (fun x -> x.ro_rela) (reloc_records case target b) in
                    predicted :=
                      ( site.Rivet_image.o_section,
                        site.Rivet_image.o_offset,
                        ty,
                        oracle_symbol r.Rivet_image.symbol,
                        if rela then
                          Some
                            (elf_addend
                               ~pcrel:(is_pcrel site.Rivet_image.o_kind_name)
                               ~site ~addend:r.Rivet_image.addend)
                        else None )
                      :: !predicted))
          (Rivet_image.fixup_observations b.laid_out);
        let measured =
          List.map
            (fun r -> (r.ro_section, r.ro_offset, r.ro_type, r.ro_symbol, r.ro_addend))
            (reloc_records case target b)
        in
        List.iter note (compare_records ~predicted:(List.rev !predicted) ~measured);
        if !problems = [] then (
          Printf.printf "%-12s %-8s %d linker-visible, %d assembler-resolved\n" case target
            (List.length !predicted) !resolved;
          List.iter print_endline (List.rev !sites))
        else List.iter (fun p -> Printf.printf "%-12s %-8s %s\n" case target p) (List.rev !problems)
end
