type outcome =
  | Assembled_matching of { bytes_hex : string }
  | Assembled_mismatched of { bytes_hex : string; detail : string }
  | Unexpected_relocation of { bytes_hex : string; relocations : string list }
  | Rejected of string

let le_int_of_bytes bytes =
  let n = String.length bytes in
  (* ISA encoding words are unsigned 32-bit quantities.  Do not use OCaml's
     machine [int] here: it has only 31 value bits on i386 and armv7 hosts. *)
  let v = ref 0L in
  for i = n - 1 downto 0 do
    v := Int64.logor (Int64.shift_left !v 8) (Int64.of_int (Char.code bytes.[i]))
  done;
  !v

let hex_of_byte_at i bytes =
  if String.length bytes <= i then "<empty>" else Printf.sprintf "0x%02x" (Char.code bytes.[i])

(* [mask]/[value] (isa-db hex strings, e.g. "0xfe00707f") and [opcode]
   (e.g. "0x81") are both already valid OCaml integer-literal syntax.  RISC-V
   masks and values may use all 32 bits, so parse them as [Int64] rather than
   a host-width [int]. *)
let observed_form_check (encoding : Isa_norm_model.encoding) bytes =
  match encoding with
  | Isa_norm_model.Riscv_encoding { width_bits; mask; value } ->
      let want_bytes = width_bits / 8 in
      if String.length bytes <> want_bytes then
        Error
          (Printf.sprintf "expected %d assembled bytes for a %d-bit encoding, got %d" want_bytes
             width_bits (String.length bytes))
      else
        let observed = le_int_of_bytes bytes in
        let m = Int64.of_string mask and want = Int64.of_string value in
        if Int64.equal (Int64.logand observed m) want then Ok ()
        else
          Error
            (Printf.sprintf "0x%Lx & mask 0x%Lx = 0x%Lx, expected value 0x%Lx" observed m
               (Int64.logand observed m) want)
  | Isa_norm_model.X86_encoding { space; opcode; _ } ->
      let expected = String.lowercase_ascii opcode in
      (* [space = "vex"] records carry only the trailing opcode byte in
         [opcode]; the VEX prefix itself (not part of [opcode_map]) precedes
         it and is 2 bytes (leading 0xc5) or 3 bytes (leading 0xc4) wide, a
         width GAS - not this project - chooses freely for a given mnemonic
         and operands. Every other [space] keeps the pre-existing plain
         byte-0 comparison unchanged, including legacy mandatory-prefix
         forms (e.g. F2/66 before the escape+opcode bytes), which this
         function has never accounted for and still does not; that is a
         separate, wider gap left for its own follow-up. *)
      let opcode_offset =
        if String.equal space "vex" && String.length bytes > 0 then
          match Char.code bytes.[0] with 0xc5 -> 2 | 0xc4 -> 3 | _ -> 0
        else 0
      in
      let observed = String.lowercase_ascii (hex_of_byte_at opcode_offset bytes) in
      if String.equal observed expected then Ok ()
      else Error (Printf.sprintf "expected leading byte %s, observed %s" expected observed)

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
  n = 0 || go 0

(* Begin with relocation-free instructions in .text: inspect
   relocation tables and fail if a supposedly relocation-free case contains
   one, never comparing unresolved object placeholders with our bound image.
   Every current pilot case is a plain register/immediate instruction with no
   symbol reference, so none is expected to trip this - but nothing upstream
   of this check enforces that expectation, and a placeholder byte compared as
   if it were a real encoding would be a silent, misleading Pass. *)
let has_text_relocations reloc_text = contains ~needle:"RELOCATION RECORDS FOR [.text]" reloc_text

let text_relocation_lines reloc_text =
  String.split_on_char '\n' (String.trim reloc_text) |> List.filter (fun l -> String.trim l <> "")

let ( let* ) = Result.bind

(* The fixed basenames this function always uses inside its own private
   scratch directory - never the real scratch path, which is machine-local
   and nondeterministic. A committed artifact's argv must not embed where the
   checkout happened to run ("removing temporary paths and timestamps from
   committed identities"), so the persisted corpus records this normalized
   argv, not the literal one passed to the child process. *)
let normalized_argv (case : Isa_generated_case.case) =
  case.configuration @ [ "-o"; "case.o"; "case.s" ]

(* The toolchain probe and the version line are per target, not per case:
   resolved once per process and reused (a regeneration runs thousands of
   cases against the same binaries). *)
let probed_labels : (Target.t, string) Hashtbl.t = Hashtbl.create 8

let gas_tool_label target tools =
  match Hashtbl.find_opt probed_labels target with
  | Some label -> Ok label
  | None ->
      let* () = Gnu_tools.require tools ~qemu:false in
      let* tool_version = Gnu_tools.version_line tools `As in
      let label = Printf.sprintf "%s-as-%s" (Target.to_string target) tool_version in
      Hashtbl.replace probed_labels target label;
      Ok label

let run (case : Isa_generated_case.case) (encoding : Isa_norm_model.encoding) =
  let tools = Gnu_tools.for_target case.target in
  let* tool_label = gas_tool_label case.target tools in
  Tool_workspace.with_scratch ~label:"isa-generated" (fun work ->
      let src = Fpath.(work / "case.s") in
      let obj = Fpath.(work / "case.o") in
      let* () = Tool_fs.write src case.rendered_source in
      let* result, gas_outcome =
        Gnu_tools.try_assemble_with_args tools ~args:case.configuration ~src ~obj
      in
      let artifact_of ?(relocations = []) bytes =
        Isa_generated_case.
          {
            tool_label;
            argv = normalized_argv case;
            exit_status = result.Tool_process.status;
            stdout =
              Gnu_tools.replace_all ~sub:(Fpath.to_string src) ~by:"case.s"
                (Option.value ~default:"" result.Tool_process.stdout);
            (* The scratch path is machine-local and differs per run; the
               committed identity is the fixed basename, as in [argv]. *)
            stderr = "";
            (* Gnu_tools.run_as_capturing merges stdout/stderr into one
               capture (Err_to_stdout), matching the shell's `2>&1` - splitting
               it back apart would reorder interleaved output, so the whole
               capture lives in [stdout] and [stderr] stays empty. *)
            bytes;
            relocations;
            (* Not full evidence: {!has_text_relocations} is a plain presence
               check on the object's own .text relocation table, not the
               committed section/symbol/fixup evidence a future linked-image
               comparison will need - it exists to make an unexpectedly
               relocatable case a loud, distinct failure instead of a silent
               placeholder-bytes Pass. *)
          }
      in
      match gas_outcome with
      | Gnu_tools.Rejected body -> Ok (Rejected body, artifact_of None)
      | Gnu_tools.Assembled -> (
          let* reloc_text = Gnu_tools.objdump_relocs tools obj in
          let bin = Fpath.(work / "text.bin") in
          let* (_ : bool) = Gnu_tools.objcopy_section tools ~src:obj ~section:".text" ~out:bin in
          let* bytes = Tool_fs.read bin in
          let bytes_hex = Hex_dump.of_bytes bytes in
          if has_text_relocations reloc_text then
            let relocations = text_relocation_lines reloc_text in
            Ok
              ( Unexpected_relocation { bytes_hex; relocations },
                artifact_of ~relocations (Some bytes_hex) )
          else
            let artifact = artifact_of (Some bytes_hex) in
            match observed_form_check encoding bytes with
            | Ok () -> Ok (Assembled_matching { bytes_hex }, artifact)
            | Error detail -> Ok (Assembled_mismatched { bytes_hex; detail }, artifact)))

(* The GAS half of a committed record is reusable when it was produced from
   the same case by the same assembler version with the same argv: the
   outcome is then recomputed from the recorded artifact exactly as
   {!Isa_generated_corpus.replay} does, and only the encoding check - which
   depends on today's normalizer - is re-run. *)
let reuse (case : Isa_generated_case.case) (encoding : Isa_norm_model.encoding)
    (artifact : Isa_generated_case.artifact) =
  let tools = Gnu_tools.for_target case.target in
  let* label = gas_tool_label case.target tools in
  if (not (String.equal artifact.tool_label label)) || artifact.argv <> normalized_argv case then
    Ok None
  else
    match artifact.bytes with
    | None -> (
        let result =
          {
            Tool_process.status = artifact.exit_status;
            stdout = Some artifact.stdout;
            stderr = None;
          }
        in
        match Gnu_tools.gas_outcome_of_result ~src:(Fpath.v "case.s") result with
        | Gnu_tools.Rejected body -> Ok (Some (Rejected body, artifact))
        | Gnu_tools.Assembled -> Ok None)
    | Some bytes_hex -> (
        if artifact.relocations <> [] then
          Ok
            (Some (Unexpected_relocation { bytes_hex; relocations = artifact.relocations }, artifact))
        else
          let* raw = Hex_dump.parse bytes_hex in
          match observed_form_check encoding raw with
          | Ok () -> Ok (Some (Assembled_matching { bytes_hex }, artifact))
          | Error detail -> Ok (Some (Assembled_mismatched { bytes_hex; detail }, artifact)))
