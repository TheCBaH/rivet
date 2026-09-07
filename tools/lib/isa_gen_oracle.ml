type outcome =
  | Assembled_matching of { bytes_hex : string }
  | Assembled_mismatched of { bytes_hex : string; detail : string }
  | Unexpected_relocation of { bytes_hex : string; relocations : string list }
  | Rejected of string

let le_int_of_bytes bytes =
  let n = String.length bytes in
  let v = ref 0 in
  for i = n - 1 downto 0 do
    v := (!v lsl 8) lor Char.code bytes.[i]
  done;
  !v

let hex_of_first_byte bytes =
  if String.length bytes = 0 then "<empty>" else Printf.sprintf "0x%02x" (Char.code bytes.[0])

(* [mask]/[value] (isa-db hex strings, e.g. "0x707f") and [opcode] (e.g.
   "0x81") are both already valid OCaml integer-literal syntax - [int_of_string]
   parses the "0x" prefix itself, no stripping needed. *)
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
        let m = int_of_string mask and want = int_of_string value in
        if observed land m = want then Ok ()
        else
          Error
            (Printf.sprintf "0x%x & mask 0x%x = 0x%x, expected value 0x%x" observed m
               (observed land m) want)
  | Isa_norm_model.X86_encoding { opcode; _ } ->
      let expected = String.lowercase_ascii opcode in
      let observed = String.lowercase_ascii (hex_of_first_byte bytes) in
      if String.equal observed expected then Ok ()
      else Error (Printf.sprintf "expected leading byte %s, observed %s" expected observed)

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
  n = 0 || go 0

(* Plan §5.4: "Begin with relocation-free instructions in .text ... Inspect
   relocation tables and fail if a supposedly relocation-free case contains
   one. Never compare unresolved object placeholders with our bound image."
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

let run (case : Isa_generated_case.case) (encoding : Isa_norm_model.encoding) =
  let tools = Gnu_tools.for_target case.target in
  let* () = Gnu_tools.require tools ~qemu:false in
  let* tool_version = Gnu_tools.version_line tools `As in
  let tool_label = Printf.sprintf "%s-as-%s" (Target.to_string case.target) tool_version in
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
            stdout = Option.value ~default:"" result.Tool_process.stdout;
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
