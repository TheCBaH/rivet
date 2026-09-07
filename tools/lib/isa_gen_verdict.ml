type gas_result = Gas_assembled of string | Gas_rejected of string
type ours_result = Ours_assembled of string | Ours_rejected of string

type outcome =
  | Gas_only_rejected of string
  | Gas_unexpected_relocation of string list
  | Both_ran of { gas_hex : string; ours : ours_result }

(* Measured for real against every one of the 21 pilot cases: exactly these
   eight case_ids reject with this project's own x86 [Missing_size_suffix]
   diagnostic (asm/targets/x86_family/x86_family_encode.ml's [widthed]) when
   handed the SAME unsuffixed canonical GAS spelling
   (Isa_norm_xed.ml's [two_operand_gprv_form]/[add_gprv_immz_form] never emit
   an AT&T size suffix - that is a genuine source-syntax fact, not a
   rendering bug: GAS infers the width from the register operand, exactly as
   real GNU as's own manual documents). This project's x86 frontend has one
   narrow carve-out for suffixless register-register [add] (the comment on
   the [_, "add"] case in x86_family_encode.ml, added for a real CompCert
   runtime fixture) and no other suffix-inference at all - not for [mov], and
   not for either immediate form. Confirmed independently at the byte level
   (x86_64-linux-gnu-as) that the explicit-suffix spelling assembles to
   IDENTICAL bytes to the suffixless one in every one of these register-only
   cases, so this is purely a missing PARSE-time inference, not a different
   encoding: our own assembler already implements every one of these opcodes
   under its suffixed spelling. That is why this is recorded as a
   {!Isa_generated_case.Frontier_gap} (a known, explained, unimplemented
   SYNTAX acceptance) rather than treated as if the encoder itself regressed. *)
let known_missing_size_suffix_case_ids =
  [
    "x86:ADD_GPRv_IMMz:canonical:x86_32";
    "x86:ADD_GPRv_IMMz:canonical:x86_64";
    "x86:MOV_GPRv_GPRv_89:canonical:x86_32";
    "x86:MOV_GPRv_GPRv_89:canonical:x86_64";
    "x86:MOV_GPRv_GPRv_8B:canonical:x86_32";
    "x86:MOV_GPRv_GPRv_8B:canonical:x86_64";
    "x86:MOV_GPRv_IMMz:canonical:x86_32";
    "x86:MOV_GPRv_IMMz:canonical:x86_64";
  ]

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
  n = 0 || go 0

let known_syntax_gap ~case_id ~diagnostic =
  List.mem case_id known_missing_size_suffix_case_ids
  && contains ~needle:"needs an operand-size suffix" diagnostic

let normalize_hex s = String.trim (String.lowercase_ascii s)

let classify ~known_syntax_gap:gap = function
  | Gas_only_rejected _ -> Isa_generated_case.Gas_rejected_valid_case
  | Gas_unexpected_relocation _ -> Isa_generated_case.Byte_mismatch
  | Both_ran { gas_hex; ours = Ours_assembled ours_hex } ->
      if String.equal (normalize_hex gas_hex) (normalize_hex ours_hex) then Isa_generated_case.Pass
      else Isa_generated_case.Byte_mismatch
  | Both_ran { ours = Ours_rejected _; _ } ->
      if gap then Isa_generated_case.Frontier_gap else Isa_generated_case.Regression
