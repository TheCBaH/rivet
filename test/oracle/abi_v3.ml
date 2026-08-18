(* Additive execution ABI v3 metadata (asm/docs/exec-abi-v3.md). ABI v1 and v2
   remain in [Abi]/[Abi_v2] unchanged; this module owns the result-vector
   extension - the observation-descriptor table and its wire constants.

   Proven against the four legacy profiles first (docs/exec-abi-v3.md §6,
   .ai/asm_plan.md M4), then against RISC-V (both XLEN profiles, sharing one
   validator per asm/helpers/riscv.c's own header comment - the wire format
   is profile-agnostic, and RISC-V's v3 support is the same ABI_VERSION-guarded
   C source as its v2 path rather than a second file). [all_profiles] is
   [Abi.all_profiles] (the full six-profile v2 set): every profile this
   milestone's exit criterion and beyond now has a real, QEMU-proven v3
   helper for. *)

type profile = Abi.profile = X86_32 | X86_64 | Arm | Aarch64 | Riscv32 | Riscv64

let all_profiles = Abi.all_profiles
let profile_id = Abi.profile_id
let profile_of_id = Abi.profile_of_id
let profile_name = Abi.profile_name
let profile_of_name name = List.find_opt (fun p -> String.equal (profile_name p) name) all_profiles
let abi_version = 3
let manifest_magic = "ASMEXE\000\003"
let result_magic = "ASMRES\000\003"

(* V3 deliberately retains every v1/v2 wire offset, bound and address. *)
let page_size = Abi.page_size
let window_size = Abi.window_size
let window_base = Abi.window_base
let code_addr = Abi.code_addr
let rodata_addr = Abi.rodata_addr
let data_addr = Abi.data_addr
let result_addr = Abi.result_addr
let guard_addr = Abi.guard_addr
let stack_start = Abi.stack_start
let bss_addr = Abi_v2.bss_addr
let manifest_header_size = Abi.manifest_header_size
let segment_descriptor_size = Abi.segment_descriptor_size
let result_record_size = Abi.result_record_size
let result_page_size = Abi.result_page_size
let n_expected_max = Abi.n_expected_max

module Manifest_off = Abi.Manifest_off
module Descriptor_off = Abi.Descriptor_off
module Result_off = Abi.Result_off

(* {1 The observation-descriptor table (exec-abi-v3.md §2)}

   Present only when [n_expected > 1]; its offset is *derived*, never stored,
   from fields the fixed header already carries. *)

let obs_descriptor_size = 16

module Obs_descriptor_off = struct
  let vaddr = 0
  let width = 8
  let flags = 9
  let reserved = 10
  let reserved_len = 6
end

(* [120] is [Abi.manifest_header_size], spelled as the literal exec-abi-v3.md
   §2's [obs_table_off] formula uses, so the formula on the page matches the
   doc without an extra lookup. *)
let obs_table_off ~n_segments = 120 + (segment_descriptor_size * n_segments)
let obs_table_len ~n_expected = obs_descriptor_size * (n_expected - 1)

let manifest_min_size ~n_segments ~n_expected =
  obs_table_off ~n_segments + obs_table_len ~n_expected

(* {1 Sign/zero extension (exec-abi-v3.md §2, flags bit 0)} *)

let sign_extend_flag = 1

let extend ~width ~flags raw =
  let bits = width * 8 in
  if bits >= 64 then raw
  else
    let shift = 64 - bits in
    let shifted = Int64.shift_left raw shift in
    if flags land sign_extend_flag <> 0 then Int64.shift_right shifted shift
    else Int64.shift_right_logical shifted shift

(* {1 New class-64 subcodes (exec-abi-v3.md §5)}

   Continuing class 64's numbering from v1's highest (42), the same pattern
   v2 used for its class-67 addition. Subcode 8, [n_expected_range], is
   reused unchanged from [Abi.manifest_subcodes] - v3 widens its condition
   from "n_expected <> 1" to "n_expected outside 1..8" without renumbering
   it, since it occupies the identical stage-3 position in both versions. *)

let obs_subcodes =
  let s code name condition = { Abi.cls = Abi.Manifest; code; name; stage = "4b"; condition } in
  [
    s 43 "obs_bad_width" "width not in {1, 2, 4, 8}";
    s 44 "obs_reserved_nonzero" "observation descriptor reserved bits nonzero";
    s 45 "obs_not_contained" "[vaddr, vaddr+width) not contained in any one declared segment";
    s 46 "obs_not_readable" "the containing segment is not readable";
  ]

let all_subcodes = Abi.all_subcodes_for_version 2 @ obs_subcodes

(* The richest known subcode namespace, for rendering only - subcodes are
   class-scoped, not version-scoped, so a v1 or v2 record's committed
   subcode still has one name here. Mirrors why [Record.pp] already renders
   through [Abi_v2]'s superset rather than [Abi]'s v1-only one. *)
let subcode_of_wire cls code =
  List.find_opt (fun s -> s.Abi.cls = cls && s.Abi.code = code) all_subcodes

(* {1 Version dispatch}

   The one table every magic-selecting call site consults, so a future v4
   touches this pair of functions instead of each call site's own special
   case (round 2's finding: the repeated binary special-case covered only
   two of what are now three real versions). Lives here, the module that
   already depends on both [Abi] and [Abi_v2], rather than in either of
   them - [Abi] is v1's own frozen transcription and must not depend
   forward on its successors. *)

let manifest_magic_for_version = function
  | 1 -> Abi.manifest_magic
  | 2 -> Abi_v2.manifest_magic
  | 3 -> manifest_magic
  | v ->
      invalid_arg (Printf.sprintf "Abi_v3.manifest_magic_for_version: unsupported abi_version %d" v)

let result_magic_for_version = function
  | 1 -> Abi.result_magic
  | 2 -> Abi_v2.result_magic
  | 3 -> result_magic
  | v ->
      invalid_arg (Printf.sprintf "Abi_v3.result_magic_for_version: unsupported abi_version %d" v)

(* The same table for the profile set a version supports - what
   [Qemu_user.available_profiles] consults, so a v3 helper actually gets
   found instead of [Abi.profiles_for_version]'s [_ -> []] silently reporting
   none. This is exactly the "one small lookup table all sites consult" the
   magic-dispatch functions above already are, generalized to the other
   version-scoped quantity call sites actually need. *)
let profiles_for_version = function
  | 1 -> Abi.v1_profiles
  | 2 -> Abi_v2.all_profiles
  | 3 -> all_profiles
  | _ -> []

(* Same pattern for the subcode table: [Abi.all_subcodes_for_version] only
   knows 1 and 2 (it is v1's own frozen transcription and cannot depend
   forward on v3), so [conform.ml]'s reachability walk and its
   [Error "class.name"] case lookups both go through this version instead
   once a case is scoped at v3. *)
let all_subcodes_for_version = function
  | 1 -> Abi.all_subcodes_for_version 1
  | 2 -> Abi.all_subcodes_for_version 2
  | 3 -> all_subcodes
  | _ -> []

let find_subcode ?(abi_version = abi_version) name =
  match List.find_opt (fun s -> Abi.key s = name) (all_subcodes_for_version abi_version) with
  | Some s -> s
  | None -> invalid_arg (Printf.sprintf "Abi_v3.find_subcode: no subcode named %S" name)

(* Version-scoped lookup by wire class/code, for validation (record.ml's
   [err] computation: is this committed subcode one the record's own claimed
   ABI version actually defines?) - deliberately distinct from
   [subcode_of_wire] above, which is version-agnostic and rendering-only. A
   v1 record must not be accepted merely because its subcode happens to
   exist in v3's superset table. *)
let subcode_of_wire_for_version ~abi_version cls code =
  List.find_opt
    (fun s -> s.Abi.cls = cls && s.Abi.code = code)
    (all_subcodes_for_version abi_version)
