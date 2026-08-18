(* Additive execution ABI v2 metadata. ABI v1 remains in [Abi] unchanged;
   this module owns the six-profile namespace and the RISC-V execution rules. *)

type profile = Abi.profile = X86_32 | X86_64 | Arm | Aarch64 | Riscv32 | Riscv64

let all_profiles = Abi.v2_profiles

let profile_id = function
  | X86_32 -> 1
  | X86_64 -> 2
  | Arm -> 3
  | Aarch64 -> 4
  | Riscv32 -> 5
  | Riscv64 -> 6

let profile_of_id id = List.find_opt (fun profile -> profile_id profile = id) all_profiles

let profile_name = function
  | X86_32 -> "x86_32"
  | X86_64 -> "x86_64"
  | Arm -> "arm"
  | Aarch64 -> "aarch64"
  | Riscv32 -> "riscv32"
  | Riscv64 -> "riscv64"

let profile_of_name name = List.find_opt (fun p -> String.equal (profile_name p) name) all_profiles
let word_bits = function X86_32 | Arm | Riscv32 -> 32 | X86_64 | Aarch64 | Riscv64 -> 64
let entry_alignment = function Riscv32 | Riscv64 | Arm | Aarch64 -> 4 | X86_32 | X86_64 -> 1
let stack_alignment = function Arm -> 8 | _ -> 16

let return_register = function
  | X86_32 | X86_64 -> "eax"
  | Arm -> "r0"
  | Aarch64 -> "w0"
  | Riscv32 | Riscv64 -> "x10"

let riscv_callee_saved =
  [ "x8"; "x9"; "x18"; "x19"; "x20"; "x21"; "x22"; "x23"; "x24"; "x25"; "x26"; "x27" ]

let callee_saved = function Riscv32 | Riscv64 -> riscv_callee_saved | _ -> []
let needs_riscv_flush_icache = function Riscv32 | Riscv64 -> true | _ -> false
let abi_version = 2
let manifest_magic = "ASMEXE\000\002"
let result_magic = "ASMRES\000\002"

(* V2 deliberately retains every v1 wire offset and bound. *)
let page_size = Abi.page_size
let window_size = Abi.window_size
let window_base p = if word_bits p = 32 then 0x30000000L else 0x40000000L
let code_addr p = window_base p
let rodata_addr p = Int64.add (window_base p) 0x10000L
let data_addr p = Int64.add (window_base p) 0x20000L
let result_addr p = Int64.add (window_base p) 0x30000L
let guard_addr p = Int64.add (window_base p) 0x3f000L
let stack_start p = Int64.add (window_base p) 0x40000L

(* M3's fixed BSS window (.ai/asm_plan.md §12; the M3 plan's §11), additive to
   v2 rather than v1: not a wire-format change (a segment's address and
   length are ordinary manifest fields either version can carry), but a new
   *policy* decision, and v1 is frozen to its own. [bss_addr] is exactly
   [stack_start + Abi.stack_size_max] - the first byte after the *largest*
   stack the ABI's own bound permits, so BSS never overlaps the stack
   regardless of which [stack_size] a given manifest actually chooses -
   through [bss_addr + bss_max_extent], the window's own end. Both the GNU
   fixture-oracle's controlled-link address for [.bss] and this in-repo
   ABI's placement use this same function, so a fixture that passes one
   never fails the other purely because the two paths picked different
   addresses. *)
let bss_addr p = Int64.add (stack_start p) (Int64.of_int Abi.stack_size_max)
let bss_max_extent = 0x80000
let manifest_header_size = Abi.manifest_header_size
let segment_descriptor_size = Abi.segment_descriptor_size
let result_record_size = Abi.result_record_size
let result_page_size = Abi.result_page_size

module Manifest_off = Abi.Manifest_off
module Descriptor_off = Abi.Descriptor_off
module Result_off = Abi.Result_off

type cache_sync_subcode = Arm_cacheflush | Aarch64_line_size | Riscv_flush_icache

let cache_sync_subcode = function
  | Arm_cacheflush -> 1
  | Aarch64_line_size -> 2
  | Riscv_flush_icache -> 3

let cache_sync_name = function
  | Arm_cacheflush -> "arm_cacheflush"
  | Aarch64_line_size -> "aarch64_line_size"
  | Riscv_flush_icache -> "riscv_flush_icache"

let normalize_return32 value = Int64.shift_right (Int64.shift_left value 32) 32
