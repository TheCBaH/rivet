(* The wire constants of asm/docs/exec-abi-v1.md.

   This module is the single OCaml transcription of the frozen ABI: profiles,
   the normative address map, both wire layouts, the status/record-state
   encodings, and the complete error class and subcode table. Nothing here
   decides policy - exec-abi-v1.md does, and §17 makes changing any of it an ABI
   v2 bump. What this module buys is that the manifest writer, the recovery
   validator and the conformance suite all read the same numbers, so a
   transcription mistake shows up once rather than three times in agreement.

   Test-only (.ai/asm_plan.md §9: production stops at address binding), and
   deliberately free of Unix so it builds and runs on every CI leg, including
   the 31-bit linux/i386 and linux/arm/v7 ones. That is also why every address
   and every 64-bit wire field is [int64] rather than [int]: the 64-bit window
   base 0x40000000 does not fit in a 31-bit native [int]. *)

(* {1 Profiles} *)

type profile = X86_32 | X86_64 | Arm | Aarch64 | Riscv32 | Riscv64

let v1_profiles = [ X86_32; X86_64; Arm; Aarch64 ]
let v2_profiles = v1_profiles @ [ Riscv32; Riscv64 ]
let all_profiles = v2_profiles
let profiles_for_version = function 1 -> v1_profiles | 2 -> v2_profiles | _ -> []

(* §1. The ids are wire values, not an enumeration order we may renumber. *)
let profile_id = function
  | X86_32 -> 1
  | X86_64 -> 2
  | Arm -> 3
  | Aarch64 -> 4
  | Riscv32 -> 5
  | Riscv64 -> 6

let profile_of_id = function
  | 1 -> Some X86_32
  | 2 -> Some X86_64
  | 3 -> Some Arm
  | 4 -> Some Aarch64
  | 5 -> Some Riscv32
  | 6 -> Some Riscv64
  | _ -> None

(* The names tools/target-matrix.sh uses, so a profile named on a command line,
   in a fixture path and in a helper filename is spelled one way. *)
let profile_name = function
  | X86_32 -> "x86_32"
  | X86_64 -> "x86_64"
  | Arm -> "arm"
  | Aarch64 -> "aarch64"
  | Riscv32 -> "riscv32"
  | Riscv64 -> "riscv64"

let profile_of_name n = List.find_opt (fun p -> profile_name p = n) all_profiles
let word_bits = function X86_32 | Arm | Riscv32 -> 32 | X86_64 | Aarch64 | Riscv64 -> 64

(* {1 Address map (§3)}

   One 1 MiB window per profile, everything inside it. The offsets are shared by
   all four profiles; only the base differs, and it differs by word size rather
   than by architecture. *)

let page_size = 4096
let window_size = 0x100000L
let window_base p = if word_bits p = 32 then 0x30000000L else 0x40000000L
let code_addr p = window_base p
let rodata_addr p = Int64.add (window_base p) 0x10000L
let data_addr p = Int64.add (window_base p) 0x20000L
let result_addr p = Int64.add (window_base p) 0x30000L

(* The guard page sits immediately below the stack: 0x3F000 + 0x1000 = 0x40000.
   §10.2 requires it in the occupied set precisely because segments are mapped
   MAP_FIXED after it, so a segment admitted over the guard would silently
   replace PROT_NONE and void the overflow test. *)
let guard_addr p = Int64.add (window_base p) 0x3F000L
let stack_start p = Int64.add (window_base p) 0x40000L

(* §10.4. The stack is [stack_start, stack_start + stack_size) and grows down
   from its top, so these are the bounds on stack_size, not on an address. *)
let stack_size_min = 4 * 1024
let stack_size_max = 256 * 1024
let init_len_max = 256 * 1024
let zero_len_max = 1024 * 1024
let manifest_size_max = 1024 * 1024
let timeout_ms_min = 100
let timeout_ms_max = 60000

(* {1 Permission bits (§8, segment descriptor)} *)

let perm_r = 1
let perm_w = 2
let perm_x = 4
let perm_known = perm_r lor perm_w lor perm_x

(* {1 Manifest layout (§8)} *)

let manifest_magic = "ASMEXE\000\001"
let abi_version = 1
let manifest_header_size = 120
let segment_descriptor_size = 40
let n_segments_max = 8
let n_expected_max = 8

module Manifest_off = struct
  let magic = 0
  let abi_version = 8
  let profile_id = 10
  let total_len = 12
  let case_id = 16
  let n_segments = 20
  let n_expected = 22
  let entry_addr = 24
  let result_addr = 32
  let result_size = 40
  let stack_size = 44
  let timeout_ms = 48
  let reserved = 52
  let reserved_len = 4
  let expected = 56
end

module Descriptor_off = struct
  let vaddr = 0
  let init_len = 8
  let zero_len = 16
  let payload_off = 24
  let align = 32
  let perms = 36
  let reserved = 37
  let reserved_len = 3
end

(* {1 Result block layout (§13)}

   The record is 256 bytes inside a 4096-byte page; 256-4095 are frozen as
   reserved padding that ftruncate zeroes and the helper never writes, so a
   stray guest store through the second alias is detectable. *)

let result_magic = "ASMRES\000\001"
let result_record_size = 256
let result_page_size = 4096
let diag_max = 64
let n_values_max = 8

module Result_off = struct
  let magic = 0
  let abi_version = 8
  let status = 10
  let case_id = 12
  let n_expected = 16
  let n_values = 18
  let diag_len = 20
  let expected = 24
  let values = 88
  let diag = 152
  let runner_error = 216
  let error_subcode = 218
  let record_state = 220
  let reserved = 224
  let reserved_len = 32
end

(* {1 Status and record state (§6, §13)} *)

type status = Not_started | Running | Passed | Failed | Fault

let status_code = function
  | Not_started -> 0
  | Running -> 1
  | Passed -> 2
  | Failed -> 3
  | Fault -> 4

let status_of_code = function
  | 0 -> Some Not_started
  | 1 -> Some Running
  | 2 -> Some Passed
  | 3 -> Some Failed
  | 4 -> Some Fault
  | _ -> None

let status_name = function
  | Not_started -> "not_started"
  | Running -> "running"
  | Passed -> "passed"
  | Failed -> "failed"
  | Fault -> "fault"

type record_state = Bootstrap | Pre_run_error | Validated | Returned

let record_state_code = function
  | Bootstrap -> 0
  | Pre_run_error -> 1
  | Validated -> 2
  | Returned -> 3

let record_state_of_code = function
  | 0 -> Some Bootstrap
  | 1 -> Some Pre_run_error
  | 2 -> Some Validated
  | 3 -> Some Returned
  | _ -> None

let record_state_name = function
  | Bootstrap -> "bootstrap"
  | Pre_run_error -> "pre_run_error"
  | Validated -> "validated"
  | Returned -> "returned"

(* §6: which statuses each state admits. [Fault] belongs to [Validated] - a
   system-mode shim commits it because the guest never returned - and
   [Returned] + [Fault] is invalid, since a guest that returned yields passed or
   failed. The user-mode helper never publishes [Fault] at all, which is exactly
   why the synthetic record tests exist. *)
let status_admitted state status =
  match (state, status) with
  | Bootstrap, Not_started -> true
  | Pre_run_error, Not_started -> true
  | Validated, (Not_started | Running | Fault) -> true
  | Returned, (Running | Passed | Failed) -> true
  | _ -> false

(* {1 Error classes (§14.1)}

   The class is also the process exit code, which is why the numbering starts at
   64: the host accepts 64-69 and normalizes anything else to unexpected_exit
   rather than letting a QEMU loader error look like a guest result. *)

type error_class = Manifest | Io | Mapping | Cache_sync | Protocol | Environment

let all_classes = [ Manifest; Io; Mapping; Cache_sync; Protocol; Environment ]

let class_code = function
  | Manifest -> 64
  | Io -> 65
  | Mapping -> 66
  | Cache_sync -> 67
  | Protocol -> 68
  | Environment -> 69

let class_of_code c = List.find_opt (fun k -> class_code k = c) all_classes

let class_name = function
  | Manifest -> "manifest"
  | Io -> "io"
  | Mapping -> "mapping"
  | Cache_sync -> "cache-sync"
  | Protocol -> "protocol"
  | Environment -> "environment"

let exit_code_min = 64
let exit_code_max = 69

(* {1 Subcodes (§14.2)}

   The complete table, transcribed in document order. Subcodes are scoped to
   their class - ABI v1 deliberately does not require a globally unique
   namespace, because the recovery validator never consults a class-specific
   table before the runner_error commit (§15.2 step 2).

   This list is not just documentation for the reader: the conformance suite
   iterates it and fails if any entry lacks a case that observes it, which is
   how §14.2's "every subcode carries a conformance case proving it is
   observable" is enforced rather than asserted. [stage] is the §10 stage or
   §10.2 group the check belongs to, and it is what makes an unreachable
   ordering visible - revision 9's result/stack collision and revision 11's four
   packing-shadowed payload subcodes were both ordering defects, not missing
   checks. *)

type subcode = { cls : error_class; code : int; name : string; stage : string; condition : string }

let key s = class_name s.cls ^ "." ^ s.name

let manifest_subcodes =
  let s code name stage condition = { cls = Manifest; code; name; stage; condition } in
  [
    s 1 "size_below_minimum" "1" "st_size < 120 (empty or truncated header)";
    s 2 "size_above_maximum" "1" "st_size > 1 MiB";
    s 3 "bad_magic" "3" "bad magic";
    s 4 "bad_abi_version" "3" "unsupported abi_version";
    s 5 "bad_profile_id" "3" "unknown or wrong profile_id";
    s 6 "total_len_mismatch" "3" "total_len <> st_size";
    s 7 "n_segments_range" "3" "n_segments outside 1..8";
    s 8 "n_expected_range" "3" "n_expected <> 1";
    s 9 "result_size_wrong" "3" "result_size <> 256";
    s 10 "timeout_range" "3" "timeout_ms outside 100..60000";
    s 11 "header_reserved_nonzero" "3" "header reserved bytes nonzero";
    s 12 "expected_tail_nonzero" "3" "expected[] tail beyond n_expected nonzero";
    s 13 "descriptor_table_overruns" "4" "descriptor table end exceeds st_size";
    s 14 "payload_overruns" "5" "payload_off + init_len exceeds st_size";
    s 15 "payload_misaligned" "6" "payload_off not 8-byte aligned";
    s 16 "payload_in_metadata" "6" "payload starts inside the metadata region";
    s 17 "payload_pairing" "6" "payload_off <> 0 and init_len = 0, or the converse";
    s 18 "payload_overlap" "7" "two payloads overlap";
    s 19 "packing_not_tight" "8" "payloads not tightly packed in descriptor order";
    s 20 "packing_gap_nonzero" "8" "inter-payload alignment gap is nonzero";
    s 21 "trailing_bytes" "8" "trailing bytes past the canonical file end";
    s 22 "init_len_too_large" "9" "init_len > 256 KiB";
    s 23 "zero_len_too_large" "9" "zero_len > 1 MiB";
    s 24 "segment_empty" "geometry" "zero-length segment (init_len = zero_len = 0)";
    s 25 "align_not_power_of_two" "geometry" "align zero or not a power of two";
    s 26 "vaddr_misaligned" "geometry" "vaddr misaligned against align";
    s 27 "perms_unknown_bits" "geometry" "unknown permission bits set";
    s 28 "perms_write_execute" "geometry" "final permissions are W+X";
    s 29 "address_overflow" "geometry" "address overflow computing data_end";
    s 30 "outside_window" "geometry" "region falls outside the reserved window";
    s 31 "collide_segment_result" "collision" "segment overlaps the result page";
    s 32 "collide_segment_stack" "collision" "segment overlaps the guest stack";
    s 33 "collide_segment_guard" "collision" "segment overlaps the guard page";
    s 34 "collide_result_stack" "collision" "result page overlaps the guest stack";
    s 35 "collide_segment_segment" "collision" "two segments overlap";
    s 36 "entry_not_in_x_segment" "well-formed"
      "entry_addr outside the initialized range of an X segment";
    s 37 "entry_profile_rule" "well-formed" "entry_addr violates the profile rule of §10.3";
    s 38 "result_addr_misaligned" "well-formed" "result_addr not page-aligned";
    s 39 "result_addr_not_normative" "well-formed" "result_addr <> the profile's normative address";
    s 40 "stack_size_invalid" "well-formed"
      "stack_size zero, not a page multiple, or outside 4 KiB..256 KiB";
    s 41 "stack_region_not_normative" "well-formed"
      "stack plus guard does not match the normative region";
    s 42 "address_high_bits_set" "well-formed"
      "high 32 bits of a u64 address field nonzero on a 32-bit profile";
  ]

let io_subcodes =
  let s code name condition = { cls = Io; code; name; stage = "syscall"; condition } in
  [
    s 1 "result_open" "result open failed";
    s 2 "result_ftruncate" "ftruncate failed";
    s 3 "input_open" "input open failed";
    s 4 "input_fstat" "input fstat failed";
    s 5 "input_mmap" "input mmap failed";
  ]

let mapping_subcodes =
  let s code name condition = { cls = Mapping; code; name; stage = "syscall"; condition } in
  [
    s 1 "window_reservation" "window reservation failed (no record can exist)";
    s 2 "control_alias" "control-alias mmap failed (no record can exist)";
    s 3 "result_alias" "second result alias failed";
    s 4 "stack_mapping" "guest stack mapping failed";
    s 5 "guard_mapping" "guard page mapping failed";
    s 6 "segment_mapping" "segment mapping failed";
    s 7 "mprotect" "mprotect to final permissions failed";
  ]

let cache_sync_subcodes =
  let s code name condition = { cls = Cache_sync; code; name; stage = "step 8"; condition } in
  [
    s 1 "arm_cacheflush" "ARM cacheflush syscall failed";
    s 2 "aarch64_line_size" "AArch64 CTR_EL0 reported an unusable line size";
  ]

let riscv_cache_sync_subcode =
  {
    cls = Cache_sync;
    code = 3;
    name = "riscv_flush_icache";
    stage = "step 8";
    condition = "RISC-V riscv_flush_icache syscall failed";
  }

let protocol_subcodes =
  let s code name condition = { cls = Protocol; code; name; stage = "post-return"; condition } in
  [
    s 1 "stack_pointer_corrupt" "guest returned with a corrupted stack pointer";
    s 2 "callee_saved_clobbered" "guest returned with a callee-saved register clobbered";
  ]

let environment_subcodes =
  let s code name condition = { cls = Environment; code; name; stage = "0"; condition } in
  [
    s 1 "page_size_wrong" "AT_PAGESZ <> 4096";
    s 2 "page_size_absent" "AT_PAGESZ absent from the auxiliary vector";
  ]

let all_subcodes =
  manifest_subcodes @ io_subcodes @ mapping_subcodes @ cache_sync_subcodes @ protocol_subcodes
  @ environment_subcodes

let all_subcodes_for_version = function
  | 1 -> all_subcodes
  | 2 -> all_subcodes @ [ riscv_cache_sync_subcode ]
  | _ -> []

let find_subcode ?(abi_version = abi_version) name =
  match List.find_opt (fun s -> key s = name) (all_subcodes_for_version abi_version) with
  | Some s -> s
  | None -> invalid_arg (Printf.sprintf "Abi.find_subcode: no subcode named %S" name)

let subcode_of_wire ?(abi_version = abi_version) cls code =
  List.find_opt (fun s -> s.cls = cls && s.code = code) (all_subcodes_for_version abi_version)

(* {1 Little-endian accessors}

   Every integer on the wire is little-endian at a fixed offset and unaligned
   access is never required (§1), so these are total for any in-range offset.

   The u32 readers return int64 on purpose. A u32 field carries values a 31-bit
   native int cannot hold, and malformed-input tests deliberately store such
   values - reading record_state as an int would make the test that a garbage
   state is rejected depend on the host word size. The writers take int and
   document the fit, because everything this code writes canonically is small;
   deliberately out-of-range values are produced by [Manifest.patch] instead. *)

let get_u8 s off = String.get_uint8 s off
let get_u16 s off = String.get_uint16_le s off
let get_u32 s off = Int64.logand (Int64.of_int32 (String.get_int32_le s off)) 0xFFFFFFFFL
let get_u64 s off = String.get_int64_le s off
let set_u8 b off v = Bytes.set_uint8 b off v
let set_u16 b off v = Bytes.set_uint16_le b off v
let set_u32 b off v = Bytes.set_int32_le b off (Int32.of_int v)
let set_u64 b off v = Bytes.set_int64_le b off v
let set_string b off s = Bytes.blit_string s 0 b off (String.length s)

(* {1 Unsigned 64-bit helpers}

   Addresses are unsigned, and OCaml's Int64 comparison is signed. Every
   comparison of a wire address goes through these, so a manifest claiming
   0x8000000000000000 is treated as the enormous address it is rather than as a
   negative number that trivially passes an upper-bound test. *)

let u64_compare a b = Int64.unsigned_compare a b
let u64_lt a b = u64_compare a b < 0
let u64_le a b = u64_compare a b <= 0

(* [checked_add]/[checked_sub] are §9's arithmetic: [None] is the overflow that
   manifest subcode 29 reports, never a wrapped result. *)
let checked_add a b =
  let s = Int64.add a b in
  if u64_lt s a then None else Some s

let floor_page a = Int64.logand a (Int64.lognot (Int64.of_int (page_size - 1)))

let ceil_page a =
  match checked_add a (Int64.of_int (page_size - 1)) with
  | None -> None
  | Some s -> Some (floor_page s)

(* {1 Ranges}

   Page-rounded [start, stop) pairs. §10.2 checks overlap on rounded page ranges
   because mprotect is page-granular: two segments sharing a page would silently
   take each other's final permissions. *)

type range = { start : int64; stop : int64 }

let range_overlaps a b = u64_lt a.start b.stop && u64_lt b.start a.stop
let range_contains outer inner = u64_le outer.start inner.start && u64_le inner.stop outer.stop

let window_range p =
  let start = window_base p in
  { start; stop = Int64.add start window_size }

let page_range_of addr len =
  match checked_add addr len with
  | None -> None
  | Some stop -> (
      match ceil_page stop with None -> None | Some stop -> Some { start = floor_page addr; stop })

(* The result page, the guard page and the stack, as the occupied regions §10.2
   names alongside the segments. *)
let result_range p =
  { start = result_addr p; stop = Int64.add (result_addr p) (Int64.of_int page_size) }

let guard_range p =
  { start = guard_addr p; stop = Int64.add (guard_addr p) (Int64.of_int page_size) }

let stack_range p ~stack_size =
  let start = stack_start p in
  { start; stop = Int64.add start (Int64.of_int stack_size) }
