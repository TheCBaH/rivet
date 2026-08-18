(* Synthetic host-side coverage for ABI v3's wire layout (asm/docs/exec-abi-v3.md
   §2), scoped exactly to Phase 2's split exit criterion: [Manifest.serialize]'s
   byte layout for the observation-descriptor table, given synthetic
   segment/observation inputs - no helper, no [conform.exe]. Execution-based v3
   conformance (the manifest-side validator actually rejecting a malformed
   table) is a helper behavior and lands with the real generated helper in
   Phase 3.5.

   Every case reads back specific fields through the same [Abi]/[Abi_v3]
   accessors the real validator and [conform.ml]'s cases use, so a
   transcription mistake here would also be a transcription mistake there. *)

open Asm_oracle

let profile = Abi.X86_64
let case = Manifest.single_code profile "\x90\x90\x90\x90"

(* {1 Continuity: n_expected = 1 is byte-shape-identical to v1/v2}

   exec-abi-v3.md §2's whole point for the derived-offset design: only the
   magic and abi_version differ. *)

let%expect_test "n_expected = 1 is byte-shape-identical across abi versions" =
  let v1 = Manifest.serialize ~abi_version:Abi.abi_version case in
  let v3 = Manifest.serialize ~abi_version:Abi_v3.abi_version case in
  Fmt.pr "lengths: v1=%d v3=%d equal=%b@." (String.length v1) (String.length v3)
    (String.length v1 = String.length v3);
  (* Everything from byte 10 onward (past magic + abi_version) is identical. *)
  let tail s = String.sub s 10 (String.length s - 10) in
  Fmt.pr "tails equal: %b@." (String.equal (tail v1) (tail v3));
  Fmt.pr "v1 magic: %S abi_version=%d@."
    (String.sub v1 0 8)
    (Abi.get_u16 v1 Abi.Manifest_off.abi_version);
  Fmt.pr "v3 magic: %S abi_version=%d@."
    (String.sub v3 0 8)
    (Abi.get_u16 v3 Abi.Manifest_off.abi_version);
  [%expect
    {|
    lengths: v1=168 v3=168 equal=true
    tails equal: true
    v1 magic: "ASMEXE\000\001" abi_version=1
    v3 magic: "ASMEXE\000\003" abi_version=3
    |}]

(* {1 The observation-descriptor table: derived offset, correct content} *)

let two_values_case =
  {
    case with
    Manifest.expected = [ 42L; 7L ];
    observations = [ { Manifest.obs_vaddr = 0x40020000L; width = 4; sign_extend = true } ];
  }

let%expect_test "one observation: table offset, size, and field bytes" =
  let s = Manifest.serialize ~abi_version:Abi_v3.abi_version two_values_case in
  let n_segments = Abi.get_u16 s Abi.Manifest_off.n_segments in
  let n_expected = Abi.get_u16 s Abi.Manifest_off.n_expected in
  let obs_off = Abi_v3.obs_table_off ~n_segments in
  let obs_len = Abi_v3.obs_table_len ~n_expected in
  Fmt.pr "n_segments=%d n_expected=%d obs_table_off=%d obs_table_len=%d@." n_segments n_expected
    obs_off obs_len;
  (* [obs_table_off] for one segment is [120 + 40] = 160, the same value v1's
     own single-segment payload base always was - the table is inserted
     *before* the payload, not appended after the header a second time. *)
  Fmt.pr "vaddr=0x%Lx width=%d sign_extend=%b@."
    (Abi.get_u64 s (obs_off + Abi_v3.Obs_descriptor_off.vaddr))
    (Abi.get_u8 s (obs_off + Abi_v3.Obs_descriptor_off.width))
    (Abi.get_u8 s (obs_off + Abi_v3.Obs_descriptor_off.flags) land Abi_v3.sign_extend_flag <> 0);
  (* The reserved tail of the one descriptor, and the payload that follows it,
     stay zero/correct - the table did not silently overwrite anything past
     itself. *)
  let reserved_zero =
    let ok = ref true in
    for i = 0 to Abi_v3.Obs_descriptor_off.reserved_len - 1 do
      if Abi.get_u8 s (obs_off + Abi_v3.Obs_descriptor_off.reserved + i) <> 0 then ok := false
    done;
    !ok
  in
  Fmt.pr "reserved zero=%b@." reserved_zero;
  let payload_off = Manifest.payload_off_of s 0 in
  Fmt.pr "segment 0 payload_off=%Ld (>= obs_table_off + obs_table_len = %d): %b@." payload_off
    (obs_off + obs_len)
    (Int64.to_int payload_off >= obs_off + obs_len);
  [%expect
    {|
    n_segments=1 n_expected=2 obs_table_off=160 obs_table_len=16
    vaddr=0x40020000 width=4 sign_extend=true
    reserved zero=true
    segment 0 payload_off=176 (>= obs_table_off + obs_table_len = 176): true
    |}]

(* {1 The minimum-size formula, checked against the same case} *)

let%expect_test "manifest_min_size matches the actual header+segments+table prefix" =
  let s = Manifest.serialize ~abi_version:Abi_v3.abi_version two_values_case in
  let n_segments = Abi.get_u16 s Abi.Manifest_off.n_segments in
  let n_expected = Abi.get_u16 s Abi.Manifest_off.n_expected in
  let min_size = Abi_v3.manifest_min_size ~n_segments ~n_expected in
  Fmt.pr "min_size=%d actual_payload_start=%Ld@." min_size (Manifest.payload_off_of s 0);
  [%expect {| min_size=176 actual_payload_start=176 |}]

(* {1 Three observations: descriptors packed contiguously, in list order} *)

let three_values_case =
  {
    case with
    Manifest.expected = [ 1L; 2L; 3L; 4L ];
    observations =
      [
        { Manifest.obs_vaddr = 0x1000L; width = 1; sign_extend = false };
        { Manifest.obs_vaddr = 0x2000L; width = 2; sign_extend = false };
        { Manifest.obs_vaddr = 0x3000L; width = 8; sign_extend = true };
      ];
  }

let%expect_test "three observations: contiguous 16-byte descriptors in order" =
  let s = Manifest.serialize ~abi_version:Abi_v3.abi_version three_values_case in
  let n_segments = Abi.get_u16 s Abi.Manifest_off.n_segments in
  let obs_off = Abi_v3.obs_table_off ~n_segments in
  for i = 0 to 2 do
    let d = obs_off + (Abi_v3.obs_descriptor_size * i) in
    Fmt.pr "[%d] vaddr=0x%Lx width=%d@." i
      (Abi.get_u64 s (d + Abi_v3.Obs_descriptor_off.vaddr))
      (Abi.get_u8 s (d + Abi_v3.Obs_descriptor_off.width))
  done;
  [%expect {|
    [0] vaddr=0x1000 width=1
    [1] vaddr=0x2000 width=2
    [2] vaddr=0x3000 width=8
    |}]
