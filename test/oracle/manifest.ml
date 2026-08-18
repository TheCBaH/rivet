(* The ABI v1 manifest writer (asm/docs/exec-abi-v1.md §8, §10.1).

   Two jobs, and the second one shapes the interface. The first is to serialize
   a bound image into the canonical wire form the helpers accept. The second is
   to produce the malformed inputs §16.1 requires - every one of the 42 manifest
   subcodes needs a file that violates exactly one rule - and that is why
   [serialize] is deliberately a *dumb* writer: it does not validate what it is
   given, and [patch] can overwrite any field afterwards.

   A writer that rejected out-of-policy descriptions would be unable to build
   the conformance corpus at all, and a writer that silently repaired them would
   be worse: the case would still run, still pass, and prove nothing about the
   helper's validator. Correctness of the *canonical* output is established by
   the round trip against the reference oracle, not by self-checking here. *)

type segment = {
  vaddr : int64;
  init : string;  (** payload bytes; the empty string means a pure zero-fill segment *)
  zero_len : int64;
  align : int;
  perms : int;  (** bit0 R, bit1 W, bit2 X *)
}

(* ABI v3 (exec-abi-v3.md §2): one entry per observation beyond slot 0 (the
   return register, which needs none). [List.length observations] is meant to
   equal [List.length expected - 1] once [expected] has more than one entry -
   a caller-side invariant, not one this writer checks, per this module's own
   "dumb writer" philosophy above. *)
type observation = { obs_vaddr : int64; width : int;  (** 1, 2, 4, or 8 *) sign_extend : bool }

type t = {
  profile : Abi.profile;
  case_id : int;
  entry_addr : int64;
  result_addr : int64;
  stack_size : int;
  timeout_ms : int;
  expected : int64 list;
  segments : segment list;
  observations : observation list;
      (** empty for every v1/v2 manifest and every single-value v3 one *)
}

let align_up_8 n = (n + 7) land lnot 7

(* {1 Canonical construction} *)

(* A single executable segment holding [code] at the profile's normative code
   address, entered at its first byte: the shape M1.6 binds and the base every
   conformance case mutates one field of. *)
let code_segment profile code =
  {
    vaddr = Abi.code_addr profile;
    init = code;
    zero_len = 0L;
    align = 16;
    perms = Abi.perm_r lor Abi.perm_x;
  }

let single_code ?(case_id = 1) ?(stack_size = 16 * 1024) ?(timeout_ms = 1000) ?(expected = [ 42L ])
    ?(observations = []) profile code =
  {
    profile;
    case_id;
    entry_addr = Abi.code_addr profile;
    result_addr = Abi.result_addr profile;
    stack_size;
    timeout_ms;
    expected;
    segments = [ code_segment profile code ];
    observations;
  }

(* {1 Serialization} *)

(* §10.1: descriptors with init_len > 0 carry payloads tightly packed in
   descriptor order at 8-byte alignment; descriptors with init_len = 0 are
   skipped and carry payload_off = 0. The offsets are computed here rather than
   supplied by the caller so that "canonical" has one definition - a case that
   wants a non-canonical offset patches it.

   [base] is shifted past the observation-descriptor table (exec-abi-v3.md
   §2) when [n_expected > 1]; at [n_expected = 1] the table is empty and this
   is exactly v1/v2's [manifest_header_size + segment_descriptor_size × n]. *)
let payload_offsets t =
  let n_expected = List.length t.expected in
  let base =
    Abi.manifest_header_size
    + (Abi.segment_descriptor_size * List.length t.segments)
    + (Abi_v3.obs_descriptor_size * max 0 (n_expected - 1))
  in
  let rec go cursor = function
    | [] -> ([], cursor)
    | s :: rest ->
        let len = String.length s.init in
        if len = 0 then
          let offs, e = go cursor rest in
          (0 :: offs, e)
        else
          let off = align_up_8 cursor in
          let offs, e = go (off + len) rest in
          (off :: offs, e)
  in
  go base t.segments

let serialize ?(abi_version = Abi.abi_version) t =
  let n = List.length t.segments in
  let n_expected = List.length t.expected in
  let offsets, payload_end = payload_offsets t in
  let total = align_up_8 payload_end in
  let b = Bytes.make total '\000' in
  Abi.set_string b Abi.Manifest_off.magic (Abi_v3.manifest_magic_for_version abi_version);
  Abi.set_u16 b Abi.Manifest_off.abi_version abi_version;
  if abi_version = Abi.abi_version && not (List.mem t.profile Abi.v1_profiles) then
    invalid_arg "Manifest.serialize: RISC-V profiles require ABI v2";
  if abi_version <> Abi_v3.abi_version && n_expected <> 1 then
    invalid_arg "Manifest.serialize: more than one expected value requires ABI v3";
  Abi.set_u16 b Abi.Manifest_off.profile_id (Abi.profile_id t.profile);
  Abi.set_u32 b Abi.Manifest_off.total_len total;
  Abi.set_u32 b Abi.Manifest_off.case_id t.case_id;
  Abi.set_u16 b Abi.Manifest_off.n_segments n;
  Abi.set_u16 b Abi.Manifest_off.n_expected n_expected;
  Abi.set_u64 b Abi.Manifest_off.entry_addr t.entry_addr;
  Abi.set_u64 b Abi.Manifest_off.result_addr t.result_addr;
  Abi.set_u32 b Abi.Manifest_off.result_size Abi.result_record_size;
  Abi.set_u32 b Abi.Manifest_off.stack_size t.stack_size;
  Abi.set_u32 b Abi.Manifest_off.timeout_ms t.timeout_ms;
  List.iteri (fun i v -> Abi.set_u64 b (Abi.Manifest_off.expected + (8 * i)) v) t.expected;
  (* The observation-descriptor table (exec-abi-v3.md §2), at the derived
     offset [Abi_v3.obs_table_off ~n_segments:n] - empty when [n_expected = 1],
     which is exactly what makes a v3 manifest with one expected value
     byte-shape-identical to a v1/v2 one. *)
  let obs_base = Abi_v3.obs_table_off ~n_segments:n in
  List.iteri
    (fun i (o : observation) ->
      let d = obs_base + (Abi_v3.obs_descriptor_size * i) in
      Abi.set_u64 b (d + Abi_v3.Obs_descriptor_off.vaddr) o.obs_vaddr;
      Abi.set_u8 b (d + Abi_v3.Obs_descriptor_off.width) o.width;
      Abi.set_u8 b
        (d + Abi_v3.Obs_descriptor_off.flags)
        (if o.sign_extend then Abi_v3.sign_extend_flag else 0))
    t.observations;
  List.iteri
    (fun i s ->
      let d = Abi.manifest_header_size + (Abi.segment_descriptor_size * i) in
      let off = List.nth offsets i in
      Abi.set_u64 b (d + Abi.Descriptor_off.vaddr) s.vaddr;
      Abi.set_u64 b (d + Abi.Descriptor_off.init_len) (Int64.of_int (String.length s.init));
      Abi.set_u64 b (d + Abi.Descriptor_off.zero_len) s.zero_len;
      Abi.set_u64 b (d + Abi.Descriptor_off.payload_off) (Int64.of_int off);
      Abi.set_u32 b (d + Abi.Descriptor_off.align) s.align;
      Abi.set_u8 b (d + Abi.Descriptor_off.perms) s.perms;
      if off <> 0 then Abi.set_string b off s.init)
    t.segments;
  Bytes.to_string b

(* {1 Field offsets, for cases that mutate a serialized manifest} *)

let descriptor_off i = Abi.manifest_header_size + (Abi.segment_descriptor_size * i)

(* The payload offset a canonical serialization assigned to descriptor [i], read
   back off the wire. A case that shifts payloads around needs to know where
   they started. *)
let payload_off_of s i = Abi.get_u64 s (descriptor_off i + Abi.Descriptor_off.payload_off)

(* {1 Mutation}

   Everything below returns a new string. A conformance case reads as
   "canonical manifest, then exactly this one difference", which is what makes
   it evidence for a specific subcode rather than for "something was wrong". *)

let patch_bytes s off bytes =
  let b = Bytes.of_string s in
  Bytes.blit_string bytes 0 b off (String.length bytes);
  Bytes.to_string b

let patch_u8 s off v =
  let b = Bytes.of_string s in
  Abi.set_u8 b off v;
  Bytes.to_string b

let patch_u16 s off v =
  let b = Bytes.of_string s in
  Abi.set_u16 b off v;
  Bytes.to_string b

(* u32 and u64 patches take int64 so that a case can store a value no native
   int holds - the point of the 32-bit high-bits-set and address-overflow cases
   is precisely that they are out of range. *)
let patch_u32 s off v =
  let b = Bytes.of_string s in
  Bytes.set_int32_le b off (Int64.to_int32 v);
  Bytes.to_string b

let patch_u64 s off v =
  let b = Bytes.of_string s in
  Abi.set_u64 b off v;
  Bytes.to_string b

let truncate_to s n = String.sub s 0 n
let append s extra = s ^ extra

(* Rewriting total_len is what almost every structural case needs after changing
   the file length, so that the change under test is the structural one and not
   an incidental total_len mismatch. *)
let set_total_len s = patch_u32 s Abi.Manifest_off.total_len (Int64.of_int (String.length s))
