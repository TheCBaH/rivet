(* The execution ABI conformance suite (asm/docs/exec-abi-v1.md §16 and
   asm/docs/exec-abi-v2.md).

   Drives each built helper under its emulator and checks the *exact* outcome:
   the exact subcode for a malformed manifest, the exact termination for a trap
   or a timeout, the exact committed status for a run that returned.

   Two properties this suite exists to establish, neither of which follows from
   the checks merely existing:

   - Reachability. §14.2 promises every subcode carries a case proving it can
     actually be observed, run against the *final* precedence table. A check
     that is shadowed by an earlier rule is indistinguishable from a check that
     is absent, and that is how revision 9's result/stack collision and revision
     11's four packing-shadowed payload subcodes both survived review. The
     report at the end walks Abi.all_subcodes and fails on any subcode that
     neither a case observed nor the exception list accounts for - with a
     recorded reason, so "unreachable" is a reviewed claim rather than a
     silence.

   - That the gate can fail. Guardrail 3: an execution gate that has not
     demonstrated it can fail proves nothing. The deliberately wrong encoding,
     the trap snippet, the timeout snippet and the guard-page overflow are all
     controls whose purpose is to *not* pass.

   Snippet bytes are in memory order and quoted from the frozen tables. *)

open Asm_oracle
open Asm_oracle_run

let abi_version =
  match Sys.getenv_opt "ASM_ABI_VERSION" with
  | None -> Abi.abi_version
  | Some "1" -> 1
  | Some "2" -> Abi_v2.abi_version
  | Some value -> invalid_arg ("ASM_ABI_VERSION must be 1 or 2, got " ^ value)

let serialize t = Manifest.serialize ~abi_version t

let () =
  let probe = serialize (Manifest.single_code Abi.X86_32 "") in
  let encoded = Abi.get_u16 probe Abi.Manifest_off.abi_version in
  if encoded <> abi_version then
    failwith
      (Printf.sprintf "manifest serializer wrote ABI v%d while v%d was requested" encoded
         abi_version)

(* {1 Reference snippets}

   Assembled by this project, from the ASTs in {!Snippet_corpus.Snippet_ast}.

   This suite used to read frozen bytes and reach no part of the assembler, so
   that a broken assembler could not make it pass. That guarantee is gone by
   choice: §16.3 was being stated twice, and once the assembler could build all
   36 the second statement was duplication rather than evidence. What is left
   in {!Snippet_bytes} is the record shape, the stack size and the M1.6 control
   pair - not the snippets.

   The suite therefore tests two things at once now, and the reports keep them
   apart: a snippet that will not assemble is a corpus failure named as such
   below, while a snippet that runs and gives the wrong verdict is a helper or
   an encoding failure, which is what the case names describe.

   What this suite depends on is the assembler's encoding half only - lower,
   encode, plan, bind. Not the driver, and not the front end: {!Asm_syntax} is
   absent from the linked binary, so no lexer, grammar or token type is reached
   to produce §16.3. *)

let stack_16k = Snippet_bytes.stack_16k
let snippets_for = Snippet_corpus.Snippet_ast.for_profile

(* {1 Expectations} *)

type expect =
  | Passed of int64
  | Failed of int64
  | Guest_fault
  | Guest_timeout
  | Error of string  (** a subcode key, e.g. "manifest.bad_magic" *)

let case_id = 7
let describe_actual (o : Qemu_user.t) = Fmt.str "%a" Qemu_user.pp o

let matches expect (o : Qemu_user.t) =
  match (expect, o.Qemu_user.termination, o.Qemu_user.record) with
  | Passed v, Qemu_user.Completed, Some (Record.Valid r) ->
      r.Record.record_state = Abi.Returned
      && r.Record.status = Abi.Passed && r.Record.values = [| v |] && r.Record.expected = [| v |]
  | Failed v, Qemu_user.Completed, Some (Record.Valid r) ->
      r.Record.record_state = Abi.Returned
      && r.Record.status = Abi.Failed && r.Record.values = [| v |]
  | Guest_fault, Qemu_user.Fault, Some (Record.Valid r) ->
      (* §16.3 requires both halves: the wait signal *and* the recovered phase.
         Either alone would accept a process that died for some other reason. *)
      r.Record.record_state = Abi.Validated && r.Record.status = Abi.Running
  | Guest_timeout, Qemu_user.Timeout, Some (Record.Valid r) ->
      r.Record.record_state = Abi.Validated && r.Record.status = Abi.Running
  | Error key, Qemu_user.Runner_error cls, _ ->
      let want = Abi.find_subcode ~abi_version key in
      want.Abi.cls = cls && Qemu_user.observed_error o = Some (cls, want.Abi.code)
  | _ -> false

(* {1 Manifest corpus}

   Every case is the canonical manifest with exactly one difference, so what it
   establishes is attributable to that difference. The base is deliberately the
   same shape M1.6 binds: one executable segment at the normative code address,
   entered at its first byte. *)

let base profile code =
  Manifest.single_code ~case_id ~stack_size:stack_16k ~timeout_ms:1000 profile code

(* Named [canonical] rather than [serialize] because the case lists open
   Manifest, which would otherwise shadow it with the raw writer. *)
let canonical profile code = serialize (base profile code)

let two_segments profile code =
  (* Two payload-bearing segments a page apart, for the overlap and
     segment/segment collision cases. *)
  serialize
    {
      (base profile code) with
      Manifest.segments =
        [
          Manifest.code_segment profile code;
          {
            Manifest.vaddr = Abi.rodata_addr profile;
            init = "\x01\x02\x03\x04";
            zero_len = 0L;
            align = 8;
            perms = Abi.perm_r;
          };
        ];
    }

(* A manifest whose first payload is four bytes long, so canonical packing has
   to insert a four-byte alignment gap before the second one. The code segment
   is described *second* on purpose: §10.1 requires payloads to follow
   descriptor order but explicitly not vaddr order, and building the case this
   way exercises that rather than assuming it.

   It exists because deriving a gap from the return-42 payload does not work
   across profiles: that payload is 23 bytes on x86-64 and 32 on ARM, so the
   final alignment padding is three bytes on one and none at all on the other.
   A case whose violation depends on the length of an unrelated snippet is not
   testing what it claims to. *)
let gapped profile code =
  serialize
    {
      (base profile code) with
      Manifest.segments =
        [
          {
            Manifest.vaddr = Abi.rodata_addr profile;
            init = "\x01\x02\x03\x04";
            zero_len = 0L;
            align = 8;
            perms = Abi.perm_r;
          };
          Manifest.code_segment profile code;
        ];
    }

(* The first byte of that gap: the descriptor table ends at 120 + 40 x 2, the
   first payload occupies four bytes from there, and the gap runs to the next
   eight-byte boundary. *)
let gap_offset = Abi.manifest_header_size + (2 * Abi.segment_descriptor_size) + 4
let d0 = Manifest.descriptor_off 0
let d1 = Manifest.descriptor_off 1

type case = {
  name : string;
  expect : expect;
  only : Abi.profile list option;
      (** [None] means every profile. A case is profile-scoped only when the *rule* is: §10.3's
          entry alignment and the 32-bit address-width rule have no counterpart on the other
          profiles, and a case that ran everywhere would be asserting a different thing on each. *)
  build : Abi.profile -> Snippet_bytes.t -> Qemu_user.t;
}

let run_manifest ?input ?extra_args profile manifest =
  Qemu_user.run ~abi_version ?input ?extra_args ~profile ~manifest ~expected_case_id:case_id ()

let m ?only name expect f =
  { name; expect; only; build = (fun profile s -> run_manifest profile (f profile s)) }

let applies profile c = match c.only with None -> true | Some ps -> List.mem profile ps
let bits32 = [ Abi.X86_32; Abi.Arm; Abi.Riscv32 ]
let bits64 = [ Abi.X86_64; Abi.Aarch64; Abi.Riscv64 ]

(* §10.3 gives only these two an instruction-address rule for the entry. *)
let has_entry_rule = [ Abi.Arm; Abi.Aarch64; Abi.Riscv32; Abi.Riscv64 ]

(* Positive controls and the deliberately-wrong ones. *)
let behavioral_cases =
  [
    m "return42" (Passed 42L) (fun p s -> canonical p s.return42);
    (* Guardrail 3: the gate must be able to fail. A random bit flip can leave
       code that still returns 42, so the mutation is the constant-materializing
       immediate and nothing else. *)
    m "induced-wrong-encoding" (Failed 41L) (fun p s -> canonical p s.return41);
    m "entry-sp-alignment" (Passed 42L) (fun p s -> canonical p s.sp_align);
    m "stack-fully-consumed" (Passed 42L) (fun p s -> canonical p s.stack_full);
    m "stack-overflow-hits-guard" Guest_fault (fun p s -> canonical p s.stack_over);
    m "deliberate-trap" Guest_fault (fun p s -> canonical p s.trap);
    m "guest-timeout" Guest_timeout (fun p s -> canonical p s.spin);
    m "protocol-sp-corrupt" (Error "protocol.stack_pointer_corrupt") (fun p s ->
        canonical p s.sp_corrupt);
    m "protocol-callee-saved" (Error "protocol.callee_saved_clobbered") (fun p s ->
        canonical p s.callee_clobber);
  ]

(* The staged-bounds and fixed-header cases (§10 stages 1-3). *)
let header_cases =
  let open Manifest in
  [
    m "empty-file" (Error "manifest.size_below_minimum") (fun _ _ -> "");
    m "truncated-header" (Error "manifest.size_below_minimum") (fun p s ->
        truncate_to (canonical p s.return42) 119);
    m "oversized-file" (Error "manifest.size_above_maximum") (fun p s ->
        let base = canonical p s.return42 in
        set_total_len
          (append base (String.make (Abi.manifest_size_max + 8 - String.length base) '\000')));
    m "bad-magic" (Error "manifest.bad_magic") (fun p s ->
        patch_bytes (canonical p s.return42) 0 "X");
    m "bad-abi-version" (Error "manifest.bad_abi_version") (fun p s ->
        patch_u16 (canonical p s.return42) Abi.Manifest_off.abi_version
          (if abi_version = 1 then 2 else 1));
    m "bad-profile-id" (Error "manifest.bad_profile_id") (fun p s ->
        patch_u16 (canonical p s.return42) Abi.Manifest_off.profile_id 9);
    (* total_len overstating the real st_size: the case that would have let an
       attacker-controlled length be used as a memory-safety bound. *)
    m "total-len-overstates" (Error "manifest.total_len_mismatch") (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.total_len
          (Int64.of_int Abi.manifest_size_max));
    m "n-segments-zero" (Error "manifest.n_segments_range") (fun p s ->
        patch_u16 (canonical p s.return42) Abi.Manifest_off.n_segments 0);
    m "n-segments-nine" (Error "manifest.n_segments_range") (fun p s ->
        patch_u16 (canonical p s.return42) Abi.Manifest_off.n_segments 9);
    m "n-expected-two" (Error "manifest.n_expected_range") (fun p s ->
        patch_u16 (canonical p s.return42) Abi.Manifest_off.n_expected 2);
    m "result-size-wrong" (Error "manifest.result_size_wrong") (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.result_size 128L);
    m "timeout-too-small" (Error "manifest.timeout_range") (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.timeout_ms
          (Int64.of_int (Abi.timeout_ms_min - 1)));
    m "timeout-too-large" (Error "manifest.timeout_range") (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.timeout_ms
          (Int64.of_int (Abi.timeout_ms_max + 1)));
    m "header-reserved-nonzero" (Error "manifest.header_reserved_nonzero") (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.reserved 1L);
    m "expected-tail-nonzero" (Error "manifest.expected_tail_nonzero") (fun p s ->
        patch_u64 (canonical p s.return42) (Abi.Manifest_off.expected + 8) 1L);
  ]

(* Descriptor and payload bounds, canonical packing, and the resource caps
   (§10 stages 4-9). *)
let payload_cases =
  let open Manifest in
  [
    m "descriptor-table-overruns" (Error "manifest.descriptor_table_overruns") (fun p s ->
        patch_u16 (canonical p s.return42) Abi.Manifest_off.n_segments 8);
    m "payload-out-of-file" (Error "manifest.payload_overruns") (fun p s ->
        let b = canonical p s.return42 in
        patch_u64 b (d0 + Abi.Descriptor_off.payload_off) (Int64.of_int (String.length b)));
    (* The file grows by one aligned slot first, so the shifted payload is
       still in-file: otherwise stage 5 fires and the misalignment subcode is
       never reached. Whether it happens to fit without that depends on the
       length of the snippet, which has nothing to do with the rule. *)
    m "payload-misaligned" (Error "manifest.payload_misaligned") (fun p s ->
        let b = set_total_len (append (canonical p s.return42) (String.make 8 (Char.chr 0))) in
        patch_u64 b (d0 + Abi.Descriptor_off.payload_off) (Int64.add (payload_off_of b 0) 1L));
    m "payload-in-metadata" (Error "manifest.payload_in_metadata") (fun p s ->
        patch_u64 (canonical p s.return42) (d0 + Abi.Descriptor_off.payload_off) 8L);
    m "payload-pairing" (Error "manifest.payload_pairing") (fun p s ->
        patch_u64 (canonical p s.return42) (d0 + Abi.Descriptor_off.init_len) 0L);
    m "payload-overlap" (Error "manifest.payload_overlap") (fun p s ->
        let b = two_segments p s.return42 in
        patch_u64 b (d1 + Abi.Descriptor_off.payload_off) (payload_off_of b 0));
    (* The file grows by one aligned slot so that the shifted payload is still
       in-file: otherwise stage 5 would fire first and the packing subcode would
       never be reached. *)
    m "packing-not-tight" (Error "manifest.packing_not_tight") (fun p s ->
        let b = set_total_len (append (canonical p s.return42) (String.make 8 '\000')) in
        patch_u64 b (d0 + Abi.Descriptor_off.payload_off) (Int64.add (payload_off_of b 0) 8L));
    m "packing-gap-nonzero" (Error "manifest.packing_gap_nonzero") (fun p s ->
        patch_bytes (gapped p s.return42) gap_offset "\x01");
    m "trailing-bytes" (Error "manifest.trailing_bytes") (fun p s ->
        set_total_len (append (canonical p s.return42) (String.make 8 '\000')));
    (* The file has to be genuinely large enough, or stage 5 shadows stage 9. *)
    m "init-len-too-large" (Error "manifest.init_len_too_large") (fun p _ ->
        Manifest.serialize ~abi_version (base p (String.make (Abi.init_len_max + 1) '\x90')));
    m "zero-len-too-large" (Error "manifest.zero_len_too_large") (fun p s ->
        patch_u64 (canonical p s.return42)
          (d0 + Abi.Descriptor_off.zero_len)
          (Int64.of_int (Abi.zero_len_max + 1)));
  ]

(* Geometry, collisions and cross-region well-formedness (§10.2). *)
let geometry_cases =
  let open Manifest in
  let vaddr0 = d0 + Abi.Descriptor_off.vaddr in
  [
    m "segment-empty" (Error "manifest.segment_empty") (fun p _ ->
        Manifest.serialize ~abi_version (base p ""));
    m "align-not-power-of-two" (Error "manifest.align_not_power_of_two") (fun p s ->
        patch_u32 (canonical p s.return42) (d0 + Abi.Descriptor_off.align) 3L);
    m "vaddr-misaligned" (Error "manifest.vaddr_misaligned") (fun p s ->
        patch_u64 (canonical p s.return42) vaddr0 (Int64.add (Abi.code_addr p) 1L));
    m "perms-unknown-bits" (Error "manifest.perms_unknown_bits") (fun p s ->
        patch_u8 (canonical p s.return42) (d0 + Abi.Descriptor_off.perms) 8);
    m "perms-write-execute" (Error "manifest.perms_write_execute") (fun p s ->
        patch_u8 (canonical p s.return42) (d0 + Abi.Descriptor_off.perms) 7);
    m "address-overflow" (Error "manifest.address_overflow") (fun p s ->
        patch_u64 (canonical p s.return42) vaddr0 0xFFFFFFFFFFFFF000L);
    m "outside-window" (Error "manifest.outside_window") (fun p s ->
        patch_u64 (canonical p s.return42) vaddr0 (Int64.add (Abi.window_base p) Abi.window_size));
    m "collide-segment-result" (Error "manifest.collide_segment_result") (fun p s ->
        patch_u64 (canonical p s.return42) vaddr0 (Abi.result_addr p));
    m "collide-segment-stack" (Error "manifest.collide_segment_stack") (fun p s ->
        patch_u64 (canonical p s.return42) vaddr0 (Abi.stack_start p));
    m "collide-segment-guard" (Error "manifest.collide_segment_guard") (fun p s ->
        patch_u64 (canonical p s.return42) vaddr0 (Abi.guard_addr p));
    (* Collisions precede the normative-address rules deliberately: with the
       opposite order this manifest would always fail "result_addr equals its
       normative address" first and this subcode could never be observed. *)
    m "collide-result-stack" (Error "manifest.collide_result_stack") (fun p s ->
        patch_u64 (canonical p s.return42) Abi.Manifest_off.result_addr (Abi.stack_start p));
    m "collide-segment-segment" (Error "manifest.collide_segment_segment") (fun p s ->
        let b = two_segments p s.return42 in
        patch_u64 b (d1 + Abi.Descriptor_off.vaddr) (Int64.add (Abi.code_addr p) 8L));
    m "entry-outside-x-segment" (Error "manifest.entry_not_in_x_segment") (fun p s ->
        patch_u64 (canonical p s.return42) Abi.Manifest_off.entry_addr
          (Int64.add (Abi.code_addr p) 1000L));
    (* §10.2 fixes no order *within* cross-region well-formedness, and "first
       failure wins" needs one. The helpers check the 32-bit address rule first,
       because every input with a high word set is also caught by a later rule -
       an entry beyond 2^32 is in no segment - so with subcode 42 last it would
       be a check that exists and can never fire. *)
    m ~only:bits32 "entry-address-high-bits" (Error "manifest.address_high_bits_set") (fun p s ->
        patch_u64 (canonical p s.return42) Abi.Manifest_off.entry_addr
          (Int64.add (Abi.code_addr p) 0x100000000L));
    (* §10.3. The address is inside the segment's initialized range, so it
       reaches the profile rule instead of being caught as an entry outside an
       executable segment. *)
    m ~only:has_entry_rule "entry-violates-profile-rule" (Error "manifest.entry_profile_rule")
      (fun p s ->
        patch_u64 (canonical p s.return42) Abi.Manifest_off.entry_addr
          (Int64.add (Abi.code_addr p) 1L));
    m "result-addr-misaligned" (Error "manifest.result_addr_misaligned") (fun p s ->
        patch_u64 (canonical p s.return42) Abi.Manifest_off.result_addr
          (Int64.add (Abi.result_addr p) 1L));
    m "result-addr-not-normative" (Error "manifest.result_addr_not_normative") (fun p s ->
        patch_u64 (canonical p s.return42) Abi.Manifest_off.result_addr
          (Int64.add (Abi.result_addr p) 0x1000L));
    (* §16.1's result-address overflow, on both address widths. Both are page
       -aligned, so both reach the normative-address rule rather than the
       alignment one, and what they exercise is that the helper's result-page
       range saturates instead of wrapping: a wrapped range would appear to sit
       at address zero and could collide with a segment nowhere near it.
       The 64-bit value is only meaningful on a 64-bit profile - on a 32-bit one
       the address-width rule catches it first, which is the case above. *)
    m "result-addr-overflow-32" (Error "manifest.result_addr_not_normative") (fun p s ->
        patch_u64 (canonical p s.return42) Abi.Manifest_off.result_addr 0xFFFFF000L);
    m ~only:bits64 "result-addr-overflow-64" (Error "manifest.result_addr_not_normative")
      (fun p s ->
        patch_u64 (canonical p s.return42) Abi.Manifest_off.result_addr 0xFFFFFFFFFFFFF000L);
    m "stack-size-not-page-multiple" (Error "manifest.stack_size_invalid") (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.stack_size 0x800L);
    m "stack-size-below-minimum" (Error "manifest.stack_size_invalid") (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.stack_size 0L);
    m "stack-size-above-maximum" (Error "manifest.stack_size_invalid") (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.stack_size
          (Int64.of_int (Abi.stack_size_max + Abi.page_size)));
  ]

(* Boundaries, and the multi-violation input that proves precedence is
   deterministic rather than merely defined. *)
let boundary_cases =
  let open Manifest in
  [
    m "stack-size-minimum" (Passed 42L) (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.stack_size
          (Int64.of_int Abi.stack_size_min));
    m "stack-size-maximum" (Passed 42L) (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.stack_size
          (Int64.of_int Abi.stack_size_max));
    m "timeout-minimum" (Passed 42L) (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.timeout_ms
          (Int64.of_int Abi.timeout_ms_min));
    m "timeout-maximum" (Passed 42L) (fun p s ->
        patch_u32 (canonical p s.return42) Abi.Manifest_off.timeout_ms
          (Int64.of_int Abi.timeout_ms_max));
    (* Three violations at once - an oversized zero_len, which also puts the
       segment outside the window, together with a bad magic. The fixed header
       is the ABI discriminator and settles first, so the magic subcode wins;
       without stage 3 preceding stages 4-9 this file would be read as a v1
       layout and reported as a resource or geometry error instead. *)
    m "multi-violation-precedence" (Error "manifest.bad_magic") (fun p s ->
        let b = patch_u64 (canonical p s.return42) (d0 + Abi.Descriptor_off.zero_len) 0xFFFFFFFFL in
        patch_bytes b 0 "X");
  ]

(* M3's fixed BSS window (.ai/asm_plan.md §12; the M3 plan's §11): a second,
   pure zero-fill segment at [Abi_v2.bss_addr], with no payload bytes at all -
   exactly the shape [bind_image] produces for a NOBITS section. The address
   and its cap are not asserted in isolation; they are proven against the
   generic segment/collision machinery every other geometry case already
   exercises, which is what makes "fits" and "one byte over is rejected"
   evidence rather than restated arithmetic. Unrestricted by [~only]: the
   address is a pure function of the profile, so it is exercised under both
   ABI v1's four profiles and, in the v2 pass, RISC-V's two as well. *)
let bss_segment ~zero_len profile =
  {
    Manifest.vaddr = Abi_v2.bss_addr profile;
    init = "";
    zero_len;
    align = 4096;
    perms = Abi.perm_r lor Abi.perm_w;
  }

let with_bss ~zero_len profile code =
  serialize
    {
      (base profile code) with
      Manifest.segments = [ Manifest.code_segment profile code; bss_segment ~zero_len profile ];
    }

let bss_cases =
  [
    (* The whole remainder of the window above the largest permitted stack
       ([stack_start + stack_size_max]) - reaching exactly the window's own
       end. A passing run is itself the collision proof: any overlap with
       the stack, guard or result page would have failed it. *)
    m "bss-max-extent-binds" (Passed 42L) (fun p s ->
        with_bss ~zero_len:(Int64.of_int Abi_v2.bss_max_extent) p s.return42);
    (* One byte past the window's end - not a generic "somewhere outside",
       but the specific boundary [bss_addr + bss_max_extent] pins, so this
       is a check on the exact numbers rather than on the rule's existence
       (already covered by "outside-window" above). *)
    m "bss-one-byte-over-outside-window" (Error "manifest.outside_window") (fun p s ->
        with_bss ~zero_len:(Int64.add (Int64.of_int Abi_v2.bss_max_extent) 1L) p s.return42);
  ]

(* The io class, and the environment class through the helper's §14.2 test
   entry. Neither is reachable by a well-formed run, which is exactly why they
   need a deliberate mechanism rather than a hope. *)
let environment_cases =
  [
    {
      name = "input-missing";
      only = None;
      expect = Error "io.input_open";
      build = (fun p s -> run_manifest ~input:Qemu_user.Missing p (canonical p s.return42));
    };
    {
      name = "input-not-mappable";
      only = None;
      expect = Error "io.input_mmap";
      build = (fun p s -> run_manifest ~input:Qemu_user.Directory p (canonical p s.return42));
    };
    {
      name = "page-size-wrong";
      only = None;
      expect = Error "environment.page_size_wrong";
      build = (fun p s -> run_manifest ~extra_args:[ "pgsz-bad" ] p (canonical p s.return42));
    };
    {
      name = "page-size-absent";
      only = None;
      expect = Error "environment.page_size_absent";
      build = (fun p s -> run_manifest ~extra_args:[ "pgsz-abs" ] p (canonical p s.return42));
    };
  ]

let all_cases =
  behavioral_cases @ header_cases @ payload_cases @ geometry_cases @ boundary_cases @ bss_cases
  @ environment_cases

(* {1 Reachability exceptions}

   Every subcode not observed by a case must appear here with a reason. This is
   the honest form of §14.2's promise: a subcode whose condition cannot arise
   under ABI v1's own constants, or that only a failing syscall could produce,
   is not evidence of a missing test - but it must be a reviewed claim, not a
   gap nobody noticed. Anything absent from both lists fails the run. *)

type exemption = { subcode : string; reason : string; profiles : Abi.profile list option }

let exempt ?profiles subcode reason = { subcode; reason; profiles }

let exemptions =
  [
    (* Structural: the condition cannot arise given the frozen constants. The
       helper asserts the same fact at assembly time; see x86_64.s. *)
    exempt "manifest.stack_region_not_normative"
      "unreachable in v1: stack_start and guard_addr are normative constants and stack_size is \
       already fully constrained by subcode 40, so nothing is left for this rule to catch \
       (asserted at assembly time in the helper)";
    (* Per-profile: the rule itself is profile-dependent. *)
    exempt ~profiles:[ Abi.X86_32; Abi.X86_64 ] "manifest.entry_profile_rule"
      "§10.3 places no instruction-alignment restriction on x86 entries";
    exempt
      ~profiles:[ Abi.X86_64; Abi.Aarch64; Abi.Riscv64 ]
      "manifest.address_high_bits_set" "a 64-bit profile has no high half to reject";
    exempt
      ~profiles:[ Abi.X86_32; Abi.X86_64; Abi.Aarch64; Abi.Riscv32; Abi.Riscv64 ]
      "cache-sync.arm_cacheflush" "§12: only the ARM profile issues a cacheflush syscall";
    exempt ~profiles:[ Abi.Arm ] "cache-sync.arm_cacheflush"
      "no injection mechanism: qemu-user implements the ARM cacheflush syscall as an unconditional \
       success, and the manifest has no field that reaches it";
    exempt
      ~profiles:[ Abi.X86_32; Abi.X86_64; Abi.Arm; Abi.Riscv32; Abi.Riscv64 ]
      "cache-sync.aarch64_line_size" "§12: only AArch64 reads CTR_EL0";
    exempt ~profiles:[ Abi.Aarch64 ] "cache-sync.aarch64_line_size"
      "no injection mechanism: CTR_EL0 is emulator state, not manifest input";
    exempt
      ~profiles:[ Abi.X86_32; Abi.X86_64; Abi.Arm; Abi.Aarch64 ]
      "cache-sync.riscv_flush_icache" "ABI v2: only the RISC-V profiles issue riscv_flush_icache";
    exempt ~profiles:[ Abi.Riscv32; Abi.Riscv64 ] "cache-sync.riscv_flush_icache"
      "no injection mechanism: qemu-user implements the architecture syscall for valid mapped \
       ranges";
    (* Syscalls that cannot be made to fail from the host side once the run has
       got far enough to attempt them. §15.2 already scopes the exact-subcode
       guarantee to failures occurring after the control alias exists; these
       sit outside that scope or have no injection mechanism in v1. *)
    exempt "io.result_open" "no record can exist; the class is observed through the exit code";
    exempt "io.result_ftruncate" "no injection mechanism: ftruncate on a fresh O_EXCL file";
    exempt "io.input_fstat" "no injection mechanism: fstat on a successfully opened fd";
    exempt "mapping.window_reservation"
      "no record can exist; requires the guest address space to be occupied";
    exempt "mapping.control_alias"
      "no record can exist; requires mmap of a fresh 4 KiB file to fail";
    exempt "mapping.result_alias" "the mapping is inside our own validated reservation";
    exempt "mapping.stack_mapping" "the mapping is inside our own validated reservation";
    exempt "mapping.guard_mapping" "the mapping is inside our own validated reservation";
    exempt "mapping.segment_mapping" "the mapping is inside our own validated reservation";
    exempt "mapping.mprotect" "the range was just mapped by us with compatible permissions";
  ]

let exemption_for profile key =
  List.find_map
    (fun e ->
      if e.subcode = key && match e.profiles with None -> true | Some ps -> List.mem profile ps
      then Some e.reason
      else None)
    exemptions

(* {1 Driver} *)

let () =
  let profiles =
    let available = Qemu_user.available_profiles ~abi_version () in
    match Sys.getenv_opt "ASM_ABI_PROFILES" with
    | None -> available
    | Some names ->
        let wanted = String.split_on_char ',' names |> List.map String.trim in
        List.filter (fun p -> List.mem (Abi.profile_name p) wanted) available
  in
  if profiles = [] then (
    Fmt.pr "conform: no helpers built - run `make asm-helpers`@.";
    exit 1);
  let failures = ref 0 in
  let observed = Hashtbl.create 64 in
  List.iter
    (fun profile ->
      match snippets_for profile with
      | Error why ->
          (* A helper exists but the corpus cannot produce §16.3 for it, and
             [why] names the snippet and what stopped it. Reported, never
             silently skipped: running the eight that did assemble would report
             a missing encoding form as a conformance result. *)
          Fmt.pr "@.=== %s: reference snippets not assembled (%s) - FAIL@."
            (Abi.profile_name profile) why;
          incr failures
      | Ok s ->
          Fmt.pr "@.=== %s@." (Abi.profile_name profile);
          List.iter
            (fun line -> Fmt.pr "  ..   %s@." line)
            (Qemu_user.provenance ~abi_version profile);
          List.iter
            (fun c ->
              if applies profile c then (
                let o = c.build profile s in
                let ok = matches c.expect o in
                if not ok then incr failures;
                (match c.expect with
                | Error key -> Hashtbl.replace observed (Abi.profile_name profile, key) ()
                | _ -> ());
                Fmt.pr "  %-4s %-32s %s@." (if ok then "ok" else "FAIL") c.name (describe_actual o)))
            all_cases;
          (* §14.2's reachability promise, checked rather than asserted. *)
          List.iter
            (fun sc ->
              let key = Abi.key sc in
              if not (Hashtbl.mem observed (Abi.profile_name profile, key)) then
                match exemption_for profile key with
                | Some reason -> Fmt.pr "  --   %-32s exempt: %s@." key reason
                | None ->
                    incr failures;
                    Fmt.pr
                      "  FAIL %-32s promised by §14.2 but no case observes it and no exemption \
                       covers it@."
                      key)
            (Abi.all_subcodes_for_version abi_version))
    profiles;
  Fmt.pr "@.conform: ABI v%d, %d failures over %d profile(s)@." abi_version !failures
    (List.length profiles);
  if !failures > 0 then exit 1
