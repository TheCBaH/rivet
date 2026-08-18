(* A direct, standalone proof that a real, generated v3 helper implements
   the multi-value result-block protocol (.ai/asm_plan.md M4 Phase 3.4's
   exit criterion: "x86_64's generated v3 helper passes a real QEMU run"
   with "a v3 multi-value result", proven before any real fixture uses it).

   Deliberately not routed through [exec.ml]'s [cases()] directory scan -
   there is no real v3 fixture yet (that is Phase 5/6) - so this builds a
   manifest by hand: a tiny hand-assembled machine-code segment that writes
   a known constant to a data segment and returns 42, plus one observation
   descriptor pointing at that constant. A passing run proves stage 4b's
   validation, the post-return observation load, and the element-wise
   passed/failed decision all work end to end under real QEMU - the same
   surface Phase 4 will wire [exec.ml] to reach generically. *)

open Asm_oracle
open Asm_oracle_run

(* x86-64: movabs $0x40020000,%rax ; movl $0x11223344,(%rax) ; movl $42,%eax ; ret *)
let code =
  "\x48\xb8\x00\x00\x02\x40\x00\x00\x00\x00\xc7\x00\x44\x33\x22\x11\xb8\x2a\x00\x00\x00\xc3"

let observed_value = 0x11223344L

let manifest =
  {
    (Manifest.single_code ~expected:[ 42L; observed_value ] Abi.X86_64 code) with
    Manifest.segments =
      [
        Manifest.code_segment Abi.X86_64 code;
        {
          Manifest.vaddr = Abi.data_addr Abi.X86_64;
          init = "";
          zero_len = 4L;
          align = 16;
          perms = Abi.perm_r lor Abi.perm_w;
        };
      ];
    observations =
      [ { Manifest.obs_vaddr = Abi.data_addr Abi.X86_64; width = 4; sign_extend = false } ];
  }

let () =
  match Qemu_user.available_profiles ~abi_version:Abi_v3.abi_version () with
  | [] ->
      Fmt.pr "abi-v3-smoke: no v3 helpers built - run tools/asm-helpers.sh's v3 path first@.";
      exit 1
  | profiles when not (List.mem Abi.X86_64 profiles) ->
      Fmt.pr "abi-v3-smoke: no v3 x86_64 helper built@.";
      exit 1
  | _ ->
      let serialized = Manifest.serialize ~abi_version:Abi_v3.abi_version manifest in
      let o =
        Qemu_user.run ~abi_version:Abi_v3.abi_version ~profile:Abi.X86_64 ~manifest:serialized
          ~expected_case_id:1 ()
      in
      Fmt.pr "%a@." Qemu_user.pp o;
      let ok =
        match o.Qemu_user.termination with
        | Qemu_user.Completed -> (
            match o.Qemu_user.record with
            | Some (Record.Valid r) ->
                r.Record.record_state = Abi.Returned
                && r.Record.status = Abi.Passed
                && r.Record.values = [| 42L; observed_value |]
                && r.Record.expected = [| 42L; observed_value |]
            | _ -> false)
        | _ -> false
      in
      if ok then (
        Fmt.pr "abi-v3-smoke: ok - multi-value result correct@.";
        exit 0)
      else (
        Fmt.pr "abi-v3-smoke: FAIL@.";
        exit 1)
