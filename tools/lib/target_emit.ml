let ( let* ) = Result.bind

let err detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v Tool_error.Validate detail)

(* {1 Shell rendering, fail-closed}

   Neither renderer escapes. A value that would need escaping is REJECTED
   instead, because the current matrix contains none and one appearing would be
   a fact worth seeing rather than a quoting problem to solve quietly - a
   toolchain prefix with a quote in it is a mistake somewhere upstream, and
   silently emitting `TOOLPREFIX='a'\''b-'` would carry that mistake into six
   scripts. *)

let safe_in_quotes c =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
  | '_' | '.' | '/' | '+' | '-' | '=' | ' ' | ',' | ':' -> true
  | _ -> false

let sh_string ~field v =
  if String.for_all safe_in_quotes v then Ok (Printf.sprintf "\"%s\"" v)
  else
    err (Printf.sprintf "%s value %S needs shell escaping - the matrix has never had one" field v)

(* Array elements are emitted BARE, as the hand-written file had them, so a word
   containing a space would silently split into two arguments. Rejected. *)
let sh_word ~field w =
  if w <> "" && String.for_all (fun c -> safe_in_quotes c && c <> ' ') w then Ok w
  else err (Printf.sprintf "%s element %S is not a bare shell word" field w)

let sh_array ~field = function
  | [] -> Ok None
  | words ->
      let* rendered =
        List.fold_left
          (fun acc w ->
            let* ws = acc in
            let* w = sh_word ~field w in
            Ok (w :: ws))
          (Ok []) words
      in
      Ok (Some (Printf.sprintf "%s=(%s)" field (String.concat " " (List.rev rendered))))

let assign ~field v =
  let* v = sh_string ~field v in
  Ok (Some (Printf.sprintf "%s=%s" field v))

let target_arm target =
  let c = Target.config target in
  let name = Target.to_string target in
  (* One uniform order for every target. The hand-written file had arm's
     READELF_MACHINE before its AS_FLAGS and riscv's after, which was accident
     rather than meaning; a generated file should not preserve accidents. *)
  let* lines =
    List.fold_left
      (fun acc f ->
        let* ls = acc in
        let* l = f () in
        Ok (match l with Some l -> l :: ls | None -> ls))
      (Ok [])
      [
        (fun () -> assign ~field:"CONFIGURE_TARGET" c.Target.configure_target);
        (fun () -> assign ~field:"TOOLPREFIX" c.Target.toolprefix);
        (fun () -> assign ~field:"QEMU_BIN" c.Target.qemu_bin);
        (fun () ->
          match c.Target.qemu_sysroot with
          | None -> Ok None
          | Some s -> assign ~field:"QEMU_SYSROOT" s);
        (fun () -> sh_array ~field:"CCOMP_EXTRA_ARGS" c.Target.ccomp_args);
        (fun () -> sh_array ~field:"COMPCERT_CONFIGURE_ARGS" c.Target.compcert_configure_args);
        (fun () -> sh_array ~field:"AS_FLAGS" c.Target.as_args);
        (fun () ->
          match c.Target.linker_emulation with
          | None -> Ok None
          | Some s -> assign ~field:"LINKER_EMULATION" s);
        (fun () -> sh_array ~field:"LD_FLAGS" c.Target.ld_args);
        (fun () -> assign ~field:"READELF_MACHINE" c.Target.readelf_machine);
      ]
  in
  let elf =
    (* One line, as the hand-written file had it: these three are one fact about
       the profile's width, and HAS_SYSROOT rides along because it is false by
       default and only ever set beside them. *)
    Printf.sprintf "ELF_CLASS=\"%s\"; WORD_SIZE=%d%s" c.Target.elf_class c.Target.word_size
      (if Target.has_sysroot target then "; HAS_SYSROOT=true" else "")
  in
  let link =
    [
      Printf.sprintf "LINK_TEXT_ADDR=\"%s\"" (Target.hex c.Target.link.Target.text);
      Printf.sprintf "LINK_RODATA_ADDR=\"%s\"" (Target.hex c.Target.link.Target.rodata);
      Printf.sprintf "LINK_DATA_ADDR=\"%s\"" (Target.hex c.Target.link.Target.data);
      Printf.sprintf "LINK_BSS_ADDR=\"%s\"" (Target.hex c.Target.link.Target.bss);
    ]
  in
  let body = List.rev lines @ (elf :: link) in
  Ok
    (Printf.sprintf "    %s)\n%s\n      ;;\n" name
       (String.concat "\n" (List.map (fun l -> "      " ^ l) body)))

let set_line field targets =
  Printf.sprintf "%s=(%s)" field (String.concat " " (List.map Target.to_string targets))

let header =
  {|#!/usr/bin/env bash
# GENERATED FILE - do not edit. Regenerate with `make tools-matrix`.
#
# The source of truth is asm/tools/lib/target.ml. This file exists so that shell
# consumers - tools/compcert-cross-smoke.sh, tools/compcert-fixture-setup.sh,
# tools/asm-helpers.sh and the Makefile - read the same definition the OCaml
# tooling does, without any of them acquiring a run-time dependency on a built
# executable. `make tools-matrix-diff` regenerates it and fails if the working
# tree changed, so an edit to target.ml that is not reflected here is caught in
# CI rather than at whichever consumer next disagreed.
#
# All targets are described in exactly one place because a fixture generated
# against one toolchain prefix and an oracle produced against another would
# compare cleanly and mean nothing.
#
# Capability sets are explicit: fixture work is freestanding and must not
# accidentally acquire the libc requirement of the cross-smoke suite.
#
# The two suites also own disjoint work roots. tools/compcert-cross-smoke.sh
# builds under .cross-smoke-work (CROSS_SMOKE_WORK); the fixture oracle builds
# under .fixture-work (FIXTURE_WORK). Both recreate build/, install/ and
# artifacts/ destructively per target, so a shared root meant either suite could
# delete the other's evidence mid-run.
|}

let config_doc =
  {|
# Sets CONFIGURE_TARGET, TOOLPREFIX, QEMU_BIN, QEMU_SYSROOT,
# CCOMP_EXTRA_ARGS, READELF_MACHINE for the given target. Even x86_32/x86_64
# use a dedicated cross-gcc package, not the host's native gcc -m32/-m64:
# gcc-multilib conflicts with the arm/aarch64 cross-gcc packages.
#
# LINK_*_ADDR are the controlled-link addresses M2's differential gate uses.
# They are the same numbers as asm/test/oracle/abi.ml's code_addr, rodata_addr
# and data_addr (and, for LINK_BSS_ADDR, abi_v2.ml's bss_addr - M3), because
# the GNU reference link, our own binder and the QEMU manifest must place a
# section at one address or the post-link byte comparison compares two
# different programs. abi.ml/abi_v2.ml stay the definition; Target.link
# mirrors them, and asm/test/oracle/test_record.ml asserts they still agree.
#
# The addresses are emitted per target rather than computed here, so the
# derivation lives in one language instead of two. It is abi.ml's window_base:
# 0x30000000 for the 32-bit profiles and 0x40000000 for the 64-bit ones - and
# the split exists at all because 0x40000000 does not fit a 31-bit native int -
# with rodata at +0x10000, data at +0x20000, and bss at +0x80000 (the first
# byte after the largest stack abi_v2.ml's stack_size_max permits).
target_config() {
  CONFIGURE_TARGET=""
  TOOLPREFIX=""
  QEMU_BIN=""
  QEMU_SYSROOT=""
  CCOMP_EXTRA_ARGS=()
  COMPCERT_CONFIGURE_ARGS=()
  AS_FLAGS=()
  LD_FLAGS=()
  LINKER_EMULATION=""
  ELF_CLASS=""
  WORD_SIZE=""
  HAS_SYSROOT=false
  READELF_MACHINE=""
  LINK_TEXT_ADDR=""
  LINK_RODATA_ADDR=""
  LINK_DATA_ADDR=""
  LINK_BSS_ADDR=""
  case "$1" in
|}

let trailer =
  {|    *)
      usage
      Fatal "unknown target '$1'"
      ;;
  esac
}

# When executed rather than sourced, print a named target set one per line, so
# the Makefile reads the same definition the scripts do instead of keeping its
# own copy of the list.
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  case "${1:-fixture}" in
    fixture)   printf '%s\n' "${FIXTURE_TARGETS[@]}" ;;
    assembler) printf '%s\n' "${ASSEMBLER_TARGETS[@]}" ;;
    libc)      printf '%s\n' "${LIBC_SMOKE_TARGETS[@]}" ;;
    *) echo "usage: $0 [fixture|assembler|libc]" >&2; exit 2 ;;
  esac
fi
|}

let render () =
  let* arms =
    List.fold_left
      (fun acc t ->
        let* as_ = acc in
        let* a = target_arm t in
        Ok (a :: as_))
      (Ok []) Target.all
  in
  let sets =
    String.concat "\n"
      [
        set_line "ASSEMBLER_TARGETS" (Target.set Target.Assembler);
        set_line "FIXTURE_TARGETS" (Target.set Target.Fixture);
        set_line "LIBC_SMOKE_TARGETS" (Target.set Target.Libc_smoke);
        {|ALL_TARGETS=("${ASSEMBLER_TARGETS[@]}")|};
      ]
  in
  Ok (header ^ sets ^ "\n" ^ config_doc ^ String.concat "" (List.rev arms) ^ trailer)

let emit () =
  match render () with
  | Error e -> Command.of_error e
  | Ok text ->
      (* To stdout, and the caller redirects. A tool that wrote into the source
         tree would need a work-root capability for a path that is neither a
         corpus nor a work root; the Makefile staging the output and moving it
         into place keeps that boundary intact. *)
      Command.ok [ Diagnostic.stdout text ]
