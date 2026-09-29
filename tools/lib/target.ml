type t = X86_32 | X86_64 | Arm | Aarch64 | Riscv32 | Riscv64

let all = [ X86_32; X86_64; Arm; Aarch64; Riscv32; Riscv64 ]

let to_string = function
  | X86_32 -> "x86_32"
  | X86_64 -> "x86_64"
  | Arm -> "arm"
  | Aarch64 -> "aarch64"
  | Riscv32 -> "riscv32"
  | Riscv64 -> "riscv64"

let pp ppf t = Format.pp_print_string ppf (to_string t)

let of_string s =
  match List.find_opt (fun t -> to_string t = s) all with
  | Some t -> Ok t
  | None ->
      (* The shell's exact wording, because --verify's rejection is one of the
         byte-compared diagnostics. *)
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Validate
           (Printf.sprintf "--verify supports %s, got '%s'"
              (String.concat " " (List.map to_string all))
              s))

type capability = Fixture | Assembler

let set = function Fixture | Assembler -> all

type link = { text : int; rodata : int; data : int; bss : int }

type config = {
  toolprefix : string;
  qemu_bin : string;
  qemu_sysroot : string option;
  gcc_fixture_args : string list;
  as_args : string list;
  ld_args : string list;
  linker_emulation : string option;
  elf_class : string;
  word_size : int;
  readelf_machine : string;
  link : link;
}

let hex n = Printf.sprintf "0x%x" n

(* abi.ml's window_base: 0x30000000 for the 32-bit profiles and 0x40000000 for
   the 64-bit ones. The split exists because 0x40000000 does not fit a 31-bit
   native int. rodata sits at +0x10000, data at +0x20000, bss at +0x80000
   (M3, docs/design.md §12: abi_v2.ml's bss_addr - stack_start +
   stack_size_max, the first byte after the largest stack the ABI permits).
   These must equal test/oracle/abi.ml's/abi_v2.ml's addresses: the GNU
   reference link, our own binder and the QEMU manifest have to place a
   section at ONE address, or the post-link byte comparison compares two
   different programs. *)
let link_of base =
  { text = base; rodata = base + 0x10000; data = base + 0x20000; bss = base + 0x80000 }

let base_of = function
  | X86_32 | Arm | Riscv32 -> 0x30000000
  | X86_64 | Aarch64 | Riscv64 -> 0x40000000

(* The flags a freestanding fixture is compiled with, after [-S]. This project
   has no dynamic linker and no GOT, so PIC/PIE addressing is never a construct
   a fixture here can resolve: the relocation table names direct relocations
   (R_AARCH64_ADR_PREL_PG_HI21, R_ARM_MOVW_ABS_NC, R_RISCV_PCREL_HI20,
   R_X86_64_PC32), not GOT slots. No unwind tables, so the assembly carries no
   CFI directives. The RISC-V ISA and ABI are pinned to the profile the rest of
   the tooling assumes: a generic cross gcc defaults to its multilib's widest
   ISA (riscv64-linux-gnu-gcc: rv64imafdc_zicsr_zifencei, compressed
   instructions included), which would silently test the wrong ISA. Arm gets
   only [-marm]: its triplet's default march, FPU and hard-float ABI already
   agree with [as_args], and restating [-march] without the FPU regresses the
   FPU selection and fails to compile. Nothing is inlined: a fixture that
   exists to exercise a call must keep it. No section anchors: they are printed
   as [.set] assignments this project does not model. RISC-V has no small-data section:
   the oracle's scope is .text, .data and .bss. *)
let gcc_common =
  [
    "-O1";
    "-fno-inline";
    "-fno-section-anchors";
    "-ffreestanding";
    "-fno-pic";
    "-fno-pie";
    "-fno-asynchronous-unwind-tables";
  ]

let config t =
  let link = link_of (base_of t) in
  let base =
    {
      toolprefix = "";
      qemu_bin = "";
      qemu_sysroot = None;
      gcc_fixture_args = gcc_common;
      as_args = [];
      ld_args = [];
      linker_emulation = None;
      elf_class = "";
      word_size = 0;
      readelf_machine = "";
      link;
    }
  in
  match t with
  | X86_32 ->
      {
        base with
        toolprefix = "i686-linux-gnu-";
        qemu_bin = "qemu-i386";
        qemu_sysroot = Some "/usr/i686-linux-gnu";
        readelf_machine = "Intel 80386";
        elf_class = "ELF32";
        word_size = 4;
      }
  | X86_64 ->
      {
        base with
        toolprefix = "x86_64-linux-gnu-";
        qemu_bin = "qemu-x86_64";
        qemu_sysroot = Some "/usr/x86_64-linux-gnu";
        readelf_machine = "Advanced Micro Devices X86-64";
        elf_class = "ELF64";
        word_size = 8;
      }
  | Arm ->
      {
        base with
        toolprefix = "arm-linux-gnueabihf-";
        qemu_bin = "qemu-arm";
        qemu_sysroot = Some "/usr/arm-linux-gnueabihf";
        gcc_fixture_args = "-marm" :: gcc_common;
        as_args = [ "-march=armv7-a" ];
        readelf_machine = "ARM";
        elf_class = "ELF32";
        word_size = 4;
      }
  | Aarch64 ->
      {
        base with
        toolprefix = "aarch64-linux-gnu-";
        qemu_bin = "qemu-aarch64";
        qemu_sysroot = Some "/usr/aarch64-linux-gnu";
        readelf_machine = "AArch64";
        elf_class = "ELF64";
        word_size = 8;
      }
  | Riscv32 ->
      {
        base with
        toolprefix = "riscv32-linux-gnu-";
        qemu_bin = "qemu-riscv32";
        qemu_sysroot = Some "/usr/riscv32-linux-gnu";
        gcc_fixture_args =
          gcc_common @ [ "-march=rv32imafd"; "-mabi=ilp32d"; "-mno-relax"; "-msmall-data-limit=0" ];
        as_args = [ "-march=rv32imafd"; "-mabi=ilp32d"; "-mno-relax" ];
        ld_args = [ "-m"; "elf32lriscv"; "--no-relax" ];
        linker_emulation = Some "elf32lriscv";
        readelf_machine = "RISC-V";
        elf_class = "ELF32";
        word_size = 4;
      }
  | Riscv64 ->
      {
        base with
        toolprefix = "riscv64-linux-gnu-";
        qemu_bin = "qemu-riscv64";
        qemu_sysroot = Some "/usr/riscv64-linux-gnu";
        gcc_fixture_args =
          gcc_common @ [ "-march=rv64imafd"; "-mabi=lp64d"; "-mno-relax"; "-msmall-data-limit=0" ];
        as_args = [ "-march=rv64imafd"; "-mabi=lp64d"; "-mno-relax" ];
        ld_args = [ "-m"; "elf64lriscv"; "--no-relax" ];
        linker_emulation = Some "elf64lriscv";
        readelf_machine = "RISC-V";
        elf_class = "ELF64";
        word_size = 8;
      }

let has_sysroot t = (config t).qemu_sysroot <> None

(* Init-time invariants, checked once. Each is a property the shell cannot state
   and therefore cannot enforce: they are why this is a database rather than six
   more copies of the same case statement. *)
let () =
  let names = List.map to_string all in
  assert (List.length (List.sort_uniq String.compare names) = List.length names);
  List.iter
    (fun t ->
      let c = config t in
      (* word_size and elf_class are two spellings of one fact. *)
      assert (
        (c.elf_class = "ELF32" && c.word_size = 4) || (c.elf_class = "ELF64" && c.word_size = 8));
      assert (c.link.text < c.link.rodata && c.link.rodata < c.link.data && c.link.data < c.link.bss);
      assert (c.qemu_sysroot <> None))
    all
