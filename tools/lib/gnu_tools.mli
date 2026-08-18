(** The external binutils, as an owned interface.

    Every invocation names its tool through the target's TOOLPREFIX and goes
    through {!Tool_process}, so none of it is a shell string. Capture is exact
    and the accepted status set is stated per call rather than inherited. *)

type t
(** The toolchain for one target: the prefix plus the target's assembler and
    linker flags, resolved once so no call site re-derives them. *)

val for_target : Target.t -> t

val require : t -> qemu:bool -> (unit, Tool_error.t) Err.t
(** Every program this module can invoke, checked up front so a missing cross
    toolchain is ONE clear diagnostic rather than a failure deep inside a link.
    [~qemu:true] additionally requires the target's emulator. *)

val installed : t -> [ `As | `Ld | `Qemu ] -> bool
(** Whether one named program can be STARTED, resolved through the same PATH
    search a child would use - so this means "executable by a child" and not "a
    file exists". The tool gate asks per program because a missing cross
    toolchain is one recorded failure there, not a run-ending error. *)

val assemble : t -> cwd:Fpath.t -> src:Fpath.t -> obj:Fpath.t -> (unit, Tool_error.t) Err.t

val link :
  t ->
  cwd:Fpath.t ->
  script:Fpath.t ->
  out:Fpath.t ->
  objs:string list ->
  (unit, Tool_error.t) Err.t
(** Always [-static], and the script is the only thing that places sections. *)

val objcopy_section :
  t -> src:Fpath.t -> section:string -> out:Fpath.t -> (bool, Tool_error.t) Err.t
(** Extracts one section as raw binary. [false] means the section is absent or
    empty - both of which the caller treats as "not present", matching the
    shell's `objcopy … 2>/dev/null && [ -s … ]`. A nonzero objcopy status is
    therefore NOT an error here, which is why this returns a bool rather than
    relying on the accepted-status machinery. *)

val readelf_headers : t -> Fpath.t -> (string, Tool_error.t) Err.t
(** [readelf -SW], for the REL/RELA decision. *)

val objdump_headers : t -> Fpath.t -> (string, Tool_error.t) Err.t
(** [objdump -h], for the allocated-section enumeration and section sizes. *)

val objdump_relocs : t -> Fpath.t -> (string, Tool_error.t) Err.t
val readelf_symbols : t -> Fpath.t -> (string, Tool_error.t) Err.t

val readelf_oracle : t -> Fpath.t -> scrub:Fpath.t -> (string, Tool_error.t) Err.t
(** [readelf -SWsWr], normalized for commit: the scratch path removed, the
    "File: …" line blanked, trailing whitespace stripped, and runs of blank
    lines collapsed. Those normalizations are why this artifact does not encode
    where the checkout lives. *)

val objdump_disasm :
  t ->
  Fpath.t ->
  scrub:Fpath.t ->
  drop_banner:bool ->
  riscv_numeric:bool ->
  (string, Tool_error.t) Err.t
(** [objdump -dr]. [drop_banner] removes the first two lines, which name the
    object file.

    [riscv_numeric] selects [-M no-aliases,numeric] on the two RISC-V profiles,
    and it is an EXPLICIT argument because the two corpora genuinely disagree:
    the fixture oracle passes it, so its artifacts record what was encoded
    (`addi x30,x2,0`), while the gas cross-reference does not, so its artifacts
    record the alias spelling (`li a0,42`, `ret`). Defaulting either way would
    silently rewrite one corpus. *)

val version_line :
  t -> [ `As | `Ld | `Objdump | `Readelf | `Objcopy ] -> (string, Tool_error.t) Err.t
(** The tool's first [--version] line, which is what tool-versions.txt records
    so that a binutils change is visible as a change rather than as noise on
    every artifact line. *)

val qemu_run :
  t -> exe:Fpath.t -> timeout_s:int -> (Process_status.t * string * string, Tool_error.t) Err.t
(** Runs the linked image under the target's emulator with an external
    [timeout]. Every exit status is accepted and returned, including 124, which
    the caller must classify - the 124-127 range belongs to the host runner and
    a guest result must never be confusable with a harness failure. *)

val elf_identity : t -> Fpath.t -> (string * string, Tool_error.t) Err.t
(** The Class and Machine fields of [readelf -h], compared against the target's
    declared ELF class and machine so a mis-targeted assembler is caught before
    anything is executed. *)

val readelf_exec : t -> Fpath.t -> (string, Tool_error.t) Err.t
(** [readelf -hSWsr] on the linked image. Not normalized: this is a work
    artifact for debugging a failed run, not a committed one. *)

val qemu_version : t -> (string, Tool_error.t) Err.t
(** Which emulator actually ran the bytes - the one piece of provenance an
    execution-only failure cannot be reproduced without. *)

type gas_outcome =
  | Assembled
  | Rejected of string
      (** [Rejected] carries the already-trimmed diagnostic body: at most the first
    three Error/Warning lines, with the input path and the scratch directory
    stripped. Both outcomes are DATA - "gas rejects this too" is as much a fact
    about a frontier case as its bytes are, since a file neither assembler
    accepts says nothing about the difference between them. *)

val try_assemble :
  t -> src:Fpath.t -> obj:Fpath.t -> include_dir:Fpath.t option -> (gas_outcome, Tool_error.t) Err.t
(** Assembles without requiring success. stdout and stderr are merged into one
    capture, matching the shell's `> file 2>&1` - the diagnostics are what is
    recorded, and splitting them would reorder interleaved output. *)

type gate_step =
  | Step_ok
  | Step_failed of string
      (** [Step_failed] carries the whole captured diagnostic with newlines turned
    into spaces - `tr '\n' ' '`, final newline included, so a one-line message
    ends in a space. Untrimmed and unfiltered, unlike {!gas_outcome}: a tool
    gate failure is read by someone asking what a base-image update did, and the
    first three Error lines are not reliably the informative ones. *)

val gate_assemble :
  t -> src:Fpath.t -> obj:Fpath.t -> log:Fpath.t -> (gate_step, Tool_error.t) Err.t
(** Assembles without requiring success, writing the raw stderr to [log] as an
    artifact. A rejection is DATA here: one target failing to assemble is one
    recorded gate failure and the other five still run. *)

val gate_link :
  t -> entry:string -> obj:Fpath.t -> out:Fpath.t -> log:Fpath.t -> (gate_step, Tool_error.t) Err.t
(** [-static -e ENTRY], with NO linker script: a freestanding image with no libc
    and no crt, which is a different link from {!link}'s placed one.

    Separate from {!gate_assemble} so that linking has its own claim rather than
    riding along as a side effect - a gate that only assembled would pass with a
    broken ld. *)

val preprocess :
  t ->
  src:Fpath.t ->
  out:Fpath.t ->
  defines:string list ->
  include_dir:Fpath.t ->
  (bool, Tool_error.t) Err.t
(** [gcc -E -P] with the target's runtime defines. [-P] suppresses line
    markers, which carry absolute include paths and would put this checkout's
    location into a committed artifact.

    Returns [false] rather than failing when cpp rejects the input: the shell
    spells this `|| { rm -rf "$fdir"; continue; }`, so an unpreprocessable
    source is simply not a case. *)
