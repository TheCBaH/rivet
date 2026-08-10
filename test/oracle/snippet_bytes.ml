(* The frozen reference snippets of execution ABI v1 (asm/docs/exec-abi-v1.md
   §16.3).

   Per profile, because the whole point is that the four helpers implement one
   contract on four instruction sets. Byte strings rather than assembler input:
   §16.3 freezes the trap bytes exactly, and a snippet the conformance suite
   assembled for itself would be evidence about the assembler rather than about
   the helper.

   That constraint is why this module exists as its own compilation unit rather
   than staying inside conform.ml. The table is wanted in two places with
   opposite obligations:

   - {!Conform}, which must reach *no part of the assembler*, so that a broken
     assembler cannot make the ABI suite pass;
   - test/snippets, which builds the same snippets as an AST, assembles them,
     and asserts the result equals what is written here.

   Both read this table and neither can influence it. The dependency edge runs
   test -> table and driver -> table, never table -> assembler, so dogfooding
   the bytes costs the conformance suite none of its independence. A literal
   here is the ground truth; the assembler's job is to reproduce it.

   Snippet bytes are in memory order, which is the order the manifest carries,
   the order the M1 differential gate compares, and the order every committed
   artifact in this project is written in. *)

type t = {
  return42 : string;  (** the CompCert 3.17 return-42 fixture for this profile *)
  return41 : string;  (** the same, with the constant-materializing immediate mutated *)
  trap : string;  (** §16.3, must trap at that instruction *)
  spin : string;  (** a guest that never returns *)
  sp_align : string;  (** returns 42 iff the entry SP alignment of §11 holds *)
  stack_full : string;  (** touches the lowest byte of a 16 KiB stack, then returns 42 *)
  stack_over : string;  (** touches one byte below it: the guard page *)
  sp_corrupt : string;  (** returns with a corrupted SP - protocol subcode 1 *)
  callee_clobber : string;  (** returns having clobbered the saved-SP register *)
}

(* The 16 KiB every stack-boundary snippet is written against. Hardcoded in the
   snippet's displacement, so the case must use this size. *)
let stack_16k = 16 * 1024

let x86_64 =
  {
    (* 48 83 ec 08 | 48 8d 44 24 10 | 48 89 04 24 | b8 2a 00 00 00 | 48 83 c4 08 | c3 *)
    return42 =
      "\x48\x83\xec\x08\x48\x8d\x44\x24\x10\x48\x89\x04\x24\xb8\x2a\x00\x00\x00\x48\x83\xc4\x08\xc3";
    return41 =
      "\x48\x83\xec\x08\x48\x8d\x44\x24\x10\x48\x89\x04\x24\xb8\x29\x00\x00\x00\x48\x83\xc4\x08\xc3";
    trap = "\x0f\x0b";
    (* eb fe: jmp to itself *)
    spin = "\xeb\xfe";
    (* mov %rsp,%rax; and $15,%rax; cmp $8,%rax; mov $42,%eax; je +5; mov $41,%eax; ret *)
    sp_align =
      "\x48\x89\xe0\x48\x83\xe0\x0f\x48\x83\xf8\x08\xb8\x2a\x00\x00\x00\x74\x05\xb8\x29\x00\x00\x00\xc3";
    (* lea -0x3ff8(%rsp),%rax; movb $0,(%rax); mov $42,%eax; ret.
       Entry RSP is stack_top - 8, so this is exactly the lowest stack byte. *)
    stack_full = "\x48\x8d\x84\x24\x08\xc0\xff\xff\xc6\x00\x00\xb8\x2a\x00\x00\x00\xc3";
    (* the same, one byte lower: the guard page *)
    stack_over = "\x48\x8d\x84\x24\x07\xc0\xff\xff\xc6\x00\x00\xb8\x2a\x00\x00\x00\xc3";
    (* pop %rdx; sub $16,%rsp; mov $42,%eax; jmp *%rdx *)
    sp_corrupt = "\x5a\x48\x83\xec\x10\xb8\x2a\x00\x00\x00\xff\xe2";
    (* xor %rbx,%rbx; mov $42,%eax; ret *)
    callee_clobber = "\x48\x31\xdb\xb8\x2a\x00\x00\x00\xc3";
  }

let x86_32 =
  {
    (* 83 ec 0c | 8d 44 24 10 | 89 04 24 | b8 2a 00 00 00 | 83 c4 0c | c3 *)
    return42 = "\x83\xec\x0c\x8d\x44\x24\x10\x89\x04\x24\xb8\x2a\x00\x00\x00\x83\xc4\x0c\xc3";
    return41 = "\x83\xec\x0c\x8d\x44\x24\x10\x89\x04\x24\xb8\x29\x00\x00\x00\x83\xc4\x0c\xc3";
    trap = "\x0f\x0b";
    spin = "\xeb\xfe";
    (* §11 has ESP = 12 (mod 16) at entry here, not 8: the return address is
       four bytes wide. Getting this constant wrong is the whole reason the
       case exists. *)
    sp_align =
      "\x89\xe0\x83\xe0\x0f\x83\xf8\x0c\xb8\x2a\x00\x00\x00\x74\x05\xb8\x29\x00\x00\x00\xc3";
    (* lea -0x3ffc(%esp),%eax - entry ESP is stack_top - 4 *)
    stack_full = "\x8d\x84\x24\x04\xc0\xff\xff\xc6\x00\x00\xb8\x2a\x00\x00\x00\xc3";
    stack_over = "\x8d\x84\x24\x03\xc0\xff\xff\xc6\x00\x00\xb8\x2a\x00\x00\x00\xc3";
    (* pop %edx; sub $16,%esp; mov $42,%eax; jmp *%edx *)
    sp_corrupt = "\x5a\x83\xec\x10\xb8\x2a\x00\x00\x00\xff\xe2";
    (* xor %ebx,%ebx; mov $42,%eax; ret *)
    callee_clobber = "\x31\xdb\xb8\x2a\x00\x00\x00\xc3";
  }

(* ARM words are little-endian, so 0xe3a0002a is 2a 00 a0 e3 in memory. *)
let arm =
  {
    (* mov r12,sp | sub sp,sp,#8 | str r12,[sp] | str lr,[sp,#4]
       | mov r0,#42 | ldr lr,[sp,#4] | add sp,sp,#8 | bx lr *)
    return42 =
      "\x0d\xc0\xa0\xe1\x08\xd0\x4d\xe2\x00\xc0\x8d\xe5\x04\xe0\x8d\xe5\x2a\x00\xa0\xe3\x04\xe0\x9d\xe5\x08\xd0\x8d\xe2\x1e\xff\x2f\xe1";
    return41 =
      "\x0d\xc0\xa0\xe1\x08\xd0\x4d\xe2\x00\xc0\x8d\xe5\x04\xe0\x8d\xe5\x29\x00\xa0\xe3\x04\xe0\x9d\xe5\x08\xd0\x8d\xe2\x1e\xff\x2f\xe1";
    (* §16.3: udf #0 = e7f000f0. Not .inst 0 - on A32 that word decodes as
       andeq r0,r0,r0, a conditional no-op, and a snippet that fell through into
       whatever followed would report success while proving nothing. *)
    trap = "\xf0\x00\xf0\xe7";
    (* b . = eafffffe *)
    spin = "\xfe\xff\xff\xea";
    (* mov r0,sp | and r0,r0,#7 | cmp r0,#0 | moveq r0,#42 | movne r0,#41 | bx lr *)
    sp_align =
      "\x0d\x00\xa0\xe1\x07\x00\x00\xe2\x00\x00\x50\xe3\x2a\x00\xa0\x03\x29\x00\xa0\x13\x1e\xff\x2f\xe1";
    (* sub r1,sp,#0x4000 | mov r0,#0 | strb r0,[r1] | mov r0,#42 | bx lr.
       Nothing is pushed by the call - the return address goes in lr - so entry
       SP is exactly the top of the stack and this is its lowest byte. *)
    stack_full = "\x01\x19\x4d\xe2\x00\x00\xa0\xe3\x00\x00\xc1\xe5\x2a\x00\xa0\xe3\x1e\xff\x2f\xe1";
    stack_over =
      "\x01\x19\x4d\xe2\x01\x10\x41\xe2\x00\x00\xa0\xe3\x00\x00\xc1\xe5\x2a\x00\xa0\xe3\x1e\xff\x2f\xe1";
    (* sub sp,sp,#16 | mov r0,#42 | bx lr *)
    sp_corrupt = "\x10\xd0\x4d\xe2\x2a\x00\xa0\xe3\x1e\xff\x2f\xe1";
    (* mov r4,#0 | mov r0,#42 | bx lr - r4 is §11's saved-SP register here *)
    callee_clobber = "\x00\x40\xa0\xe3\x2a\x00\xa0\xe3\x1e\xff\x2f\xe1";
  }

let aarch64 =
  {
    (* mov x15,sp | stp x15,x30,[sp,#-16]! | movz w0,#42 | ldr x30,[sp,#8]
       | add sp,sp,#16 | ret x30 *)
    return42 =
      "\xef\x03\x00\x91\xef\x7b\xbf\xa9\x40\x05\x80\x52\xfe\x07\x40\xf9\xff\x43\x00\x91\xc0\x03\x5f\xd6";
    (* movz w0,#41 = 52800520 *)
    return41 =
      "\xef\x03\x00\x91\xef\x7b\xbf\xa9\x20\x05\x80\x52\xfe\x07\x40\xf9\xff\x43\x00\x91\xc0\x03\x5f\xd6";
    (* §16.3: udf #0 is genuinely the zero word here. brk #0 (d4200000, SIGTRAP)
       is a verified alternative; udf is preferred because it keeps all four
       profiles on SIGILL. *)
    trap = "\x00\x00\x00\x00";
    (* b . = 14000000 *)
    spin = "\x00\x00\x00\x14";
    (* mov x0,sp | and x0,x0,#15 | cmp x0,#0 | mov w0,#42 | b.eq +8
       | mov w0,#41 | ret *)
    sp_align =
      "\xe0\x03\x00\x91\x00\x0c\x40\x92\x1f\x00\x00\xf1\x40\x05\x80\x52\x40\x00\x00\x54\x20\x05\x80\x52\xc0\x03\x5f\xd6";
    (* sub x1,sp,#4,lsl#12 | strb wzr,[x1] | mov w0,#42 | ret *)
    stack_full = "\xe1\x13\x40\xd1\x3f\x00\x00\x39\x40\x05\x80\x52\xc0\x03\x5f\xd6";
    stack_over = "\xe1\x13\x40\xd1\x21\x04\x00\xd1\x3f\x00\x00\x39\x40\x05\x80\x52\xc0\x03\x5f\xd6";
    (* sub sp,sp,#16 | mov w0,#42 | ret *)
    sp_corrupt = "\xff\x43\x00\xd1\x40\x05\x80\x52\xc0\x03\x5f\xd6";
    (* mov x19,#0 | mov w0,#42 | ret - x19 is §11's saved-SP register here *)
    callee_clobber = "\x13\x00\x80\xd2\x40\x05\x80\x52\xc0\x03\x5f\xd6";
  }

let for_profile = function
  | Abi.X86_64 -> Some x86_64
  | Abi.X86_32 -> Some x86_32
  | Abi.Arm -> Some arm
  | Abi.Aarch64 -> Some aarch64

(* {1 The induced-failure control}

   The byte pair M1.6 splices into an assembled image to turn a passing run into
   a failing one: the constant-materializing immediate, and the same instruction
   with 42 replaced by 41. Stated in memory order like everything else. On x86
   the immediate follows the opcode directly, so two bytes suffice; on the two
   fixed-width targets the whole instruction word is given, because a two-byte
   pattern would be ambiguous across a 32-bit encoding.

   Here rather than in exec.ml because it is the same frozen fact as
   {!return42} and {!return41} - the difference between the two snippets *is*
   this byte pair - and two statements of one fact drift. *)
let mutation = function
  | Abi.X86_32 | Abi.X86_64 -> ("\xb8\x2a", "\xb8\x29")
  | Abi.Arm -> ("\x2a\x00\xa0\xe3", "\x29\x00\xa0\xe3")
  | Abi.Aarch64 -> ("\x40\x05\x80\x52", "\x20\x05\x80\x52")
