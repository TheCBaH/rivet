(* The shape of the reference snippets of execution ABI v1 §16.3, and the two
   constants around them that are not snippets.

   The snippets themselves are no longer here. They used to be four frozen
   tables of byte strings, and test/snippets held the same nine snippets as
   ASTs and asserted that assembling them reproduced these bytes. That bought
   independence while it was needed: for as long as the assembler could not
   build six of the nine, a table it could not influence was the only thing
   that could say what the right answer was.

   It is not needed now. All 36 assemble, and {!Snippet_corpus.Snippet_ast}
   builds them through the production lowering, encoding, layout and binding
   path, so the corpus is the single statement of §16.3 and {!Conform} runs
   what it produces. What that costs is stated where the corpus states it: the
   conformance suite is no longer independent of the assembler, and what
   replaces byte equality with a frozen table is the expect baselines beside
   the corpus plus the helpers' own semantic verdicts under QEMU - a snippet
   that assembles to the wrong bytes still has to return 42, or fault, or touch
   the guard page.

   What stays here is what is not a snippet: the record the suite consumes, the
   stack size the stack-boundary cases are written against, and the M1.6
   induced-failure byte pair. Keeping the type here rather than in the corpus
   is what lets {!Conform} name it without reaching into test/snippets for
   anything but the values. *)

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
(** Snippet bytes are in memory order, which is the order the manifest carries, the order the M1
    differential gate compares, and the order every committed artifact in this project is written
    in. *)

(* The 16 KiB every stack-boundary snippet is written against. Hardcoded in the
   snippet's displacement - now in the corpus AST, as [imm 0x4000] and its
   per-target equivalents - so the case must map exactly this much. *)
let stack_16k = 16 * 1024

(* {1 The induced-failure control}

   The byte pair M1.6 splices into an assembled image to turn a passing run into
   a failing one: the constant-materializing immediate, and the same instruction
   with 42 replaced by 41. Stated in memory order like everything else. On x86
   the immediate follows the opcode directly, so two bytes suffice; on the two
   fixed-width targets the whole instruction word is given, because a two-byte
   pattern would be ambiguous across a 32-bit encoding.

   This one stays a literal even though the snippets no longer are, and the
   reason is what it is for: it is the control that proves the exec gate can
   fail. A control derived from the same assembler whose output it is spliced
   into would move with it - mutate the encoder and the mutation follows,
   leaving the gate passing for a reason nobody stated. Two bytes are cheap to
   maintain by hand and the cost of getting them wrong is a loud failure of the
   control case, not a silent one. *)
let mutation = function
  | Abi.X86_32 | Abi.X86_64 -> ("\xb8\x2a", "\xb8\x29")
  | Abi.Arm -> ("\x2a\x00\xa0\xe3", "\x29\x00\xa0\xe3")
  | Abi.Aarch64 -> ("\x40\x05\x80\x52", "\x20\x05\x80\x52")
