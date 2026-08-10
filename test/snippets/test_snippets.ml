(* Does this assembler reproduce the frozen §16.3 bytes from an AST?

   Three claims, in order of strength:

   1. The hexdump baseline below. An AST on one side, the bytes it assembles to
      on the other, reviewed as a diff. This is the artifact: it makes the
      encoding of every dogfooded snippet visible in the tree without anyone
      having to run objdump.

   2. Equality with {!Asm_oracle.Snippet_bytes}. The baseline alone would pass
      if the assembler and the corpus were wrong in the same way, because both
      live in this directory. The frozen table does not: it predates this work,
      it is what QEMU runs, and nothing here can edit it. That is what makes the
      comparison evidence rather than a restatement.

   3. The inventory. Every snippet the assembler cannot yet build is listed with
      the form it needs, so asm/docs/contracts.md §4.3's prose list of
      deliberate absences becomes a table a test regresses. *)

open Snippet_corpus.Snippet_ast

let show_bytes label b = Printf.printf "%-16s %s\n" label (hex b)

(* {1 The dogfooded snippets}

   One expect block per target rather than one for all four: a diff in the
   aarch64 encoding should not reprint the x86 baseline, and four blocks say
   which target moved. *)

let report target_name =
  let t = List.find (fun t -> String.equal t.target target_name) all in
  List.iter
    (fun e ->
      match e.built with
      | Needs _ -> ()
      | Refused msg ->
          Printf.printf "%s: REFUSED by the assembler: %s\n" e.name msg;
          Printf.printf "  frozen  %s\n" (hex (e.frozen t.snippets))
      | Assembled { bytes; canonical } ->
          let want = e.frozen t.snippets in
          Printf.printf "%s\n" e.name;
          List.iter (fun line -> Printf.printf "  | %s\n" line) canonical;
          show_bytes "  assembled" bytes;
          if String.equal bytes want then print_endline "  == frozen"
          else (
            show_bytes "  FROZEN" want;
            (* Naming the first differing offset, because two 32-byte hexdumps
               that differ in one nibble are not something a reader diffs. *)
            let n = min (String.length bytes) (String.length want) in
            let rec first i =
              if i >= n then n else if Char.equal bytes.[i] want.[i] then first (i + 1) else i
            in
            Printf.printf "  MISMATCH at byte %d\n" (first 0)))
    t.entries

let%expect_test "x86_64: AST assembles to the frozen bytes" =
  report "x86_64";
  [%expect
    {|
    return42
      | subq $8, %rsp
      | leaq 16(%rsp), %rax
      | movq %rax, (%rsp)
      | movl $42, %eax
      | addq $8, %rsp
      | ret
      assembled      48 83 ec 08 48 8d 44 24 10 48 89 04 24 b8 2a 00 00 00 48 83 c4 08 c3
      == frozen
    return41
      | subq $8, %rsp
      | leaq 16(%rsp), %rax
      | movq %rax, (%rsp)
      | movl $41, %eax
      | addq $8, %rsp
      | ret
      assembled      48 83 ec 08 48 8d 44 24 10 48 89 04 24 b8 29 00 00 00 48 83 c4 08 c3
      == frozen |}]

let%expect_test "x86_32: AST assembles to the frozen bytes" =
  report "x86_32";
  [%expect
    {|
    return42
      | subl $12, %esp
      | leal 16(%esp), %eax
      | movl %eax, (%esp)
      | movl $42, %eax
      | addl $12, %esp
      | ret
      assembled      83 ec 0c 8d 44 24 10 89 04 24 b8 2a 00 00 00 83 c4 0c c3
      == frozen
    return41
      | subl $12, %esp
      | leal 16(%esp), %eax
      | movl %eax, (%esp)
      | movl $41, %eax
      | addl $12, %esp
      | ret
      assembled      83 ec 0c 8d 44 24 10 89 04 24 b8 29 00 00 00 83 c4 0c c3
      == frozen |}]

let%expect_test "arm: AST assembles to the frozen bytes" =
  report "arm";
  [%expect
    {|
    return42
      | mov ip, sp
      | sub sp, sp, #8
      | str ip, [sp, #0]
      | str lr, [sp, #4]
      | mov r0, #42
      | ldr lr, [sp, #4]
      | add sp, sp, #8
      | bx lr
      assembled      0d c0 a0 e1 08 d0 4d e2 00 c0 8d e5 04 e0 8d e5 2a 00 a0 e3 04 e0 9d e5 08 d0 8d e2 1e ff 2f e1
      == frozen
    return41
      | mov ip, sp
      | sub sp, sp, #8
      | str ip, [sp, #0]
      | str lr, [sp, #4]
      | mov r0, #41
      | ldr lr, [sp, #4]
      | add sp, sp, #8
      | bx lr
      assembled      0d c0 a0 e1 08 d0 4d e2 00 c0 8d e5 04 e0 8d e5 29 00 a0 e3 04 e0 9d e5 08 d0 8d e2 1e ff 2f e1
      == frozen
    callee_clobber
      | mov r4, #0
      | mov r0, #42
      | bx lr
      assembled      00 40 a0 e3 2a 00 a0 e3 1e ff 2f e1
      == frozen
    sp_corrupt
      | sub sp, sp, #16
      | mov r0, #42
      | bx lr
      assembled      10 d0 4d e2 2a 00 a0 e3 1e ff 2f e1
      == frozen |}]

let%expect_test "aarch64: AST assembles to the frozen bytes" =
  report "aarch64";
  [%expect
    {|
    return42
      | mov x15, sp
      | stp x15, x30, [sp, #-16]!
      | movz w0, #42
      | ldr x30, [sp, #8]
      | add sp, sp, #16
      | ret
      assembled      ef 03 00 91 ef 7b bf a9 40 05 80 52 fe 07 40 f9 ff 43 00 91 c0 03 5f d6
      == frozen
    return41
      | mov x15, sp
      | stp x15, x30, [sp, #-16]!
      | movz w0, #41
      | ldr x30, [sp, #8]
      | add sp, sp, #16
      | ret
      assembled      ef 03 00 91 ef 7b bf a9 20 05 80 52 fe 07 40 f9 ff 43 00 91 c0 03 5f d6
      == frozen
    callee_clobber
      | movz x19, #0
      | movz w0, #42
      | ret
      assembled      13 00 80 d2 40 05 80 52 c0 03 5f d6
      == frozen |}]

(* {1 The inventory}

   What is dogfooded and what is not, in one table. The [needs] column is the
   frontier: it is the work M2 has to do before that snippet can be built here,
   stated per profile because the instruction sets do not run out of forms at
   the same place - AArch64 encodes [movz x19, #0] with the fixture's own form
   and so reaches [callee_clobber] today, while x86 needs [xor] for the same
   snippet. *)

let%expect_test "the M1 scope frontier, per snippet and profile" =
  let status e =
    match e.built with
    | Assembled _ -> "ok"
    | Refused _ -> "REFUSED"
    | Needs what -> "needs: " ^ what
  in
  let names = List.map (fun e -> e.name) (List.hd all).entries in
  List.iter
    (fun name ->
      Printf.printf "%s\n" name;
      List.iter
        (fun t ->
          match List.find_opt (fun e -> String.equal e.name name) t.entries with
          | Some e -> Printf.printf "  %-8s %s\n" t.target (status e)
          | None -> Printf.printf "  %-8s ABSENT FROM CORPUS\n" t.target)
        all)
    names;
  let total = List.length all * List.length names in
  let built =
    List.length
      (List.filter
         (fun e -> match e.built with Assembled _ -> true | _ -> false)
         (List.concat_map (fun t -> t.entries) all))
  in
  Printf.printf "\ndogfooded %d of %d\n" built total;
  [%expect
    {|
    return42
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
    return41
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
    trap
      x86_32   needs: ud2
      x86_64   needs: ud2
      arm      needs: udf - no trap form
      aarch64  needs: udf - no trap form
    spin
      x86_32   needs: jmp . - PC-relative branch, §4.3 defers fixups to M2
      x86_64   needs: jmp . - PC-relative branch, §4.3 defers fixups to M2
      arm      needs: b . - PC-relative branch, §4.3 defers fixups to M2
      aarch64  needs: b . - PC-relative branch, §4.3 defers fixups to M2
    sp_align
      x86_32   needs: and/cmp/jcc - two ALU extensions plus a PC-relative branch
      x86_64   needs: and/cmp/jcc - two ALU extensions plus a PC-relative branch
      arm      needs: and/cmp plus the conditional-execution suffixes moveq/movne
      aarch64  needs: and/cmp/b.eq - three forms plus a PC-relative branch
    stack_full
      x86_32   needs: movb $0,(%reg) - byte-width store to memory
      x86_64   needs: movb $0,(%reg) - byte-width store to memory
      arm      needs: strb
      aarch64  needs: sub-imm with lsl #12, and strb
    stack_over
      x86_32   needs: movb $0,(%reg) - byte-width store to memory
      x86_64   needs: movb $0,(%reg) - byte-width store to memory
      arm      needs: strb
      aarch64  needs: sub-imm with lsl #12, and strb
    sp_corrupt
      x86_32   needs: pop, and the indirect jmp *%reg
      x86_64   needs: pop, and the indirect jmp *%reg
      arm      ok
      aarch64  needs: sub-imm - A64 encodes it separately from add-imm
    callee_clobber
      x86_32   needs: xor
      x86_64   needs: xor
      arm      ok
      aarch64  ok

    dogfooded 11 of 36 |}]

(* {1 Padding}

   [nop_bytes] is the one blob that reaches an image without passing through an
   instruction, so no snippet covers it. Pinned per length here instead.

   The x86 table is a genuine encoding table - nine hand-written multi-byte
   no-ops, one per length - and stays a literal in the target, because it *is*
   the encoding rather than a restatement of one. What it did not have was
   anything asserting the lengths come out right. *)

let%expect_test "nop_bytes, every length a target accepts" =
  List.iter
    (fun t ->
      Printf.printf "%s\n" t.target;
      List.iter
        (fun length ->
          match t.nop ~length with
          | Ok b when String.length b = length -> Printf.printf "  %2d  %s\n" length (hex b)
          | Ok b -> Printf.printf "  %2d  WRONG LENGTH %d: %s\n" length (String.length b) (hex b)
          | Error m -> Printf.printf "  %2d  rejected: %s\n" length m)
        (List.init 16 (fun i -> i + 1)))
    all;
  [%expect
    {|
    x86_32
       1  90
       2  66 90
       3  0f 1f 00
       4  0f 1f 40 00
       5  0f 1f 44 00 00
       6  66 0f 1f 44 00 00
       7  0f 1f 80 00 00 00 00
       8  0f 1f 84 00 00 00 00 00
       9  66 0f 1f 84 00 00 00 00 00
      10  66 0f 1f 84 00 00 00 00 00 90
      11  66 0f 1f 84 00 00 00 00 00 66 90
      12  66 0f 1f 84 00 00 00 00 00 0f 1f 00
      13  66 0f 1f 84 00 00 00 00 00 0f 1f 40 00
      14  66 0f 1f 84 00 00 00 00 00 0f 1f 44 00 00
      15  66 0f 1f 84 00 00 00 00 00 66 0f 1f 44 00 00
      16  66 0f 1f 84 00 00 00 00 00 0f 1f 80 00 00 00 00
    x86_64
       1  90
       2  66 90
       3  0f 1f 00
       4  0f 1f 40 00
       5  0f 1f 44 00 00
       6  66 0f 1f 44 00 00
       7  0f 1f 80 00 00 00 00
       8  0f 1f 84 00 00 00 00 00
       9  66 0f 1f 84 00 00 00 00 00
      10  66 0f 1f 84 00 00 00 00 00 90
      11  66 0f 1f 84 00 00 00 00 00 66 90
      12  66 0f 1f 84 00 00 00 00 00 0f 1f 00
      13  66 0f 1f 84 00 00 00 00 00 0f 1f 40 00
      14  66 0f 1f 84 00 00 00 00 00 0f 1f 44 00 00
      15  66 0f 1f 84 00 00 00 00 00 66 0f 1f 44 00 00
      16  66 0f 1f 84 00 00 00 00 00 0f 1f 80 00 00 00 00
    arm
       1  rejected: A32 padding must be a whole number of four-byte instructions
       2  rejected: A32 padding must be a whole number of four-byte instructions
       3  rejected: A32 padding must be a whole number of four-byte instructions
       4  00 f0 20 e3
       5  rejected: A32 padding must be a whole number of four-byte instructions
       6  rejected: A32 padding must be a whole number of four-byte instructions
       7  rejected: A32 padding must be a whole number of four-byte instructions
       8  00 f0 20 e3 00 f0 20 e3
       9  rejected: A32 padding must be a whole number of four-byte instructions
      10  rejected: A32 padding must be a whole number of four-byte instructions
      11  rejected: A32 padding must be a whole number of four-byte instructions
      12  00 f0 20 e3 00 f0 20 e3 00 f0 20 e3
      13  rejected: A32 padding must be a whole number of four-byte instructions
      14  rejected: A32 padding must be a whole number of four-byte instructions
      15  rejected: A32 padding must be a whole number of four-byte instructions
      16  00 f0 20 e3 00 f0 20 e3 00 f0 20 e3 00 f0 20 e3
    aarch64
       1  rejected: A64 padding must be a whole number of four-byte instructions
       2  rejected: A64 padding must be a whole number of four-byte instructions
       3  rejected: A64 padding must be a whole number of four-byte instructions
       4  1f 20 03 d5
       5  rejected: A64 padding must be a whole number of four-byte instructions
       6  rejected: A64 padding must be a whole number of four-byte instructions
       7  rejected: A64 padding must be a whole number of four-byte instructions
       8  1f 20 03 d5 1f 20 03 d5
       9  rejected: A64 padding must be a whole number of four-byte instructions
      10  rejected: A64 padding must be a whole number of four-byte instructions
      11  rejected: A64 padding must be a whole number of four-byte instructions
      12  1f 20 03 d5 1f 20 03 d5 1f 20 03 d5
      13  rejected: A64 padding must be a whole number of four-byte instructions
      14  rejected: A64 padding must be a whole number of four-byte instructions
      15  rejected: A64 padding must be a whole number of four-byte instructions
      16  1f 20 03 d5 1f 20 03 d5 1f 20 03 d5 1f 20 03 d5 |}]
