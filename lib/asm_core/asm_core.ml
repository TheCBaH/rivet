(* Lexer, Menhir grammar, expressions, the three staged public ASTs, and the
   generic passes over them (.ai/asm_plan.md §4, §7).

   Filled in at M1.1-M1.3. AST containers are parameterized, not functorized
   ([type 'insn module_]), so a normalized AST for one target is a distinct
   type from another's and OCaml rejects passing an ARM normalized AST to x86
   lowering. Per §4.7 Menhir owns the common statement and expression grammar;
   hand-written token-cursor parsers own target operands. *)

type nothing = |
