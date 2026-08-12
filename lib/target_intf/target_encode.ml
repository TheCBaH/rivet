(* The encoding half of the TARGET signature (.ai/asm_plan.md §5.1).

   Everything a target does once an instruction exists as a value: normalize
   it, lower it, encode it, decode it back, evaluate a fixup, pad a section.
   None of it needs source text, so none of it is stated in terms of tokens -
   which is what lets this signature, and the libraries that satisfy it, be
   linked without a lexer or a parser.

   {!Target.TARGET} is this plus the three members that do read source text: a
   lexical profile, an operand parser, and a directive handler. Splitting the
   two was not a tidiness exercise. A program that builds instructions as
   values - test/snippets does, and so does anything that generates them -
   wants the whole assembler *below* parsing and none of it above, and a single
   signature made that unaskable: naming any part of a target dragged in the
   front end, because the signature itself mentioned {!Asm_syntax.Token}.

   Signatures only, like {!Target}: no implementation lives here, so a concrete
   target depends on a description it cannot influence. *)

open Foundation

module type ENCODE = sig
  (* {1 Identity} *)

  val name : string
  (** the registry key and the [--target] spelling: [x86_32], [x86_64], [arm], [aarch64] *)

  val triple : string
  (** the GNU triple the fixture oracle used, so a differential failure can name the exact
      toolchain it disagrees with rather than "the reference" *)

  (* {1 Target-specific types}

     All abstract. The pipeline moves values of these types between the stages
     below and never looks inside one.

     Each family is a *module* rather than a bare type plus loose functions,
     and that is what removes the [pp_lowered = Lowered.pp] shim every target
     otherwise had to write. A staged AST is a type together with the two
     operations generic code performs on it - print it, and for the last stage
     compare it - so the module is the unit that travels, and a target
     satisfies this signature by exposing the module it already has.

     [Surface.t] is a spelling - [movl $42, %eax] with its operands, before any
     decision about which encoding it becomes. [Instruction.t] is a canonical
     semantic operation, mnemonic aliases resolved and pseudo-forms still
     intact. [Lowered.t] is one real machine instruction, ready to encode; a
     pseudo-instruction became several of these. *)

  module Reg : sig
    type t
  end

  module Operand : sig
    type t
  end

  module Opcode : sig
    type t
  end

  (* {1 Errors}

     Abstract for the same reason the ASTs are: generic code moves a target's
     failures around and renders them, and must not be able to inspect one.
     Each target's domain is {!Target_error.t} plus its own tags, so the shared
     kinds - unknown mnemonic, no such form, out of range - are one row rather
     than three copies (asm/docs/errors.md §1).

     [error_diagnostic] is the erasure boundary, named so it is visible as one:
     {!Image} holds a target-supplied fixup evaluator and {!Target.DRIVER}
     erases the architecture entirely, so neither can carry this type. Below
     that call the payload survives; at it, a diagnostic is what crosses. *)

  type error

  val pp_error : Format.formatter -> error -> unit
  val error_code : error -> string
  val error_diagnostic : error -> Diagnostic.t

  (* The three staged ASTs of §4.2-§4.4. The printers are the *canonical*
     spellings asm/docs/contracts.md §1 fixes: a printer here is a contract,
     not a debugging aid, and its output is compared byte-for-byte across three
     runtimes. *)

  module Surface : sig
    type t

    val pp : Format.formatter -> t -> unit
  end

  module Instruction : sig
    type t

    val pp : Format.formatter -> t -> unit
  end

  module Lowered : sig
    type t

    val pp : Format.formatter -> t -> unit

    val equal : t -> t -> bool
    (** what the round-trip and path-coherence tests compare. Structural equality would compare
        [Origin.t] values too, and two paths that reach the same instruction from different source
        positions are the same instruction. *)
  end

  type feature
  type fixup_kind
  type target_state

  val default_state : target_state
  val default_features : feature list

  val make_surface_instruction :
    mnemonic:string -> origin:Origin.t -> Operand.t list -> (Surface.t, error) Err.t

  (* {1 Staged transformations}

     Three functions, three ASTs (§4.2-§4.4). Each is total on its input type
     and pure: same inputs, same output, no state outside [target_state]. *)

  val simplify_instruction : features:feature list -> Surface.t -> (Instruction.t, error) Err.t
  (** surface -> normalized: alias resolution, operand-order canonicalization, and the checks that
      need only the instruction itself *)

  val lower_instruction : target_state -> Instruction.t -> (Lowered.t list, error) Err.t
  (** normalized -> lowered: pseudo expansion, one to many. The list is ordered. *)

  (* {1 Encoding}

     One codec per target, holding every form as data (§4.9.1). [Codec.check]
     runs over it in the test suite; [Codec.inspect] is what [--dump-codec]
     prints; [Codec.encode] and [Codec.decode] are the two directions of this
     single tree. *)

  val codec : (Lowered.t, fixup_kind) Codec.t

  val encode :
    Lowered.t ->
    ( [ `Fixed of fixup_kind Asm_core.Lowered_ast.encoded_form
      | `Relax of fixup_kind Asm_core.Lowered_ast.encoded_form list ],
      error )
    Err.t
  (** Separate from {!codec} because packing bits into *memory order* is target-specific: a codec
      describes bits most-significant first, which for x86's byte-at-a-time forms already is memory
      order, while ARM and AArch64 author one 32-bit word and must reverse it. Putting the reversal
      in generic code would mean the generic code knew which targets are fixed-width, and putting
      it in the codec would mean [inspect] printed a form the machine does not contain.

      Converting a {!Codec.placement} into a {!Asm_core.Lowered_ast.fixup} happens here for the same
      reason: only the target knows that its bit positions have just been reversed, and only it can
      say what PC the architecture measures from.

      [`Relax] when the operand is symbolic and unpinned and the codec offers a ladder; [`Fixed]
      otherwise. The choice is made from the lowered operand, never inferred by the codec: a
      resolved displacement that fits the short form also fits the long one, so "several rungs
      succeeded" cannot distinguish them. *)

  type decode_context = { state : target_state; address : int64 }
  (** decoding needs more than the bytes: on ARM the same word is a different instruction depending
      on the mode, and a PC-relative operand cannot be printed without the address *)

  val decode : decode_context -> string -> pos:int -> (Instruction.t * string * int, error) Err.t
  (** [(instruction, form_id, bytes_consumed)]. The form id is the codec's alternative path, so a
      round-trip test can assert not just "the same instruction" but "the same encoding decision". *)

  (* {1 Linking and padding} *)

  val evaluate_fixup : fixup_kind -> place:int64 -> target:int64 -> (int64, error) Err.t
  (** the value to patch in, given where the fixup sits and where its target landed. Range checks
      belong here: a branch that does not reach is a diagnostic, not a truncation.

      [place] is already biased: the caller adds the fixup's [pc_bias], so an ARM implementation
      does not add its own 8 and an x86 one does not have to know how long the instruction it sits
      in turned out to be. *)

  (* {1 Describing a fixup kind}

     The kind is abstract to everything above, so these four are how generic
     code says anything about it at all. They are declared projections, not
     inferences: nothing recovers a role or a family by parsing a name. *)

  val fixup_kind_name : fixup_kind -> string
  (** stable, for dumps and for the relocation oracle. Used for *messages* and *artifacts*; two
      kinds are told apart by {!equal_fixup_kind}, never by comparing these. *)

  val equal_fixup_kind : fixup_kind -> fixup_kind -> bool
  (** identity. [Codec.check] groups a split fixup's slices by name and has to confirm they agree on
      the kind; it cannot use polymorphic equality on an abstract type that may one day hold a
      function, and a printed name is not identity. *)

  val fixup_family : fixup_kind -> string
  (** the semantic family two ends of a relaxation ladder share. An x86 short/near branch pair
      *must* change kind and width - that is what a ladder is - so ladder compatibility is checked
      on this rather than on the kind. *)

  val fixup_role : fixup_kind -> Asm_core.Lowered_ast.fixup_role
  (** call, branch, or data address. The oracle's classifier is indexed by role because the four
      targets disagree about which references survive assembly, and they disagree *per role*: a
      same-file global call keeps a record everywhere while a local branch keeps one nowhere. *)

  val data_widths : (string * int) list
  (** The data directives this dialect spells, and the width in *bytes* each denotes. Asked before
      the common table, because there is no universal answer: [.word] is two bytes in GNU x86 syntax
      and four on ARM and AArch64, and CompCert writes [.4byte] for a 32-bit address on ARM. A
      single shared table would be silently wrong for one of them. *)

  val data_fixup : width:int -> (fixup_kind, error) Err.t
  (** Which fixup kind an initializer naming a symbol takes at that width. An error rather than an
      option, because a width with no absolute relocation is a real limit and the caller has to say
      so rather than emit an unpatched zero. *)

  val nop_bytes : length:int -> (string, error) Err.t
  (** padding for [.align] in an executable section. Not zeroes: a zero-filled gap in x86 [.text]
      decodes as [add %al,(%rax)] and would make the diagnostic disassembler print instructions
      nobody wrote. *)
end
