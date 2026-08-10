(* The TARGET signature (.ai/asm_plan.md §5.1).

   Signatures only. This package deliberately contains no implementation, so
   that every concrete target depends on a description it cannot influence, and
   so that the architecture-independent pipeline can be written against
   something that no target can widen locally.

   Every architecture-specific type stays abstract. What the shared code is
   allowed to know about a target is exactly what appears below: a lexical
   profile, a way to get from tokens to a surface instruction, three staged
   transformations, a codec, a fixup evaluator, padding, and directive
   handling. Everything else - source-AST construction, layout iteration,
   symbol processing, image building, diagnostics rendering - is written once.

   Four refinements to §5.1's sketch, each forced by something concrete:

   1. [parse_operands] replaces [parse_surface_operand]. Operands cannot be
      parsed one at a time: [stp x15, x30, [sp, #-16]!] has a [!] that binds to
      the whole preceding memory operand, and in [add r0, r1, lsl #2] the
      [lsl #2] is a modifier of the previous operand rather than an operand.
      Both decisions need the neighbours, so the whole comma-separated list is
      the unit. It takes the mnemonic too, because [ldm sp!, {r0, r1}] and
      [.word 1, 2] tokenize identically and only the mnemonic says which
      shape is expected.

   2. [encoding_alternatives] and [encode_form] collapse into [codec]. §5.1
      wrote alternative selection as a function returning a list, which is a
      shape no interpreter can inspect: the reason a form was chosen lives
      inside OCaml code. §4.9.1 requires the opposite, so alternatives are the
      codec's own [Alt] node, with priority, guard and cost as data. The shared
      encoder calls [Codec.encode]; [Codec.check] then reports unreachable
      alternatives and overlapping patterns statically, and [Codec.decode] is
      the same tree read backwards, so a target cannot encode and decode
      differently by accident.

   3. [decode] survives separately, because a decoder needs a *context* the
      codec node does not carry - the current mode, and the address, since
      whether some bytes are ARM or Thumb is not in the bytes.

   4. [handle_directive] returns a result rather than mutating: target state is
      a value threaded through lowering, which is what makes lowering a pure
      function of (state, instruction) and therefore replayable. §5.1's own
      constraint - "must not contain global linker state" - is easier to keep
      when the state cannot be reached except by threading. *)

open Foundation

(* What a target does with a directive it was handed. [Unhandled] is not an
   error and not silence: the caller (simplify) tries the target first for
   [.syntax]/[.arch]/[.fpu]/[.arm]-style state directives, and on [Unhandled]
   falls back to the common table. A target that answered "yes, ignored" to
   everything would make every misspelled directive assemble. *)
type ('state, 'insn) directive_result =
  | Handled of { state : 'state; emit : 'insn list }
      (** the directive changed target state, and possibly emitted instructions - [.arm] after
          [.thumb] does not, but a target macro directive would *)
  | Unhandled
  | Rejected of Diagnostic.t

module type TARGET = sig
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

  (* {1 Frontend} *)

  val lexical_profile : Asm_core.Lexical_profile.t
  (** comment introducers, statement separator, sigils. The lexer is shared; only its table is
      per-target, which is why [#] can start a comment on ARM and an immediate on x86 without two
      lexers existing. *)

  val default_state : target_state
  val default_features : feature list

  val parse_operands :
    mnemonic:string -> Asm_core.Token.slice list -> (Operand.t list, Diagnostic.t) result
  (** the hand-written per-target operand parser (§4.7). Menhir has already split the line into
      comma-separated slices; what those slices mean is decided here. *)

  val make_surface_instruction :
    mnemonic:string -> origin:Origin.t -> Operand.t list -> (Surface.t, Diagnostic.t) result

  (* {1 Staged transformations}

     Three functions, three ASTs (§4.2-§4.4). Each is total on its input type
     and pure: same inputs, same output, no state outside [target_state]. *)

  val simplify_instruction :
    features:feature list -> Surface.t -> (Instruction.t, Diagnostic.t) result
  (** surface -> normalized: alias resolution, operand-order canonicalization, and the checks that
      need only the instruction itself *)

  val lower_instruction : target_state -> Instruction.t -> (Lowered.t list, Diagnostic.t) result
  (** normalized -> lowered: pseudo expansion, one to many. The list is ordered. *)

  (* {1 Encoding}

     One codec per target, holding every form as data (§4.9.1). [Codec.check]
     runs over it in the test suite; [Codec.inspect] is what [--dump-codec]
     prints; [Codec.encode] and [Codec.decode] are the two directions of this
     single tree. *)

  val codec : (Lowered.t, fixup_kind) Codec.t

  val encode : Lowered.t -> (string * string * fixup_kind Codec.placement list, Diagnostic.t) result
  (** [(bytes, form_id, placements)]. Separate from {!codec} because packing bits into *memory
      order* is target-specific: a codec describes bits most-significant first, which for x86's
      byte-at-a-time forms already is memory order, while ARM and AArch64 author one 32-bit word
      and must reverse it. Putting the reversal in generic code would mean the generic code knew
      which targets are fixed-width, and putting it in the codec would mean [inspect] printed a
      form the machine does not contain. *)

  type decode_context = { state : target_state; address : int64 }
  (** decoding needs more than the bytes: on ARM the same word is a different instruction depending
      on the mode, and a PC-relative operand cannot be printed without the address *)

  val decode :
    decode_context -> string -> pos:int -> (Instruction.t * string * int, Diagnostic.t) result
  (** [(instruction, form_id, bytes_consumed)]. The form id is the codec's alternative path, so a
      round-trip test can assert not just "the same instruction" but "the same encoding decision". *)

  (* {1 Linking and padding} *)

  val evaluate_fixup : fixup_kind -> place:int64 -> target:int64 -> (int64, Diagnostic.t) result
  (** the value to patch in, given where the fixup sits and where its target landed. Range checks
      belong here: a branch that does not reach is a diagnostic, not a truncation. *)

  val nop_bytes : length:int -> (string, Diagnostic.t) result
  (** padding for [.align] in an executable section. Not zeroes: a zero-filled gap in x86 [.text]
      decodes as [add %al,(%rax)] and would make the diagnostic disassembler print instructions
      nobody wrote. *)

  (* {1 Directives} *)

  val handle_directive :
    name:string -> argument:string -> target_state -> (target_state, Instruction.t) directive_result
end

(* The architecture-erased view (§5.1's DRIVER). Its whole purpose is that the
   CLI and the internal linker never mention a target-specific type, so no
   existential escapes into them. Defined here rather than in [driver] because
   it is part of the same contract: it says which requests survive erasure. *)
module type DRIVER = sig
  val name : string
  val triple : string

  val assemble :
    unit_name:string -> source:Span.source -> (Image.laid_out, Diagnostic.t list) result
  (** Text in, laid-out image out - laid out but *not bound*: choosing addresses is the host's
      step and stays outside every target (§9). [Image.bind_image] is what turns the result into
      bytes at an address, and it is deliberately not part of this interface, because it is not
      target-specific at all. *)

  (* The six dumps of asm/docs/contracts.md §1. Each is a separate entry point
     rather than a [~stage] parameter, because they have genuinely different
     inputs: three take source text, two take bytes and an address, and one
     takes nothing. *)

  val dump_tokens : source:Span.source -> (string, Diagnostic.t list) result
  val dump_source_ast : unit_name:string -> source:Span.source -> (string, Diagnostic.t list) result

  val dump_normalized_ast :
    unit_name:string -> source:Span.source -> (string, Diagnostic.t list) result

  val dump_lowered_ast :
    unit_name:string -> source:Span.source -> (string, Diagnostic.t list) result

  val dump_disasm_canonical : address:int64 -> string -> (string, Diagnostic.t list) result
  (** exactly re-parseable: feeding this back through the assembler must produce the identical
      image, and a round-trip test does exactly that *)

  val dump_disasm_diagnostic : address:int64 -> string -> (string, Diagnostic.t list) result
  (** address, bytes, spelling and form id in columns. Read by humans and by the differential gate;
      **not** re-parseable, and no test may re-parse it - a format that had to satisfy both
      audiences would satisfy neither. *)

  val dump_codec : unit -> string

  val check_codec : unit -> string list
  (** [Codec.check] rendered. A driver that cannot be asked whether its own encoding tables are
      consistent is one whose tables are only tested where a fixture happens to look. *)
end
