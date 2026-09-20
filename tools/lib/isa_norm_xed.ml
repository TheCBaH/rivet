open Isa_norm_model
module R = Isa_source_record

let err rule message : (_, diagnostic) result = Error { rule; message }

let x86_encoding_of (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; _ } ->
      Ok (X86_encoding { space; opcode_map; opcode; pattern })
  | _ -> Error "record's encoding is not XED x86_encoding"

let xed_provenance_of (rec_ : R.t) =
  match rec_.provenance with
  | R.Xed_provenance p -> Ok p
  | _ -> Error "record's provenance is not a XED provenance"

(* provenance.mode_restriction is a raw native fact (an "unspecified" string
   or, for a mode-gated form, XED's own internal integer mode id - see
   Isa_source_record's docstring); applicability is the adapter's own derived
   three-valued expression over it (isa-db/schema §applicability) and is what
   gets threaded into {!Isa_norm_model.requirement} instead of
   re-interpreting the raw fact here. requirement_of_applicability folds
   trivial App_all[] members away so an unconditional BASE/X87 form's
   requirement stays exactly [Req_all []]/[Req_feature "x86:x87"], matching
   the pilot forms byte-for-byte rather than growing a redundant
   [Req_all [Req_feature _; Req_all []]] wrapper. *)
let rec requirement_of_applicability : R.applicability -> requirement = function
  | R.App_all cs -> Req_all (List.map requirement_of_applicability cs)
  | R.App_any cs -> Req_any (List.map requirement_of_applicability cs)
  | R.App_mode { mode; equals } -> Req_mode { mode; equals }
  | R.App_unknown msg -> Req_unknown msg

let req_and a b = match (a, b) with Req_all [], x | x, Req_all [] -> x | _ -> Req_all [ a; b ]

let requirement_of (rec_ : R.t) =
  let applic () = requirement_of_applicability rec_.applicability in
  match xed_provenance_of rec_ with
  | Ok { extension = Some "BASE"; _ } -> applic ()
  | Ok { extension = Some "X87"; _ } -> req_and (Req_feature "x86:x87") (applic ())
  | Ok { extension = Some ext; _ } -> Req_unknown (Printf.sprintf "unmapped XED extension: %s" ext)
  | Ok { extension = None; _ } -> Req_unknown "XED record has no provenance.extension"
  | Error msg -> Req_unknown msg

(* ADD_GPRv_IMMz: REG0 (GPRv_B, rw) is the sole GAS-visible destination
   register; IMM0 (imm_const, oc2 "z") is a 16- or 32-bit immediate selected
   by effective operand size - XED's own resolved facts do not resolve that
   selection, so its width is left unstated rather than guessed (diagnostic
   below): a parsed operand's bits value must be
   interpreted using its native type; it is not universally a literal
   machine operand value. AT&T operand order (immediate first, destination
   last) is this rule's own domain knowledge, not a source fact - see the
   Inferred fact. *)
let add_gprv_immz_form (rec_ : R.t) =
  match x86_encoding_of rec_ with
  | Error msg -> err "ADD_GPRv_IMMz-not-x86-encoding" msg
  | Ok encoding ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_gpr; excluded = [] };
          role = In_out;
          explicit = true;
        }
      in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 0;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs = [];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "x86:ADD_GPRv_IMMz";
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ dest; imm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic = "add";
              operands =
                [ Syn_decorated ("$", Syn_operand "imm"); Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "REG0 (GPRv_B, rw), IMM0 (imm_const, oc2 z) taken verbatim from encoding.operands";
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (source immediate, then destination register) is GAS \
                   convention, not a XED fact";
              };
            ];
          diagnostics =
            [
              {
                rule = "xed-imm-width-oc2-z";
                message =
                  "XED's oc2 'z' denotes a 16- or 32-bit immediate selected by effective operand \
                   size; this normalization does not resolve that selection, so imm.width_bits is \
                   left 0/unstated rather than guessed";
              };
            ];
        }

(* The rest of the register/immediate ALU family sharing ADD_GPRv_IMMz's
   own shape (OR/ADC/SBB/AND/SUB/XOR/CMP_GPRv_IMMz) - unlike
   ADD_GPRv_IMMz's own deliberate bare-mnemonic design test (see
   Isa_norm_model.mli's own docstring), each of these fixes its mnemonic
   to the explicit 32-bit spelling {!two_operand_gprv_form}'s own
   register-register ALU dispatch already uses for the same reason:
   confirmed against real GNU as and this project's own encoder, none of
   these seven mnemonics assemble bare either. *)
let alu_gprv_immz_form ~form_id ~mnemonic (rec_ : R.t) =
  match x86_encoding_of rec_ with
  | Error msg -> err (form_id ^ "-not-x86-encoding") msg
  | Ok encoding ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_gpr; excluded = [] };
          role = In_out;
          explicit = true;
        }
      in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 0;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs = [];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ dest; imm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_decorated ("$", Syn_operand "imm"); Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "REG0 (GPRv_B, rw), IMM0 (imm_const, oc2 z) taken verbatim from encoding.operands";
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (source immediate, then destination register) is GAS \
                   convention, not a XED fact";
              };
            ];
          diagnostics =
            [
              {
                rule = "xed-imm-width-oc2-z";
                message =
                  "XED's oc2 'z' denotes a 16- or 32-bit immediate selected by effective operand \
                   size; this normalization does not resolve that selection, so imm.width_bits is \
                   left 0/unstated rather than guessed";
              };
            ];
        }

(* The register/immediate ALU family's imm8 rung (ADD/OR/ADC/SBB/AND/SUB/
   XOR/CMP_GPRv_IMMb, opcode 0x83): {!alu_gprv_immz_form}'s own shape with
   IMM0's oc2 "b" in place of "z" - GAS picks this opcode automatically
   whenever the immediate value fits a signed byte (confirmed against real
   GNU as: [addl $5, %ecx] -> [83 c1 05] versus [addl $1000000, %ecx] ->
   [81 ...]), so this iform's own reconstructed immediate value is still
   sign-extended to the destination's operand-size-dependent width - "b"
   names the encoded field's width, not the reconstructed value's, the same
   reason {!alu_gprv_immz_form}'s own "z" case leaves width_bits unstated. *)
let alu_gprv_immb_form ~form_id ~mnemonic (rec_ : R.t) =
  match x86_encoding_of rec_ with
  | Error msg -> err (form_id ^ "-not-x86-encoding") msg
  | Ok encoding ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_gpr; excluded = [] };
          role = In_out;
          explicit = true;
        }
      in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 0;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs = [];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ dest; imm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_decorated ("$", Syn_operand "imm"); Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "REG0 (GPRv_B, rw), IMM0 (imm_const, oc2 b) taken verbatim from encoding.operands";
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (source immediate, then destination register) is GAS \
                   convention, not a XED fact";
              };
            ];
          diagnostics =
            [
              {
                rule = "xed-imm-width-oc2-b";
                message =
                  "XED's oc2 'b' names this iform's own encoded-field width (a byte), not its \
                   reconstructed value's width - GNU as sign-extends it to the destination's own \
                   operand-size-dependent width, which this normalization does not resolve, so \
                   imm.width_bits is left 0/unstated rather than guessed, matching the 'z' rung's \
                   own precedent";
              };
            ];
        }

(* FADD_ST0_X87: REG0 is the implicit, IMPLICIT-visibility ST0 (read-write);
   REG1 is the explicit ST(i) source via the X87 lookup function; REG2 is a
   SUPPRESSED status-word write, never spelled in assembly text. This rule
   is spelled [fadd %st(i), %st] in GAS AT&T syntax.  This is distinct from
   FADD_X87_ST0, whose destination is ST(i), and is verified by the
   generated differential corpus before this rule grants support credit. *)
let fadd_st0_x87_form (rec_ : R.t) =
  match x86_encoding_of rec_ with
  | Error msg -> err "FADD_ST0_X87-not-x86-encoding" msg
  | Ok encoding ->
      let dest =
        {
          op_name = "st0";
          op_kind = Implicit_register { class_ = X87_st; native_name = "XED_REG_ST0" };
          role = In_out;
          explicit = false;
        }
      in
      let src =
        {
          op_name = "src";
          op_kind = Register { class_ = X87_st; excluded = [] };
          role = In;
          explicit = true;
        }
      in
      let status =
        {
          op_name = "status";
          op_kind = Implicit_register { class_ = X87_st; native_name = "XED_REG_X87STATUS" };
          role = Out;
          explicit = false;
        }
      in
      Ok
        {
          form_id = "x86:FADD_ST0_X87";
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ dest; src; status ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic = "fadd";
              operands =
                [
                  Syn_group [ Syn_literal "%st("; Syn_operand "src"; Syn_literal ")" ];
                  Syn_literal "%st";
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "REG0 (ST0, IMPLICIT, rw), REG1 (X87 lookup, DEFAULT, r), REG2 (X87STATUS, \
                   SUPPRESSED, w) taken verbatim from encoding.operands";
              };
            ];
          diagnostics = [];
        }

let role_of_rw = function "r" -> In | "w" -> Out | _ -> In_out

(* XED reports [MEM0] as an [imm_const] operand with [oc2 = v]; its name and
   MOD!=3 pattern establish that it is memory, while the [v] data width stays
   variable.  This deliberately admits only the 32-bit GAS spelling [movl]
   on each target, so the source form remains visible without pretending that
   XED selected a universal operand size. *)
let mov_gprv_memv_form ~form_id ~load (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when (a.op_name = "REG0" && b.op_name = "MEM0") || (a.op_name = "MEM0" && b.op_name = "REG0") ->
      let reg =
        {
          op_name = "reg";
          op_kind = Register { class_ = X86_gpr; excluded = [] };
          role = (if load then Out else In);
          explicit = true;
        }
      in
      let mem =
        {
          op_name = "mem";
          op_kind = Memory { width_bits = None };
          role = (if load then In else Out);
          explicit = true;
        }
      in
      let operands, syntax_operands =
        if load then ([ mem; reg ], [ Syn_operand "mem"; Syn_decorated ("%", Syn_operand "reg") ])
        else ([ reg; mem ], [ Syn_decorated ("%", Syn_operand "reg"); Syn_operand "mem" ])
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding = X86_encoding { space; opcode_map; opcode; pattern };
          operands;
          syntax = { dialect = "gas-att"; mnemonic = "movl"; operands = syntax_operands };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "REG0 and MEM0 plus MOD!=3 taken verbatim from encoding.operands/pattern";
              };
              {
                label = Inferred;
                note =
                  "This bounds the recipe to explicit 32-bit AT&T movl; XED oc2 v remains \
                   source-variable";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/MEM0 operands, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* The register<-memory ALU direction (ADD_GPRv_MEMv/ADC_GPRv_MEMv/
   XOR_GPRv_MEMv): {!mov_gprv_memv_form}'s own shape, generalized to read
   each operand's real [rw] fact instead of hardcoding write-only/read-only
   roles - unlike MOV, these read-modify-write the destination register
   (REG0's rw is "rw", not "w"), so a hardcoded [Out] would misreport it.
   XED always orders REG0 before MEM0 for this iform family (unlike
   {!two_operand_gprv_form}'s two GPRv operands, there is only ever one
   register<-memory direction present per mnemonic here - the reverse
   MEMv<-GPRv direction, e.g. ADD_MEMv_GPRv, is a separate shape,
   {!alu_memv_gprv_form} below). Deliberately
   admits only the explicit 32-bit AT&T spelling on each target, the same
   bound {!mov_gprv_memv_form} already uses. *)
let alu_gprv_memv_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "REG0" && b.op_name = "MEM0" ->
      let reg =
        {
          op_name = "reg";
          op_kind = Register { class_ = X86_gpr; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let mem =
        {
          op_name = "mem";
          op_kind = Memory { width_bits = None };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding = X86_encoding { space; opcode_map; opcode; pattern };
          operands = [ mem; reg ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "mem"; Syn_decorated ("%", Syn_operand "reg") ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf "REG0 (rw=%s), MEM0 (rw=%s) taken verbatim from encoding.operands"
                    a.rw b.rw;
              };
              {
                label = Inferred;
                note =
                  "This bounds the recipe to explicit 32-bit AT&T spelling; XED oc2 v remains \
                   source-variable";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/MEM0 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* The reverse, MEMv<-GPRv, ALU direction (ADD_MEMv_GPRv/ADC_MEMv_GPRv/.../
   TEST_MEMv_GPRv): {!alu_gprv_memv_form}'s own shape with the mem/reg roles
   swapped - XED orders MEM0 before REG0 here (the opposite of
   {!alu_gprv_memv_form}'s own REG0-before-MEM0), and AT&T puts the register
   source first, the memory destination last ([addl %eax, 0x10(%esp)]), the
   reverse of that function's own [mem, reg] order too. `Opcode.to_rm_r`'s
   opcode table already covers this direction (x86_family_encode.ml's own
   comment on the new [Alu_rm_r] lowering arm explains why: a memory r/m and
   a register r/m share one opcode per operation, distinguished only by
   ModR/M's mod field), so this needed no new encoder table, only the new
   lowering arm and this normalization function. [Test] is read-only on
   both operands (MEM0's own rw is "r", not "rw" the way the other eight
   ops' MEM0 is) but is otherwise the same shape - real GNU as: [testl
   %eax, 0x10(%esp)] -> [85 44 24 10], the same opcode [TEST_GPRv_GPRv]
   already uses. *)
let alu_memv_gprv_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "MEM0" && b.op_name = "REG0" ->
      let mem =
        {
          op_name = "mem";
          op_kind = Memory { width_bits = None };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let reg =
        {
          op_name = "reg";
          op_kind = Register { class_ = X86_gpr; excluded = [] };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding = X86_encoding { space; opcode_map; opcode; pattern };
          operands = [ reg; mem ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_decorated ("%", Syn_operand "reg"); Syn_operand "mem" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf "MEM0 (rw=%s), REG0 (rw=%s) taken verbatim from encoding.operands"
                    a.rw b.rw;
              };
              {
                label = Inferred;
                note =
                  "This bounds the recipe to explicit 32-bit AT&T spelling; XED oc2 v remains \
                   source-variable";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected MEM0/REG0 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* The MEMv<-IMMb/IMMz ALU-immediate direction (ADD/OR/ADC/SBB/AND/SUB/XOR/
   CMP_MEMv_IMMb and _MEMv_IMMz, opcodes 0x83/0x81 with a memory r/m):
   {!alu_gprv_immb_form}/{!alu_gprv_immz_form}'s own shape with the register
   destination swapped for {!alu_gprv_memv_form}'s MEM0 operand - XED reports
   MEM0 here as an [imm_const] operand exactly the way {!mov_gprv_memv_form}'s
   own doc comment already explains, so this reuses that same MOD!=3/name
   evidence rather than a genuine memory-typed source field. This project's
   own encoder already lowers [Operand.Mem] destinations for every one of
   these eight mnemonics through the same [Lowered.Alu_rm_imm] codec
   {!alu_gprv_immb_form}'s register destination uses (x86_family_encode.ml's
   [lower_instruction], the [Opcode.Add | ... ], [Imm v; Mem m] case) with no
   further encoder change needed, unlike the register<-memory
   [Alu_r_rm]/[Opcode.to_r_rm] direction that {!alu_gprv_memv_form}'s own
   doc comment reports as still encoder-unsupported in the other direction. *)
let alu_memv_imm_form ~form_id ~mnemonic ~oc2 (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "MEM0" && b.op_name = "IMM0" ->
      let mem =
        {
          op_name = "mem";
          op_kind = Memory { width_bits = None };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 0;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs = [];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding = X86_encoding { space; opcode_map; opcode; pattern };
          operands = [ imm; mem ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_decorated ("$", Syn_operand "imm"); Syn_operand "mem" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "MEM0 (imm_const, oc2 %s, rw=%s), IMM0 (imm_const, oc2 %s, rw=%s) taken \
                     verbatim from encoding.operands"
                    (Option.value ~default:"?" a.oc2) a.rw (Option.value ~default:"?" b.oc2) b.rw;
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (source immediate, then destination memory) is GAS \
                   convention, not a XED fact";
              };
            ];
          diagnostics =
            [
              {
                rule = Printf.sprintf "xed-imm-width-oc2-%s" oc2;
                message =
                  Printf.sprintf
                    "XED's oc2 '%s' names this iform's own encoded-field width, not its \
                     reconstructed value's width, which GNU as sign-extends to the destination's \
                     own operand-size-dependent width; this normalization does not resolve that, \
                     so imm.width_bits is left 0/unstated rather than guessed"
                    oc2;
              };
            ];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected MEM0/IMM0 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* GRP1's own byte-operand rung of the register/immediate ALU family
   (ADD/OR/ADC/SBB/AND/SUB/XOR/CMP_GPR8_IMMb_80r<N>, opcode 0x80):
   {!alu_gprv_immb_form}'s own shape, but REG0's real lookup function is
   "GPR8_B", not "GPRv_B" - a genuinely different opcode from the 0x83/0x81
   rungs (x86_family_encode.ml's own {!alu_form_byte} doc comment explains
   why 8-bit operands need a dedicated opcode rather than a narrower reading
   of either), so this is its own function rather than a third case
   {!alu_gprv_immb_form} dispatches on. The undocumented [0x82] alias
   (`ADD_GPR8_IMMb_82r0` etc., 64-bit-invalid, XED's own applicability marks
   it [not64]) is never GAS-selected and is deliberately not admitted, the
   same reverse-iform precedent this file already applies elsewhere. *)
let alu_gpr8_immb_form ~form_id ~mnemonic (rec_ : R.t) =
  match x86_encoding_of rec_ with
  | Error msg -> err (form_id ^ "-not-x86-encoding") msg
  | Ok encoding ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_gpr; excluded = [] };
          role = In_out;
          explicit = true;
        }
      in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 0;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs = [];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ dest; imm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_decorated ("$", Syn_operand "imm"); Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "REG0 (GPR8_B, rw), IMM0 (imm_const, oc2 b) taken verbatim from encoding.operands";
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (source immediate, then destination register) is GAS \
                   convention, not a XED fact";
              };
            ];
          diagnostics = [];
        }

(* The [0x80] rung's memory-destination sibling
   (ADD/OR/ADC/SBB/AND/SUB/XOR/CMP_MEMb_IMMb_80r<N>): {!alu_memv_imm_form}'s
   own MEM0/IMM0 shape at the single byte-width rung ("b" is the only
   immediate width this opcode ever carries, unlike {!alu_memv_imm_form}'s
   own 0x83/0x81 pair, so there is no ~oc2 to parameterize). This project's
   own encoder already lowers a memory ALU-immediate destination through
   {!Lowered.Alu_rm_imm} regardless of width (x86_family_encode.ml's
   [lower_instruction] does not gate the [Mem]-destination arm on width), so
   only {!alu_form_byte}'s own new [0x80] codec rung was needed, no further
   normalization-side special-casing beyond this shape. *)
let alu_memb_immb_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "MEM0" && b.op_name = "IMM0" ->
      let mem =
        {
          op_name = "mem";
          op_kind = Memory { width_bits = None };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 0;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs = [];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding = X86_encoding { space; opcode_map; opcode; pattern };
          operands = [ imm; mem ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_decorated ("$", Syn_operand "imm"); Syn_operand "mem" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "MEM0 (imm_const, oc2 %s, rw=%s), IMM0 (imm_const, oc2 %s, rw=%s) taken \
                     verbatim from encoding.operands"
                    (Option.value ~default:"?" a.oc2) a.rw (Option.value ~default:"?" b.oc2) b.rw;
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (source immediate, then destination memory) is GAS \
                   convention, not a XED fact";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected MEM0/IMM0 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* The accumulator-immediate byte rung (ADD/OR/ADC/SBB/AND/SUB/XOR/CMP_AL_IMMb,
   opcode [ext<<3 | 4]): {!fadd_st0_x87_form}'s own [Implicit_register] shape
   for XED's IMPLICIT-visibility, fixed-[bits] REG0 ("XED_REG_AL"), rather
   than {!alu_gpr8_immb_form}'s variable-lookup [Register] - AT&T syntax still
   spells [%al] explicitly, so the fixed name is a [Syn_literal], not a
   [Syn_operand]. x86_family_encode.ml's own {!alu_acc_form_byte} doc comment
   explains why real GNU as always prefers this two-byte form over
   {!alu_form_byte}'s three-byte ModR/M one whenever the destination is
   [%al], unlike {!alu_acc_form}'s own imm8-fits-check-gated preference at
   width 32/64. *)
let alu_al_immb_form ~form_id ~mnemonic (rec_ : R.t) =
  match x86_encoding_of rec_ with
  | Error msg -> err (form_id ^ "-not-x86-encoding") msg
  | Ok encoding ->
      let dest =
        {
          op_name = "dest";
          op_kind = Implicit_register { class_ = X86_gpr; native_name = "XED_REG_AL" };
          role = In_out;
          explicit = false;
        }
      in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 0;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs = [];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ dest; imm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_decorated ("$", Syn_operand "imm"); Syn_literal "%al" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "REG0 (reg, IMPLICIT, bits XED_REG_AL, rw), IMM0 (imm_const, oc2 b) taken \
                   verbatim from encoding.operands";
              };
            ];
          diagnostics = [];
        }

(* Generic two-operand GPRv ALU/MOV forms (the legacy
   register/register and register/immediate x86 pilot): exactly one of
   [encoding.operands] is a GPRv-lookup register or an oc2 "z" immediate
   acting as the read source, the other a GPRv-lookup register acting as the
   read-write or write-only destination. AT&T syntax puts the source first
   regardless of which XED operand slot (REG0 vs REG1) the pattern happens to
   assign it to - this reads that role from each operand's own [rw] fact
   (now captured in {!Isa_source_record.x86_operand}) rather than assuming a
   fixed REG0/REG1 position: a parsed operand's bits value
   must be interpreted using its native type. *)
let is_gprv_reg (o : R.x86_operand) =
  o.op_type = "nt_lookup_fn"
  && match o.lookupfn_name with Some fn -> String.starts_with ~prefix:"GPRv" fn | None -> false

let is_immz (o : R.x86_operand) = o.op_type = "imm_const" && o.oc2 = Some "z"

let norm_operand_of (o : R.x86_operand) name =
  if is_immz o then
    {
      op_name = name;
      op_kind =
        Immediate
          { width_bits = 0; signed = true; implicit_low_zero_bits = 0; nonzero = false; runs = [] };
      role = role_of_rw o.rw;
      explicit = true;
    }
  else
    {
      op_name = name;
      op_kind = Register { class_ = X86_gpr; excluded = [] };
      role = role_of_rw o.rw;
      explicit = true;
    }

let two_operand_gprv_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when (is_gprv_reg a || is_immz a) && (is_gprv_reg b || is_immz b) -> (
      let build ~source_raw ~dest_raw =
        let source = norm_operand_of source_raw "src" in
        let dest = norm_operand_of dest_raw "dest" in
        let decorator (op : operand) = match op.op_kind with Immediate _ -> "$" | _ -> "%" in
        let imm_diagnostics =
          if is_immz a || is_immz b then
            [
              {
                rule = "xed-imm-width-oc2-z";
                message =
                  "XED's oc2 'z' denotes a 16- or 32-bit immediate selected by effective operand \
                   size; this normalization does not resolve that selection, so the immediate \
                   operand's width_bits is left 0/unstated rather than guessed";
              };
            ]
          else []
        in
        Ok
          {
            form_id = "x86:" ^ form_id;
            arch = X86;
            native_name = rec_.native_name;
            source_record_ids = [ rec_.record_id ];
            requirement = requirement_of rec_;
            encoding = X86_encoding { space; opcode_map; opcode; pattern };
            operands = [ source; dest ];
            syntax =
              {
                dialect = "gas-att";
                mnemonic;
                operands =
                  [
                    Syn_decorated (decorator source, Syn_operand source.op_name);
                    Syn_decorated (decorator dest, Syn_operand dest.op_name);
                  ];
              };
            concreteness = Concrete;
            facts =
              [
                {
                  label = Upstream;
                  note =
                    Printf.sprintf
                      "%s (%s, rw=%s), %s (%s, rw=%s) taken verbatim from encoding.operands"
                      a.op_name a.op_type a.rw b.op_name b.op_type b.rw;
                };
                {
                  label = Inferred;
                  note =
                    "AT&T operand order (source, then destination) is GAS convention, not a XED \
                     fact";
                };
              ];
            diagnostics = imm_diagnostics;
          }
      in
      match (a.rw, b.rw) with
      | "r", ("w" | "rw") -> build ~source_raw:a ~dest_raw:b
      | ("w" | "rw"), "r" -> build ~source_raw:b ~dest_raw:a
      (* CMP/TEST write neither operand (both compare-only, rw="r"): [a]/[b]
         still carry the same REG0=GPRv_B-first, REG1=GPRv_R-second
         positional layout every other member of this opcode family
         (09/11/19/21/29/31/39/85, the [to_rm_r] direction) already
         establishes, so this keeps [a] (GPRv_B) as "dest" and [b] (GPRv_R)
         as "src" by that same positional convention rather than rw, matching
         real GNU as's own AT&T rendering [cmp %src, %dest]. *)
      | "r", "r" -> build ~source_raw:b ~dest_raw:a
      | rw_a, rw_b ->
          err (form_id ^ "-unrecognized-rw")
            (Printf.sprintf "expected one 'r' and one 'w'/'rw' operand, got rw=%s/rw=%s" rw_a rw_b))
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected exactly 2 GPRv-register/immz operands, got %d"
           (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

let normalize (rec_ : R.t) =
  match xed_provenance_of rec_ with
  | Ok { iform = Some "ADD_GPRv_IMMz"; _ } -> add_gprv_immz_form rec_
  | Ok { iform = Some "FADD_ST0_X87"; _ } -> fadd_st0_x87_form rec_
  | Ok { iform = Some "MOV_GPRv_MEMv"; _ } ->
      mov_gprv_memv_form ~form_id:"MOV_GPRv_MEMv" ~load:true rec_
  | Ok { iform = Some "MOV_MEMv_GPRv"; _ } ->
      mov_gprv_memv_form ~form_id:"MOV_MEMv_GPRv" ~load:false rec_
  | Ok { iform = Some "ADD_GPRv_GPRv_01"; _ } ->
      two_operand_gprv_form ~form_id:"ADD_GPRv_GPRv_01" ~mnemonic:"add" rec_
  | Ok { iform = Some "ADD_GPRv_GPRv_03"; _ } ->
      two_operand_gprv_form ~form_id:"ADD_GPRv_GPRv_03" ~mnemonic:"add" rec_
  | Ok { iform = Some "MOV_GPRv_GPRv_89"; _ } ->
      two_operand_gprv_form ~form_id:"MOV_GPRv_GPRv_89" ~mnemonic:"mov" rec_
  | Ok { iform = Some "MOV_GPRv_GPRv_8B"; _ } ->
      two_operand_gprv_form ~form_id:"MOV_GPRv_GPRv_8B" ~mnemonic:"mov" rec_
  | Ok { iform = Some "MOV_GPRv_IMMz"; _ } ->
      two_operand_gprv_form ~form_id:"MOV_GPRv_IMMz" ~mnemonic:"mov" rec_
  (* The plain register-register ALU family sharing ADD_GPRv_GPRv_01's own
     to_rm_r opcode-selection precedent (real GNU as always picks this
     "low-numbered" iform for two register operands regardless of AT&T
     argument order; confirmed for every one of these eight mnemonics, not
     assumed from ADD alone). Unlike [add], none of these mnemonics assemble
     bare in this project's own x86 frontend - confirmed against both real
     GNU as and this project's own encoder - so each fixes its mnemonic to
     the explicit 32-bit spelling {!mov_gprv_memv_form} already uses for
     MOV_GPRv_MEMv/MOV_MEMv_GPRv, rather than reusing ADD_GPRv_GPRv_01's own
     bare "add". The reverse "_0B"/"_13"/"_1B"/"_23"/"_2B"/"_33"/"_3B" iforms
     stay unhandled, matching ADD_GPRv_GPRv_03's own precedent: GNU as never
     selects them for two register operands under any argument order. *)
  | Ok { iform = Some "SUB_GPRv_GPRv_29"; _ } ->
      two_operand_gprv_form ~form_id:"SUB_GPRv_GPRv_29" ~mnemonic:"subl" rec_
  | Ok { iform = Some "AND_GPRv_GPRv_21"; _ } ->
      two_operand_gprv_form ~form_id:"AND_GPRv_GPRv_21" ~mnemonic:"andl" rec_
  | Ok { iform = Some "OR_GPRv_GPRv_09"; _ } ->
      two_operand_gprv_form ~form_id:"OR_GPRv_GPRv_09" ~mnemonic:"orl" rec_
  | Ok { iform = Some "XOR_GPRv_GPRv_31"; _ } ->
      two_operand_gprv_form ~form_id:"XOR_GPRv_GPRv_31" ~mnemonic:"xorl" rec_
  | Ok { iform = Some "ADC_GPRv_GPRv_11"; _ } ->
      two_operand_gprv_form ~form_id:"ADC_GPRv_GPRv_11" ~mnemonic:"adcl" rec_
  | Ok { iform = Some "SBB_GPRv_GPRv_19"; _ } ->
      two_operand_gprv_form ~form_id:"SBB_GPRv_GPRv_19" ~mnemonic:"sbbl" rec_
  | Ok { iform = Some "CMP_GPRv_GPRv_39"; _ } ->
      two_operand_gprv_form ~form_id:"CMP_GPRv_GPRv_39" ~mnemonic:"cmpl" rec_
  | Ok { iform = Some "TEST_GPRv_GPRv"; _ } ->
      two_operand_gprv_form ~form_id:"TEST_GPRv_GPRv" ~mnemonic:"testl" rec_
  (* The register<-memory ALU direction: every to_r_rm opcode this
     project's encoder lowers with a memory source ({!alu_gprv_memv_form}'s
     own doc comment) except TEST, whose own GPRv_MEMv form has no
     upstream-named XED iform to admit at all (real GNU as does accept it,
     see x86_family_encode.ml's own Opcode.to_r_rm comment). *)
  | Ok { iform = Some "ADD_GPRv_MEMv"; _ } ->
      alu_gprv_memv_form ~form_id:"ADD_GPRv_MEMv" ~mnemonic:"addl" rec_
  | Ok { iform = Some "ADC_GPRv_MEMv"; _ } ->
      alu_gprv_memv_form ~form_id:"ADC_GPRv_MEMv" ~mnemonic:"adcl" rec_
  | Ok { iform = Some "XOR_GPRv_MEMv"; _ } ->
      alu_gprv_memv_form ~form_id:"XOR_GPRv_MEMv" ~mnemonic:"xorl" rec_
  | Ok { iform = Some "SUB_GPRv_MEMv"; _ } ->
      alu_gprv_memv_form ~form_id:"SUB_GPRv_MEMv" ~mnemonic:"subl" rec_
  | Ok { iform = Some "AND_GPRv_MEMv"; _ } ->
      alu_gprv_memv_form ~form_id:"AND_GPRv_MEMv" ~mnemonic:"andl" rec_
  | Ok { iform = Some "OR_GPRv_MEMv"; _ } ->
      alu_gprv_memv_form ~form_id:"OR_GPRv_MEMv" ~mnemonic:"orl" rec_
  | Ok { iform = Some "SBB_GPRv_MEMv"; _ } ->
      alu_gprv_memv_form ~form_id:"SBB_GPRv_MEMv" ~mnemonic:"sbbl" rec_
  | Ok { iform = Some "CMP_GPRv_MEMv"; _ } ->
      alu_gprv_memv_form ~form_id:"CMP_GPRv_MEMv" ~mnemonic:"cmpl" rec_
  (* The reverse, MEMv<-GPRv, direction ({!alu_memv_gprv_form}'s own doc
     comment): every `to_rm_r` opcode, including TEST this time - unlike
     the GPRv_MEMv load direction above, XED does export a TEST_MEMv_GPRv
     record. *)
  | Ok { iform = Some "ADD_MEMv_GPRv"; _ } ->
      alu_memv_gprv_form ~form_id:"ADD_MEMv_GPRv" ~mnemonic:"addl" rec_
  | Ok { iform = Some "OR_MEMv_GPRv"; _ } ->
      alu_memv_gprv_form ~form_id:"OR_MEMv_GPRv" ~mnemonic:"orl" rec_
  | Ok { iform = Some "ADC_MEMv_GPRv"; _ } ->
      alu_memv_gprv_form ~form_id:"ADC_MEMv_GPRv" ~mnemonic:"adcl" rec_
  | Ok { iform = Some "SBB_MEMv_GPRv"; _ } ->
      alu_memv_gprv_form ~form_id:"SBB_MEMv_GPRv" ~mnemonic:"sbbl" rec_
  | Ok { iform = Some "AND_MEMv_GPRv"; _ } ->
      alu_memv_gprv_form ~form_id:"AND_MEMv_GPRv" ~mnemonic:"andl" rec_
  | Ok { iform = Some "SUB_MEMv_GPRv"; _ } ->
      alu_memv_gprv_form ~form_id:"SUB_MEMv_GPRv" ~mnemonic:"subl" rec_
  | Ok { iform = Some "XOR_MEMv_GPRv"; _ } ->
      alu_memv_gprv_form ~form_id:"XOR_MEMv_GPRv" ~mnemonic:"xorl" rec_
  | Ok { iform = Some "CMP_MEMv_GPRv"; _ } ->
      alu_memv_gprv_form ~form_id:"CMP_MEMv_GPRv" ~mnemonic:"cmpl" rec_
  | Ok { iform = Some "TEST_MEMv_GPRv"; _ } ->
      alu_memv_gprv_form ~form_id:"TEST_MEMv_GPRv" ~mnemonic:"testl" rec_
  | Ok { iform = Some "OR_GPRv_IMMz"; _ } ->
      alu_gprv_immz_form ~form_id:"OR_GPRv_IMMz" ~mnemonic:"orl" rec_
  | Ok { iform = Some "ADC_GPRv_IMMz"; _ } ->
      alu_gprv_immz_form ~form_id:"ADC_GPRv_IMMz" ~mnemonic:"adcl" rec_
  | Ok { iform = Some "SBB_GPRv_IMMz"; _ } ->
      alu_gprv_immz_form ~form_id:"SBB_GPRv_IMMz" ~mnemonic:"sbbl" rec_
  | Ok { iform = Some "AND_GPRv_IMMz"; _ } ->
      alu_gprv_immz_form ~form_id:"AND_GPRv_IMMz" ~mnemonic:"andl" rec_
  | Ok { iform = Some "SUB_GPRv_IMMz"; _ } ->
      alu_gprv_immz_form ~form_id:"SUB_GPRv_IMMz" ~mnemonic:"subl" rec_
  | Ok { iform = Some "XOR_GPRv_IMMz"; _ } ->
      alu_gprv_immz_form ~form_id:"XOR_GPRv_IMMz" ~mnemonic:"xorl" rec_
  | Ok { iform = Some "CMP_GPRv_IMMz"; _ } ->
      alu_gprv_immz_form ~form_id:"CMP_GPRv_IMMz" ~mnemonic:"cmpl" rec_
  (* The imm8 rung of the same register/immediate ALU family (opcode 0x83,
     {!alu_gprv_immb_form}'s own doc comment) - includes ADD this time, unlike
     the "z" rung's dispatch above: ADD_GPRv_IMMz's bare "add" was a
     deliberate one-off design test, not a precedent this rung repeats. *)
  | Ok { iform = Some "ADD_GPRv_IMMb"; _ } ->
      alu_gprv_immb_form ~form_id:"ADD_GPRv_IMMb" ~mnemonic:"addl" rec_
  | Ok { iform = Some "OR_GPRv_IMMb"; _ } ->
      alu_gprv_immb_form ~form_id:"OR_GPRv_IMMb" ~mnemonic:"orl" rec_
  | Ok { iform = Some "ADC_GPRv_IMMb"; _ } ->
      alu_gprv_immb_form ~form_id:"ADC_GPRv_IMMb" ~mnemonic:"adcl" rec_
  | Ok { iform = Some "SBB_GPRv_IMMb"; _ } ->
      alu_gprv_immb_form ~form_id:"SBB_GPRv_IMMb" ~mnemonic:"sbbl" rec_
  | Ok { iform = Some "AND_GPRv_IMMb"; _ } ->
      alu_gprv_immb_form ~form_id:"AND_GPRv_IMMb" ~mnemonic:"andl" rec_
  | Ok { iform = Some "SUB_GPRv_IMMb"; _ } ->
      alu_gprv_immb_form ~form_id:"SUB_GPRv_IMMb" ~mnemonic:"subl" rec_
  | Ok { iform = Some "XOR_GPRv_IMMb"; _ } ->
      alu_gprv_immb_form ~form_id:"XOR_GPRv_IMMb" ~mnemonic:"xorl" rec_
  | Ok { iform = Some "CMP_GPRv_IMMb"; _ } ->
      alu_gprv_immb_form ~form_id:"CMP_GPRv_IMMb" ~mnemonic:"cmpl" rec_
  (* The MEMv<-IMMb/IMMz ALU-immediate direction ({!alu_memv_imm_form}'s own
     doc comment): both opcode rungs (0x83 imm8, 0x81 immz) for all eight
     mnemonics, since this project's own encoder already lowers both without
     further change. *)
  | Ok { iform = Some "ADD_MEMv_IMMb"; _ } ->
      alu_memv_imm_form ~form_id:"ADD_MEMv_IMMb" ~mnemonic:"addl" ~oc2:"b" rec_
  | Ok { iform = Some "OR_MEMv_IMMb"; _ } ->
      alu_memv_imm_form ~form_id:"OR_MEMv_IMMb" ~mnemonic:"orl" ~oc2:"b" rec_
  | Ok { iform = Some "ADC_MEMv_IMMb"; _ } ->
      alu_memv_imm_form ~form_id:"ADC_MEMv_IMMb" ~mnemonic:"adcl" ~oc2:"b" rec_
  | Ok { iform = Some "SBB_MEMv_IMMb"; _ } ->
      alu_memv_imm_form ~form_id:"SBB_MEMv_IMMb" ~mnemonic:"sbbl" ~oc2:"b" rec_
  | Ok { iform = Some "AND_MEMv_IMMb"; _ } ->
      alu_memv_imm_form ~form_id:"AND_MEMv_IMMb" ~mnemonic:"andl" ~oc2:"b" rec_
  | Ok { iform = Some "SUB_MEMv_IMMb"; _ } ->
      alu_memv_imm_form ~form_id:"SUB_MEMv_IMMb" ~mnemonic:"subl" ~oc2:"b" rec_
  | Ok { iform = Some "XOR_MEMv_IMMb"; _ } ->
      alu_memv_imm_form ~form_id:"XOR_MEMv_IMMb" ~mnemonic:"xorl" ~oc2:"b" rec_
  | Ok { iform = Some "CMP_MEMv_IMMb"; _ } ->
      alu_memv_imm_form ~form_id:"CMP_MEMv_IMMb" ~mnemonic:"cmpl" ~oc2:"b" rec_
  | Ok { iform = Some "ADD_MEMv_IMMz"; _ } ->
      alu_memv_imm_form ~form_id:"ADD_MEMv_IMMz" ~mnemonic:"addl" ~oc2:"z" rec_
  | Ok { iform = Some "OR_MEMv_IMMz"; _ } ->
      alu_memv_imm_form ~form_id:"OR_MEMv_IMMz" ~mnemonic:"orl" ~oc2:"z" rec_
  | Ok { iform = Some "ADC_MEMv_IMMz"; _ } ->
      alu_memv_imm_form ~form_id:"ADC_MEMv_IMMz" ~mnemonic:"adcl" ~oc2:"z" rec_
  | Ok { iform = Some "SBB_MEMv_IMMz"; _ } ->
      alu_memv_imm_form ~form_id:"SBB_MEMv_IMMz" ~mnemonic:"sbbl" ~oc2:"z" rec_
  | Ok { iform = Some "AND_MEMv_IMMz"; _ } ->
      alu_memv_imm_form ~form_id:"AND_MEMv_IMMz" ~mnemonic:"andl" ~oc2:"z" rec_
  | Ok { iform = Some "SUB_MEMv_IMMz"; _ } ->
      alu_memv_imm_form ~form_id:"SUB_MEMv_IMMz" ~mnemonic:"subl" ~oc2:"z" rec_
  | Ok { iform = Some "XOR_MEMv_IMMz"; _ } ->
      alu_memv_imm_form ~form_id:"XOR_MEMv_IMMz" ~mnemonic:"xorl" ~oc2:"z" rec_
  | Ok { iform = Some "CMP_MEMv_IMMz"; _ } ->
      alu_memv_imm_form ~form_id:"CMP_MEMv_IMMz" ~mnemonic:"cmpl" ~oc2:"z" rec_
  (* GRP1's byte-operand rung ({!alu_gpr8_immb_form}'s own doc comment):
     opcode 0x80, register destination, all eight mnemonics. The undocumented
     [0x82] alias is deliberately not dispatched here - never GAS-selected,
     matching the reverse-iform precedent elsewhere in this file. *)
  | Ok { iform = Some "ADD_GPR8_IMMb_80r0"; _ } ->
      alu_gpr8_immb_form ~form_id:"ADD_GPR8_IMMb_80r0" ~mnemonic:"addb" rec_
  | Ok { iform = Some "OR_GPR8_IMMb_80r1"; _ } ->
      alu_gpr8_immb_form ~form_id:"OR_GPR8_IMMb_80r1" ~mnemonic:"orb" rec_
  | Ok { iform = Some "ADC_GPR8_IMMb_80r2"; _ } ->
      alu_gpr8_immb_form ~form_id:"ADC_GPR8_IMMb_80r2" ~mnemonic:"adcb" rec_
  | Ok { iform = Some "SBB_GPR8_IMMb_80r3"; _ } ->
      alu_gpr8_immb_form ~form_id:"SBB_GPR8_IMMb_80r3" ~mnemonic:"sbbb" rec_
  | Ok { iform = Some "AND_GPR8_IMMb_80r4"; _ } ->
      alu_gpr8_immb_form ~form_id:"AND_GPR8_IMMb_80r4" ~mnemonic:"andb" rec_
  | Ok { iform = Some "SUB_GPR8_IMMb_80r5"; _ } ->
      alu_gpr8_immb_form ~form_id:"SUB_GPR8_IMMb_80r5" ~mnemonic:"subb" rec_
  | Ok { iform = Some "XOR_GPR8_IMMb_80r6"; _ } ->
      alu_gpr8_immb_form ~form_id:"XOR_GPR8_IMMb_80r6" ~mnemonic:"xorb" rec_
  | Ok { iform = Some "CMP_GPR8_IMMb_80r7"; _ } ->
      alu_gpr8_immb_form ~form_id:"CMP_GPR8_IMMb_80r7" ~mnemonic:"cmpb" rec_
  (* Its memory-destination sibling ({!alu_memb_immb_form}'s own doc
     comment): same opcode 0x80, all eight mnemonics. *)
  | Ok { iform = Some "ADD_MEMb_IMMb_80r0"; _ } ->
      alu_memb_immb_form ~form_id:"ADD_MEMb_IMMb_80r0" ~mnemonic:"addb" rec_
  | Ok { iform = Some "OR_MEMb_IMMb_80r1"; _ } ->
      alu_memb_immb_form ~form_id:"OR_MEMb_IMMb_80r1" ~mnemonic:"orb" rec_
  | Ok { iform = Some "ADC_MEMb_IMMb_80r2"; _ } ->
      alu_memb_immb_form ~form_id:"ADC_MEMb_IMMb_80r2" ~mnemonic:"adcb" rec_
  | Ok { iform = Some "SBB_MEMb_IMMb_80r3"; _ } ->
      alu_memb_immb_form ~form_id:"SBB_MEMb_IMMb_80r3" ~mnemonic:"sbbb" rec_
  | Ok { iform = Some "AND_MEMb_IMMb_80r4"; _ } ->
      alu_memb_immb_form ~form_id:"AND_MEMb_IMMb_80r4" ~mnemonic:"andb" rec_
  | Ok { iform = Some "SUB_MEMb_IMMb_80r5"; _ } ->
      alu_memb_immb_form ~form_id:"SUB_MEMb_IMMb_80r5" ~mnemonic:"subb" rec_
  | Ok { iform = Some "XOR_MEMb_IMMb_80r6"; _ } ->
      alu_memb_immb_form ~form_id:"XOR_MEMb_IMMb_80r6" ~mnemonic:"xorb" rec_
  | Ok { iform = Some "CMP_MEMb_IMMb_80r7"; _ } ->
      alu_memb_immb_form ~form_id:"CMP_MEMb_IMMb_80r7" ~mnemonic:"cmpb" rec_
  (* The accumulator-immediate byte rung ({!alu_al_immb_form}'s own doc
     comment): opcode [ext<<3 | 4], all eight mnemonics. *)
  | Ok { iform = Some "ADD_AL_IMMb"; _ } ->
      alu_al_immb_form ~form_id:"ADD_AL_IMMb" ~mnemonic:"addb" rec_
  | Ok { iform = Some "OR_AL_IMMb"; _ } ->
      alu_al_immb_form ~form_id:"OR_AL_IMMb" ~mnemonic:"orb" rec_
  | Ok { iform = Some "ADC_AL_IMMb"; _ } ->
      alu_al_immb_form ~form_id:"ADC_AL_IMMb" ~mnemonic:"adcb" rec_
  | Ok { iform = Some "SBB_AL_IMMb"; _ } ->
      alu_al_immb_form ~form_id:"SBB_AL_IMMb" ~mnemonic:"sbbb" rec_
  | Ok { iform = Some "AND_AL_IMMb"; _ } ->
      alu_al_immb_form ~form_id:"AND_AL_IMMb" ~mnemonic:"andb" rec_
  | Ok { iform = Some "SUB_AL_IMMb"; _ } ->
      alu_al_immb_form ~form_id:"SUB_AL_IMMb" ~mnemonic:"subb" rec_
  | Ok { iform = Some "XOR_AL_IMMb"; _ } ->
      alu_al_immb_form ~form_id:"XOR_AL_IMMb" ~mnemonic:"xorb" rec_
  | Ok { iform = Some "CMP_AL_IMMb"; _ } ->
      alu_al_immb_form ~form_id:"CMP_AL_IMMb" ~mnemonic:"cmpb" rec_
  | Ok { iform = Some other; _ } ->
      err "unhandled-iform"
        (Printf.sprintf
           "Isa_norm_xed only normalizes the frozen pilot iforms, the register/register and \
            register/immediate legacy ADD/MOV forms, the explicit-32-bit-width \
            SUB/AND/OR/XOR/ADC/SBB/CMP/TEST register-register forms, the explicit-32-bit-width \
            ADD/ADC/XOR/SUB/AND/OR/SBB/CMP register<-memory forms, the explicit-32-bit-width \
            ADD/OR/ADC/SBB/AND/SUB/XOR/CMP/TEST memory<-register forms, the explicit-32-bit-width \
            OR/ADC/SBB/AND/SUB/XOR/CMP register/immz forms, the explicit-32-bit-width \
            ADD/OR/ADC/SBB/AND/SUB/XOR/CMP register/immb forms, the explicit-32-bit-width \
            ADD/OR/ADC/SBB/AND/SUB/XOR/CMP memory/immb and memory/immz forms, and the byte-width \
            ADD/OR/ADC/SBB/AND/SUB/XOR/CMP register/immb, memory/immb, and accumulator/immb forms; \
            %s is not one of them"
           other)
  | Ok { iform = None; _ } -> err "missing-iform" "XED record has no provenance.iform"
  | Error msg -> err "not-a-xed-record" msg
