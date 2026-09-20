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
  | Ok { extension = Some "SSE"; _ } -> req_and (Req_feature "x86:sse") (applic ())
  | Ok { extension = Some "SSE2"; _ } -> req_and (Req_feature "x86:sse2") (applic ())
  | Ok { extension = Some "AVX"; _ } -> req_and (Req_feature "x86:avx") (applic ())
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

(* SSE2 scalar-float register-register binops (ADDSD/SUBSD/MULSD/
   DIVSD_XMMsd_XMMsd): x86_family_encode.ml's own [Lowered.Sse_binop_r_rm],
   already fully built and fixture-verified (M5, asm/docs/corpus.md) before
   XED-driven admission began - this is the first XED-driven admission
   of any xmm-register form, so it introduces the model's own [X86_xmm]
   register class rather than reusing [X86_gpr]. XED orders REG0 (rw, the
   destination) before REG1 (r, the source) - the reverse of AT&T's own
   source-first spelling ([addsd %xmm1, %xmm0]) - so this reads the role
   off each operand's own [rw] fact the same way {!two_operand_gprv_form}
   does for GPRv, rather than assuming a fixed slot order. Register-memory
   (XMMsd_MEMsd) is a separate, not-yet-admitted shape - nothing here
   claims it. *)
let xmm_binop_rr_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "REG0" && b.op_name = "REG1" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let src =
        {
          op_name = "src";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
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
          operands = [ src; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_decorated ("%", Syn_operand "src"); Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf "REG0 (rw=%s), REG1 (rw=%s) taken verbatim from encoding.operands"
                    a.rw b.rw;
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (source, then destination) is GAS convention, not a XED fact";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/REG1 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* SSE2 scalar-float register<-memory binops (ADDSD/SUBSD/MULSD/
   DIVSD_XMMsd_MEMsd): {!xmm_binop_rr_form}'s own register-register sibling
   with MEM0 standing in for REG1 - x86_family_encode.ml's own
   [Lowered.Sse_binop_r_rm] already builds both directions off the same
   [rm : Rm.t] field (its own doc comment: "a memory [rm] needs no check
   here"), so this is a second normalizer over that one shape, not a second
   encoder path. XED orders REG0 (rw, destination) before MEM0 (r, source)
   here too, matching {!alu_gprv_memv_form}'s own REG0-before-MEM0
   convention, so this mirrors that function's [mem; reg] operand order and
   AT&T syntax exactly, with [X86_xmm] replacing [X86_gpr]. *)
let xmm_binop_rm_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "REG0" && b.op_name = "MEM0" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
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
          operands = [ mem; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "mem"; Syn_decorated ("%", Syn_operand "dest") ];
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
                  "AT&T operand order (source, then destination) is GAS convention, not a XED fact";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/MEM0 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* SHUFPS/SHUFPD: {!xmm_binop_rr_form}'s trailing-immediate sibling - the first
   XMM-immediate-carrying legacy shape. XED's resolved operands are REG0 (dest, rw), REG1
   (src, r) and IMM0 (r, oc2 "b") in that order; x86_family_encode.ml's own
   [Lowered.Sse_binop_imm_r_rm] already builds both this and the register<-memory sibling off
   the same [rm : Rm.t] field, so this is a second normalizer over that one shape, not a second
   encoder path - {!xmm_binop_rr_form}'s own comment explains the pattern. AT&T operand order
   is [imm, src, dest], the same [imm, src, dst] order {!shld_imm_form}'s own [Shld] uses,
   confirmed against real GNU as. *)
let xmm_binop_imm_rr_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b; c ] }
    when a.op_name = "REG0" && b.op_name = "REG1" && c.op_name = "IMM0" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let src =
        {
          op_name = "src";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 8;
                signed = false;
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
          operands = [ imm; src; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_decorated ("$", Syn_operand "imm");
                  Syn_decorated ("%", Syn_operand "src");
                  Syn_decorated ("%", Syn_operand "dest");
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "REG0 (rw=%s, dest), REG1 (rw=%s, src), IMM0 (rw=%s) taken verbatim from \
                     encoding.operands"
                    a.rw b.rw c.rw;
              };
              {
                label = Inferred;
                note = "AT&T operand order (imm, src, dest) is GAS convention, not a XED fact";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/REG1/IMM0 operands in that order, got %d"
           (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* {!xmm_binop_imm_rr_form}'s register<-memory sibling (SHUFPS_XMMps_MEMps_IMMb etc.): XED's
   resolved operands are REG0 (dest, rw), MEM0 (src, r) and IMM0 (r) - {!xmm_binop_rm_form}'s
   own REG0/MEM0 pair with the same trailing IMM0 {!xmm_binop_imm_rr_form} adds. *)
let xmm_binop_imm_rm_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b; c ] }
    when a.op_name = "REG0" && b.op_name = "MEM0" && c.op_name = "IMM0" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
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
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 8;
                signed = false;
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
          operands = [ imm; mem; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_decorated ("$", Syn_operand "imm");
                  Syn_operand "mem";
                  Syn_decorated ("%", Syn_operand "dest");
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "REG0 (rw=%s, dest), MEM0 (rw=%s, src), IMM0 (rw=%s) taken verbatim from \
                     encoding.operands"
                    a.rw b.rw c.rw;
              };
              {
                label = Inferred;
                note = "AT&T operand order (imm, mem, dest) is GAS convention, not a XED fact";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/MEM0/IMM0 operands in that order, got %d"
           (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* VEX-encoded scalar-double register-register binops (VADDSD/VSUBSD/VMULSD/
   VDIVSD_XMMdq_XMMdq_XMMq): the first x86 vector-extension (AVX) admission,
   as opposed to every {!xmm_binop_rr_form} caller above, which is legacy
   SSE/SSE2. XED's resolved operands are REG0 (dest, w), REG1 (src1, r) and
   REG2 (src2, r) in that order - x86_family_encode.ml's own
   [Lowered.Vex_binop_rrr] doc comment explains why REG1 is the VEX prefix's
   own [vvvv] field rather than a ModR/M field, and why REG2 (the ModR/M r/m
   operand) is restricted to xmm0-7 there (a fact about the encoder's
   two-byte-VEX-only scope, not something this normalizer's [operands] need
   to restate - [x86_encoding] preserves the source's full unrestricted
   pattern regardless of what the encoder currently admits). AT&T operand
   order is [src2, src1, dest] - GAS's own non-destructive three-operand
   convention, confirmed against real GNU as - the reverse of
   {!xmm_binop_rr_form}'s two-operand [src, dest]. *)
let vex_binop_rrr_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b; c ] }
    when a.op_name = "REG0" && b.op_name = "REG1" && c.op_name = "REG2" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let src1 =
        {
          op_name = "src1";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      let src2 =
        {
          op_name = "src2";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw c.rw;
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
          operands = [ src2; src1; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_decorated ("%", Syn_operand "src2");
                  Syn_decorated ("%", Syn_operand "src1");
                  Syn_decorated ("%", Syn_operand "dest");
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "REG0 (rw=%s, dest), REG1 (rw=%s, src1/vvvv), REG2 (rw=%s, src2/rm) taken \
                     verbatim from encoding.operands"
                    a.rw b.rw c.rw;
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (src2, src1, dest) is GAS's own non-destructive VEX \
                   convention, not a XED fact";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/REG1/REG2 operands in that order, got %d"
           (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* {!vex_binop_rrr_form}'s register<-memory sibling (VADDSD_XMMdq_XMMdq_MEMq
   etc.): XED's resolved operands are REG0 (dest, w), REG1 (src1/vvvv, r) and
   MEM0 (src2/rm-as-memory, r), the same REG0/REG1 pair as
   {!vex_binop_rrr_form} with its REG2 replaced by a memory operand -
   x86_family_encode.ml's own [Lowered.Vex_binop_rr_rm] now represents both
   directly, confirmed against real GNU as to be structurally identical to
   legacy SSE's own memory encoding once the VEX prefix bytes are in place. *)
let vex_binop_rr_mem_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b; c ] }
    when a.op_name = "REG0" && b.op_name = "REG1" && c.op_name = "MEM0" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let src1 =
        {
          op_name = "src1";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      let src2 =
        {
          op_name = "src2";
          op_kind = Memory { width_bits = None };
          role = role_of_rw c.rw;
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
          operands = [ src2; src1; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_operand "src2";
                  Syn_decorated ("%", Syn_operand "src1");
                  Syn_decorated ("%", Syn_operand "dest");
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "REG0 (rw=%s, dest), REG1 (rw=%s, src1/vvvv), MEM0 (rw=%s, src2/rm) taken \
                     verbatim from encoding.operands"
                    a.rw b.rw c.rw;
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (src2, src1, dest) is GAS's own non-destructive VEX \
                   convention, not a XED fact - same as {!vex_binop_rrr_form}'s register form";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/REG1/MEM0 operands in that order, got %d"
           (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* VSHUFPS/VSHUFPD: {!vex_binop_rrr_form}'s trailing-immediate sibling - the VEX
   sibling of {!xmm_binop_imm_rr_form}. Only the plain [vex]-space, 128-bit iforms (real GNU
   as's [vshufps]/[vshufpd]) are handled here; the [evex]-space AVX-512 masked iforms sharing
   the same native name (a distinct [REG1]-as-mask operand layout) are out of scope. XED's
   resolved operands are REG0 (dest, w), REG1 (src1/vvvv, r), REG2 (src2/rm, r) and IMM0 (r) -
   {!vex_binop_rrr_form}'s own REG0/REG1/REG2 triple with the same trailing IMM0
   {!xmm_binop_imm_rr_form} adds. AT&T operand order is [imm, src2, src1, dest], confirmed
   against real GNU as: [vshufps $0x1b,%xmm3,%xmm2,%xmm1] -> [c5 e8 c6 cb 1b]. *)
let vex_binop_imm_rrr_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b; c; d ] }
    when a.op_name = "REG0" && b.op_name = "REG1" && c.op_name = "REG2" && d.op_name = "IMM0" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let src1 =
        {
          op_name = "src1";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      let src2 =
        {
          op_name = "src2";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw c.rw;
          explicit = true;
        }
      in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 8;
                signed = false;
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
          operands = [ imm; src2; src1; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_decorated ("$", Syn_operand "imm");
                  Syn_decorated ("%", Syn_operand "src2");
                  Syn_decorated ("%", Syn_operand "src1");
                  Syn_decorated ("%", Syn_operand "dest");
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "REG0 (rw=%s, dest), REG1 (rw=%s, src1/vvvv), REG2 (rw=%s, src2/rm), IMM0 \
                     (rw=%s) taken verbatim from encoding.operands"
                    a.rw b.rw c.rw d.rw;
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (imm, src2, src1, dest) is GAS's own non-destructive VEX \
                   convention, not a XED fact - same as {!vex_binop_rrr_form}'s register form";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/REG1/REG2/IMM0 operands in that order, got %d"
           (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* {!vex_binop_imm_rrr_form}'s register<-memory sibling (VSHUFPS_XMMdq_XMMdq_MEMdq_IMMb etc.):
   XED's resolved operands are REG0 (dest, w), REG1 (src1/vvvv, r), MEM0 (src2/rm-as-memory, r)
   and IMM0 (r) - {!vex_binop_rr_mem_form}'s own REG0/REG1/MEM0 triple with the same trailing
   IMM0 {!vex_binop_imm_rrr_form} adds. *)
let vex_binop_imm_rr_mem_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b; c; d ] }
    when a.op_name = "REG0" && b.op_name = "REG1" && c.op_name = "MEM0" && d.op_name = "IMM0" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let src1 =
        {
          op_name = "src1";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      let src2 =
        {
          op_name = "src2";
          op_kind = Memory { width_bits = None };
          role = role_of_rw c.rw;
          explicit = true;
        }
      in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 8;
                signed = false;
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
          operands = [ imm; src2; src1; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_decorated ("$", Syn_operand "imm");
                  Syn_operand "src2";
                  Syn_decorated ("%", Syn_operand "src1");
                  Syn_decorated ("%", Syn_operand "dest");
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "REG0 (rw=%s, dest), REG1 (rw=%s, src1/vvvv), MEM0 (rw=%s, src2/rm), IMM0 \
                     (rw=%s) taken verbatim from encoding.operands"
                    a.rw b.rw c.rw d.rw;
              };
              {
                label = Inferred;
                note =
                  "AT&T operand order (imm, src2, src1, dest) is GAS's own non-destructive VEX \
                   convention, not a XED fact - same as {!vex_binop_imm_rrr_form}'s register form";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/REG1/MEM0/IMM0 operands in that order, got %d"
           (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* {!vex_binop_rrr_form}'s two-operand unary sibling (VSQRTPS_XMMdq_XMMdq etc.): XED's resolved
   operands are just REG0 (dest, w) and REG1 (src, r) - no third [vvvv]-carrying operand, since
   VEX's [vvvv] field is architecturally unused (must be [1111]) for these packed unary forms -
   confirmed against real GNU as, which rejects a third operand outright ("number of operands
   mismatch") for [vsqrtps]/[vsqrtpd], unlike {!vex_binop_rrr_form}'s scalar siblings where the
   [vvvv] operand is real (it merges the destination's upper bits, even though it isn't a second
   arithmetic input). *)
let vex_unop_rr_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "REG0" && b.op_name = "REG1" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let src =
        {
          op_name = "src";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
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
          operands = [ src; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_decorated ("%", Syn_operand "src"); Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "REG0 (rw=%s, dest), REG1 (rw=%s, src) taken verbatim from encoding.operands"
                    a.rw b.rw;
              };
              {
                label = Inferred;
                note =
                  "VEX.vvvv is architecturally unused (must be 1111) for this packed unary form - \
                   confirmed against real GNU as rejecting a third operand";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/REG1 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* {!vex_unop_rr_form}'s register<-memory sibling (VSQRTPS_XMMdq_MEMdq etc.): REG0 (dest, w) and
   MEM0 (src, r), the same REG0/MEM0 pair {!vex_binop_rr_mem_form} uses with its REG1 dropped. *)
let vex_unop_rr_mem_form ~form_id ~mnemonic (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "REG0" && b.op_name = "MEM0" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let src =
        {
          op_name = "src";
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
          operands = [ src; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "src"; Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "REG0 (rw=%s, dest), MEM0 (rw=%s, src) taken verbatim from encoding.operands"
                    a.rw b.rw;
              };
              {
                label = Inferred;
                note =
                  "VEX.vvvv is architecturally unused (must be 1111) for this packed unary form - \
                   same as {!vex_unop_rr_form}'s register form";
              };
            ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/MEM0 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* [movsd]/[movss] load/store (MOVSD_XMM_XMMdq_MEMsd/MOVSD_XMM_MEMsd_XMMsd
   and their MOVSS siblings): {!mov_gprv_memv_form}'s own load/store shape
   with [X86_xmm] replacing [X86_gpr], and the mnemonic ("movsd"/"movss")
   passed in rather than fixed to "movl", since GAS spells each mnemonic
   explicitly rather than via a shared suffix. XED orders REG0 before MEM0
   for the load direction and MEM0 before REG0 for the store direction,
   exactly as {!mov_gprv_memv_form} already handles via [load]. Role is
   fixed write-only(load)/read-only(store) rather than read off [rw],
   matching MOVSD_XMM's own REG0/MEM0 facts ("w"/"r" for load, "r"/"w" for
   store - never "rw" the way a real binop's destination is): a plain move,
   unlike {!xmm_binop_rr_form}/{!xmm_binop_rm_form}'s shape above.
   Register-register [movsd]/[movss] is deliberately not covered here,
   matching x86_family_encode.ml's own comment that this project's encoder
   does not build it (unevidenced by the corpus). *)
let xmm_mov_form ~form_id ~mnemonic ~load (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when (a.op_name = "REG0" && b.op_name = "MEM0") || (a.op_name = "MEM0" && b.op_name = "REG0") ->
      let reg =
        {
          op_name = "reg";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
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
          syntax = { dialect = "gas-att"; mnemonic; operands = syntax_operands };
          concreteness = Concrete;
          facts =
            [ { label = Upstream; note = "REG0 and MEM0 taken verbatim from encoding.operands" } ];
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/MEM0 operands, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* [cvtsi2sd]/[cvtsi2ss] register-source (CVTSI2SD_XMMsd_GPR32d/GPR64q and
   their CVTSI2SS siblings): the model's first mixed-register-class shape -
   REG0 is [X86_xmm] (the destination) while REG1 is [X86_gpr] (the source),
   unlike {!xmm_binop_rr_form}'s xmm/xmm pair. XED gives the 32-bit
   ([GPR32_B]) and 64-bit ([GPR64_B]) source width as two distinct records
   sharing REG0/REG1's own positional order, and this project's own frontend
   spells the two widths as distinct mnemonics ([cvtsi2sd]/[cvtsi2sdq]),
   so [mnemonic] is passed in per form_id rather than derived from [oc2].
   [mode64] adds [Req_mode {mode="mode64"; equals=true}]: the record's own
   [provenance.mode_restriction] is "unspecified" here (a GPR64 register
   operand implies 64-bit mode by register-class fact, not by an
   applicability token {!requirement_of_applicability} would see), so this
   is a derived requirement layered on top of {!requirement_of}'s
   unconditional SSE/SSE2 feature, not a second read of the same source
   fact. Memory-source (MEMd/MEMq) is a separate shape, {!cvtsi2f_rm_form}
   below - nothing here claims it. *)
let cvtsi2f_rr_form ~form_id ~mnemonic ~mode64 (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "REG0" && b.op_name = "REG1" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let src =
        {
          op_name = "src";
          op_kind = Register { class_ = X86_gpr; excluded = [] };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      let requirement =
        if mode64 then req_and (Req_mode { mode = "mode64"; equals = true }) (requirement_of rec_)
        else requirement_of rec_
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding = X86_encoding { space; opcode_map; opcode; pattern };
          operands = [ src; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_decorated ("%", Syn_operand "src"); Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            ([
               {
                 label = Upstream;
                 note =
                   Printf.sprintf "REG0 (rw=%s), REG1 (rw=%s) taken verbatim from encoding.operands"
                     a.rw b.rw;
               };
               {
                 label = Inferred;
                 note =
                   "AT&T operand order (source, then destination) is GAS convention, not a XED fact";
               };
             ]
            @
            if mode64 then
              [
                {
                  label = Inferred;
                  note =
                    "mode64 requirement derived from the GPR64 source register class, since \
                     provenance.mode_restriction is unspecified for this record";
                };
              ]
            else []);
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/REG1 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* [cvtsi2sd]/[cvtsi2ss] memory-source sibling (CVTSI2SD_XMMsd_MEMd/MEMq and
   their CVTSI2SS siblings): {!cvtsi2f_rr_form}'s own shape with MEM0
   standing in for REG1, the same way {!xmm_binop_rm_form} stands in for
   {!xmm_binop_rr_form}'s REG1. *)
let cvtsi2f_rm_form ~form_id ~mnemonic ~mode64 (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "REG0" && b.op_name = "MEM0" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let mem =
        {
          op_name = "src";
          op_kind = Memory { width_bits = None };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      let requirement =
        if mode64 then req_and (Req_mode { mode = "mode64"; equals = true }) (requirement_of rec_)
        else requirement_of rec_
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding = X86_encoding { space; opcode_map; opcode; pattern };
          operands = [ mem; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "src"; Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            ([
               {
                 label = Upstream;
                 note =
                   Printf.sprintf "REG0 (rw=%s), MEM0 (rw=%s) taken verbatim from encoding.operands"
                     a.rw b.rw;
               };
               {
                 label = Inferred;
                 note =
                   "AT&T operand order (source, then destination) is GAS convention, not a XED fact";
               };
             ]
            @
            if mode64 then
              [
                {
                  label = Inferred;
                  note =
                    "mode64 requirement derived from the GPR64-sized memory operand (MEMq), since \
                     provenance.mode_restriction is unspecified for this record";
                };
              ]
            else []);
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/MEM0 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* [cvttsd2si] register-source (CVTTSD2SI_GPR32d_XMMsd/GPR64q_XMMsd):
   {!cvtsi2f_rr_form}'s own mixed-register-class shape reversed - REG0 is
   the [X86_gpr] destination here, REG1 the [X86_xmm] source. This
   project's frontend spells both GPR widths with the one bare mnemonic
   [cvttsd2si] (it reads the destination register's own width, exactly as
   x86_family_encode.ml's own "cvttsd2si" parse-time comment documents, and
   real GNU as does the same - confirmed against real GNU as: [cvttsd2si
   %xmm0, %rax] and [cvttsd2siq %xmm0, %rax] assemble to the identical
   bytes), so unlike {!cvtsi2f_rr_form} there is no per-width mnemonic
   parameter. [mode64] carries the same derived-requirement reasoning as
   {!cvtsi2f_rr_form}'s own doc comment, for the GPR64 destination instead
   of a GPR64 source. *)
let cvtf2i_rr_form ~form_id ~mode64 (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "REG0" && b.op_name = "REG1" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_gpr; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let src =
        {
          op_name = "src";
          op_kind = Register { class_ = X86_xmm; excluded = [] };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      let requirement =
        if mode64 then req_and (Req_mode { mode = "mode64"; equals = true }) (requirement_of rec_)
        else requirement_of rec_
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding = X86_encoding { space; opcode_map; opcode; pattern };
          operands = [ src; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic = "cvttsd2si";
              operands =
                [ Syn_decorated ("%", Syn_operand "src"); Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            ([
               {
                 label = Upstream;
                 note =
                   Printf.sprintf "REG0 (rw=%s), REG1 (rw=%s) taken verbatim from encoding.operands"
                     a.rw b.rw;
               };
               {
                 label = Inferred;
                 note =
                   "AT&T operand order (source, then destination) is GAS convention, not a XED fact";
               };
             ]
            @
            if mode64 then
              [
                {
                  label = Inferred;
                  note =
                    "mode64 requirement derived from the GPR64 destination register class, since \
                     provenance.mode_restriction is unspecified for this record";
                };
              ]
            else []);
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/REG1 operands in that order, got %d" (List.length operands))
  | _ -> err (form_id ^ "-not-x86-encoding") "record's encoding is not XED x86_encoding"

(* [cvttsd2si] memory-source sibling (CVTTSD2SI_GPR32d_MEMsd/GPR64q_MEMsd):
   {!cvtf2i_rr_form}'s own shape with MEM0 standing in for REG1. Unlike
   {!cvtsi2f_rm_form}'s MEMd/MEMq split, XED reports MEM0 here with a fixed
   [oc2] of "sd" regardless of the GPR destination's own width - only the
   destination varies between the 32-bit and 64-bit forms, never the
   memory operand (always a double), so [mode64] alone (no per-width MEM0
   fact) distinguishes the two records. *)
let cvtf2i_rm_form ~form_id ~mode64 (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; opcode; pattern; operands = [ a; b ] }
    when a.op_name = "REG0" && b.op_name = "MEM0" ->
      let dest =
        {
          op_name = "dest";
          op_kind = Register { class_ = X86_gpr; excluded = [] };
          role = role_of_rw a.rw;
          explicit = true;
        }
      in
      let mem =
        {
          op_name = "src";
          op_kind = Memory { width_bits = None };
          role = role_of_rw b.rw;
          explicit = true;
        }
      in
      let requirement =
        if mode64 then req_and (Req_mode { mode = "mode64"; equals = true }) (requirement_of rec_)
        else requirement_of rec_
      in
      Ok
        {
          form_id = "x86:" ^ form_id;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding = X86_encoding { space; opcode_map; opcode; pattern };
          operands = [ mem; dest ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic = "cvttsd2si";
              operands = [ Syn_operand "src"; Syn_decorated ("%", Syn_operand "dest") ];
            };
          concreteness = Concrete;
          facts =
            ([
               {
                 label = Upstream;
                 note =
                   Printf.sprintf "REG0 (rw=%s), MEM0 (rw=%s) taken verbatim from encoding.operands"
                     a.rw b.rw;
               };
               {
                 label = Inferred;
                 note =
                   "AT&T operand order (source, then destination) is GAS convention, not a XED fact";
               };
             ]
            @
            if mode64 then
              [
                {
                  label = Inferred;
                  note =
                    "mode64 requirement derived from the GPR64 destination register class, since \
                     provenance.mode_restriction is unspecified for this record";
                };
              ]
            else []);
          diagnostics = [];
        }
  | R.X86_encoding { operands; _ } ->
      err
        (form_id ^ "-unrecognized-operands")
        (Printf.sprintf "expected REG0/MEM0 operands in that order, got %d" (List.length operands))
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
  (* The first xmm-register admission ({!xmm_binop_rr_form}'s own doc
     comment): SSE2 scalar-float register-register binops, already fully
     built and fixture-verified by this project's own encoder. *)
  | Ok { iform = Some "ADDSD_XMMsd_XMMsd"; _ } ->
      xmm_binop_rr_form ~form_id:"ADDSD_XMMsd_XMMsd" ~mnemonic:"addsd" rec_
  | Ok { iform = Some "SUBSD_XMMsd_XMMsd"; _ } ->
      xmm_binop_rr_form ~form_id:"SUBSD_XMMsd_XMMsd" ~mnemonic:"subsd" rec_
  | Ok { iform = Some "MULSD_XMMsd_XMMsd"; _ } ->
      xmm_binop_rr_form ~form_id:"MULSD_XMMsd_XMMsd" ~mnemonic:"mulsd" rec_
  | Ok { iform = Some "DIVSD_XMMsd_XMMsd"; _ } ->
      xmm_binop_rr_form ~form_id:"DIVSD_XMMsd_XMMsd" ~mnemonic:"divsd" rec_
  (* Its register<-memory sibling ({!xmm_binop_rm_form}'s own doc comment). *)
  | Ok { iform = Some "ADDSD_XMMsd_MEMsd"; _ } ->
      xmm_binop_rm_form ~form_id:"ADDSD_XMMsd_MEMsd" ~mnemonic:"addsd" rec_
  | Ok { iform = Some "SUBSD_XMMsd_MEMsd"; _ } ->
      xmm_binop_rm_form ~form_id:"SUBSD_XMMsd_MEMsd" ~mnemonic:"subsd" rec_
  | Ok { iform = Some "MULSD_XMMsd_MEMsd"; _ } ->
      xmm_binop_rm_form ~form_id:"MULSD_XMMsd_MEMsd" ~mnemonic:"mulsd" rec_
  | Ok { iform = Some "DIVSD_XMMsd_MEMsd"; _ } ->
      xmm_binop_rm_form ~form_id:"DIVSD_XMMsd_MEMsd" ~mnemonic:"divsd" rec_
  (* The rest of the plain xmm-xmm/xmm-memory binop shape {!xmm_binop_rr_form}/
     {!xmm_binop_rm_form} already cover generically (their REG0/REG1 and
     REG0/MEM0 match arms read every operand's role off its own [rw] fact,
     never assuming ADDSD's own read-write destination): MULSS/DIVSS
     (mandatory-prefix F3, {!requirement_of}'s SSE case - not SSE2, confirmed
     from provenance.extension) and COMISD/UCOMISD/XORPD/PXOR/
     MOVAPD_XMMpd_XMMpd_0F28 (mandatory-prefix 66, SSE2) and CVTSD2SS/
     CVTSS2SD (already admitted for the F2/F3-mandatory-prefix binop rungs
     above, this is their own xmm-xmm/xmm-memory pair) all reuse
     x86_family_encode.ml's existing [sse_binop_f3_codec]/[sse_binop_66_codec]
     tables with zero encoder change. COMISS has no mandatory prefix at all
     ([sse_binop_none_alt]) but is still [Lowered.Sse_binop_r_rm] under the
     hood, so the same two normalizers cover it too - confirmed against real
     GNU as for every register-register and register<-memory spelling below.
     MOVAPD's own reverse MEMpd<-XMMpd store direction (opcode 0x29, a
     genuinely different Lowered/alt shape this project's encoder does not
     build) and its redundant _0F29 register-register iform are deliberately
     left unhandled here, matching this file's existing to_rm_r "low-numbered
     iform" precedent (confirmed: real GNU as selects 0x28 for
     [movapd %xmm1, %xmm0]). *)
  | Ok { iform = Some "MULSS_XMMss_XMMss"; _ } ->
      xmm_binop_rr_form ~form_id:"MULSS_XMMss_XMMss" ~mnemonic:"mulss" rec_
  | Ok { iform = Some "DIVSS_XMMss_XMMss"; _ } ->
      xmm_binop_rr_form ~form_id:"DIVSS_XMMss_XMMss" ~mnemonic:"divss" rec_
  | Ok { iform = Some "COMISD_XMMsd_XMMsd"; _ } ->
      xmm_binop_rr_form ~form_id:"COMISD_XMMsd_XMMsd" ~mnemonic:"comisd" rec_
  | Ok { iform = Some "UCOMISD_XMMsd_XMMsd"; _ } ->
      xmm_binop_rr_form ~form_id:"UCOMISD_XMMsd_XMMsd" ~mnemonic:"ucomisd" rec_
  | Ok { iform = Some "COMISS_XMMss_XMMss"; _ } ->
      xmm_binop_rr_form ~form_id:"COMISS_XMMss_XMMss" ~mnemonic:"comiss" rec_
  | Ok { iform = Some "UCOMISS_XMMss_XMMss"; _ } ->
      xmm_binop_rr_form ~form_id:"UCOMISS_XMMss_XMMss" ~mnemonic:"ucomiss" rec_
  | Ok { iform = Some "XORPD_XMMxuq_XMMxuq"; _ } ->
      xmm_binop_rr_form ~form_id:"XORPD_XMMxuq_XMMxuq" ~mnemonic:"xorpd" rec_
  | Ok { iform = Some "PXOR_XMMdq_XMMdq"; _ } ->
      xmm_binop_rr_form ~form_id:"PXOR_XMMdq_XMMdq" ~mnemonic:"pxor" rec_
  | Ok { iform = Some "MOVAPD_XMMpd_XMMpd_0F28"; _ } ->
      xmm_binop_rr_form ~form_id:"MOVAPD_XMMpd_XMMpd_0F28" ~mnemonic:"movapd" rec_
  | Ok { iform = Some "CVTSD2SS_XMMss_XMMsd"; _ } ->
      xmm_binop_rr_form ~form_id:"CVTSD2SS_XMMss_XMMsd" ~mnemonic:"cvtsd2ss" rec_
  | Ok { iform = Some "CVTSS2SD_XMMsd_XMMss"; _ } ->
      xmm_binop_rr_form ~form_id:"CVTSS2SD_XMMsd_XMMss" ~mnemonic:"cvtss2sd" rec_
  | Ok { iform = Some "MULSS_XMMss_MEMss"; _ } ->
      xmm_binop_rm_form ~form_id:"MULSS_XMMss_MEMss" ~mnemonic:"mulss" rec_
  | Ok { iform = Some "DIVSS_XMMss_MEMss"; _ } ->
      xmm_binop_rm_form ~form_id:"DIVSS_XMMss_MEMss" ~mnemonic:"divss" rec_
  | Ok { iform = Some "COMISD_XMMsd_MEMsd"; _ } ->
      xmm_binop_rm_form ~form_id:"COMISD_XMMsd_MEMsd" ~mnemonic:"comisd" rec_
  | Ok { iform = Some "UCOMISD_XMMsd_MEMsd"; _ } ->
      xmm_binop_rm_form ~form_id:"UCOMISD_XMMsd_MEMsd" ~mnemonic:"ucomisd" rec_
  | Ok { iform = Some "COMISS_XMMss_MEMss"; _ } ->
      xmm_binop_rm_form ~form_id:"COMISS_XMMss_MEMss" ~mnemonic:"comiss" rec_
  | Ok { iform = Some "UCOMISS_XMMss_MEMss"; _ } ->
      xmm_binop_rm_form ~form_id:"UCOMISS_XMMss_MEMss" ~mnemonic:"ucomiss" rec_
  | Ok { iform = Some "XORPD_XMMxuq_MEMxuq"; _ } ->
      xmm_binop_rm_form ~form_id:"XORPD_XMMxuq_MEMxuq" ~mnemonic:"xorpd" rec_
  | Ok { iform = Some "PXOR_XMMdq_MEMdq"; _ } ->
      xmm_binop_rm_form ~form_id:"PXOR_XMMdq_MEMdq" ~mnemonic:"pxor" rec_
  | Ok { iform = Some "MOVAPD_XMMpd_MEMpd"; _ } ->
      xmm_binop_rm_form ~form_id:"MOVAPD_XMMpd_MEMpd" ~mnemonic:"movapd" rec_
  | Ok { iform = Some "CVTSD2SS_XMMss_MEMsd"; _ } ->
      xmm_binop_rm_form ~form_id:"CVTSD2SS_XMMss_MEMsd" ~mnemonic:"cvtsd2ss" rec_
  | Ok { iform = Some "CVTSS2SD_XMMsd_MEMss"; _ } ->
      xmm_binop_rm_form ~form_id:"CVTSS2SD_XMMsd_MEMss" ~mnemonic:"cvtss2sd" rec_
  (* [movsd]/[movss] load/store ({!xmm_mov_form}'s own doc comment). *)
  | Ok { iform = Some "MOVSD_XMM_XMMdq_MEMsd"; _ } ->
      xmm_mov_form ~form_id:"MOVSD_XMM_XMMdq_MEMsd" ~mnemonic:"movsd" ~load:true rec_
  | Ok { iform = Some "MOVSD_XMM_MEMsd_XMMsd"; _ } ->
      xmm_mov_form ~form_id:"MOVSD_XMM_MEMsd_XMMsd" ~mnemonic:"movsd" ~load:false rec_
  | Ok { iform = Some "MOVSS_XMMdq_MEMss"; _ } ->
      xmm_mov_form ~form_id:"MOVSS_XMMdq_MEMss" ~mnemonic:"movss" ~load:true rec_
  | Ok { iform = Some "MOVSS_MEMss_XMMss"; _ } ->
      xmm_mov_form ~form_id:"MOVSS_MEMss_XMMss" ~mnemonic:"movss" ~load:false rec_
  (* [cvtsi2sd]/[cvtsi2ss]/[cvttsd2si] ({!cvtsi2f_rr_form}/{!cvtsi2f_rm_form}/
     {!cvtf2i_rr_form}/{!cvtf2i_rm_form}'s own doc comments). *)
  | Ok { iform = Some "CVTSI2SD_XMMsd_GPR32d"; _ } ->
      cvtsi2f_rr_form ~form_id:"CVTSI2SD_XMMsd_GPR32d" ~mnemonic:"cvtsi2sd" ~mode64:false rec_
  | Ok { iform = Some "CVTSI2SD_XMMsd_GPR64q"; _ } ->
      cvtsi2f_rr_form ~form_id:"CVTSI2SD_XMMsd_GPR64q" ~mnemonic:"cvtsi2sdq" ~mode64:true rec_
  | Ok { iform = Some "CVTSI2SD_XMMsd_MEMd"; _ } ->
      cvtsi2f_rm_form ~form_id:"CVTSI2SD_XMMsd_MEMd" ~mnemonic:"cvtsi2sd" ~mode64:false rec_
  | Ok { iform = Some "CVTSI2SD_XMMsd_MEMq"; _ } ->
      cvtsi2f_rm_form ~form_id:"CVTSI2SD_XMMsd_MEMq" ~mnemonic:"cvtsi2sdq" ~mode64:true rec_
  | Ok { iform = Some "CVTSI2SS_XMMss_GPR32d"; _ } ->
      cvtsi2f_rr_form ~form_id:"CVTSI2SS_XMMss_GPR32d" ~mnemonic:"cvtsi2ss" ~mode64:false rec_
  | Ok { iform = Some "CVTSI2SS_XMMss_GPR64q"; _ } ->
      cvtsi2f_rr_form ~form_id:"CVTSI2SS_XMMss_GPR64q" ~mnemonic:"cvtsi2ssq" ~mode64:true rec_
  | Ok { iform = Some "CVTSI2SS_XMMss_MEMd"; _ } ->
      cvtsi2f_rm_form ~form_id:"CVTSI2SS_XMMss_MEMd" ~mnemonic:"cvtsi2ss" ~mode64:false rec_
  | Ok { iform = Some "CVTSI2SS_XMMss_MEMq"; _ } ->
      cvtsi2f_rm_form ~form_id:"CVTSI2SS_XMMss_MEMq" ~mnemonic:"cvtsi2ssq" ~mode64:true rec_
  | Ok { iform = Some "CVTTSD2SI_GPR32d_XMMsd"; _ } ->
      cvtf2i_rr_form ~form_id:"CVTTSD2SI_GPR32d_XMMsd" ~mode64:false rec_
  | Ok { iform = Some "CVTTSD2SI_GPR64q_XMMsd"; _ } ->
      cvtf2i_rr_form ~form_id:"CVTTSD2SI_GPR64q_XMMsd" ~mode64:true rec_
  | Ok { iform = Some "CVTTSD2SI_GPR32d_MEMsd"; _ } ->
      cvtf2i_rm_form ~form_id:"CVTTSD2SI_GPR32d_MEMsd" ~mode64:false rec_
  | Ok { iform = Some "CVTTSD2SI_GPR64q_MEMsd"; _ } ->
      cvtf2i_rm_form ~form_id:"CVTTSD2SI_GPR64q_MEMsd" ~mode64:true rec_
  (* Packed bitwise-logical family: {!Opcode.Xorpd}'s siblings, the same plain
     xmm-xmm/xmm-memory binop shape {!xmm_binop_rr_form}/{!xmm_binop_rm_form}
     already cover generically. ANDPS/ANDNPS/ORPS/XORPS are XED extension SSE
     (no mandatory prefix, {!requirement_of}'s SSE case); ANDPD/ANDNPD/ORPD
     are SSE2 (66 mandatory prefix), like XORPD. Confirmed against real GNU
     as (i686-linux-gnu-as 2.44) for every register-register and
     register<-memory spelling below: `0F 54/55/56/57` for the ps forms,
     `66 0F 54/55/56` for the pd forms. *)
  | Ok { iform = Some "ANDPS_XMMxud_XMMxud"; _ } ->
      xmm_binop_rr_form ~form_id:"ANDPS_XMMxud_XMMxud" ~mnemonic:"andps" rec_
  | Ok { iform = Some "ANDNPS_XMMxud_XMMxud"; _ } ->
      xmm_binop_rr_form ~form_id:"ANDNPS_XMMxud_XMMxud" ~mnemonic:"andnps" rec_
  | Ok { iform = Some "ORPS_XMMxud_XMMxud"; _ } ->
      xmm_binop_rr_form ~form_id:"ORPS_XMMxud_XMMxud" ~mnemonic:"orps" rec_
  | Ok { iform = Some "XORPS_XMMxud_XMMxud"; _ } ->
      xmm_binop_rr_form ~form_id:"XORPS_XMMxud_XMMxud" ~mnemonic:"xorps" rec_
  | Ok { iform = Some "ANDPD_XMMxuq_XMMxuq"; _ } ->
      xmm_binop_rr_form ~form_id:"ANDPD_XMMxuq_XMMxuq" ~mnemonic:"andpd" rec_
  | Ok { iform = Some "ANDNPD_XMMxuq_XMMxuq"; _ } ->
      xmm_binop_rr_form ~form_id:"ANDNPD_XMMxuq_XMMxuq" ~mnemonic:"andnpd" rec_
  | Ok { iform = Some "ORPD_XMMxuq_XMMxuq"; _ } ->
      xmm_binop_rr_form ~form_id:"ORPD_XMMxuq_XMMxuq" ~mnemonic:"orpd" rec_
  | Ok { iform = Some "ANDPS_XMMxud_MEMxud"; _ } ->
      xmm_binop_rm_form ~form_id:"ANDPS_XMMxud_MEMxud" ~mnemonic:"andps" rec_
  | Ok { iform = Some "ANDNPS_XMMxud_MEMxud"; _ } ->
      xmm_binop_rm_form ~form_id:"ANDNPS_XMMxud_MEMxud" ~mnemonic:"andnps" rec_
  | Ok { iform = Some "ORPS_XMMxud_MEMxud"; _ } ->
      xmm_binop_rm_form ~form_id:"ORPS_XMMxud_MEMxud" ~mnemonic:"orps" rec_
  | Ok { iform = Some "XORPS_XMMxud_MEMxud"; _ } ->
      xmm_binop_rm_form ~form_id:"XORPS_XMMxud_MEMxud" ~mnemonic:"xorps" rec_
  | Ok { iform = Some "ANDPD_XMMxuq_MEMxuq"; _ } ->
      xmm_binop_rm_form ~form_id:"ANDPD_XMMxuq_MEMxuq" ~mnemonic:"andpd" rec_
  | Ok { iform = Some "ANDNPD_XMMxuq_MEMxuq"; _ } ->
      xmm_binop_rm_form ~form_id:"ANDNPD_XMMxuq_MEMxuq" ~mnemonic:"andnpd" rec_
  | Ok { iform = Some "ORPD_XMMxuq_MEMxuq"; _ } ->
      xmm_binop_rm_form ~form_id:"ORPD_XMMxuq_MEMxuq" ~mnemonic:"orpd" rec_
  (* {!Opcode.Movapd}'s data-movement siblings: aligned/unaligned packed move, single/double
     precision. Same plain xmm-xmm/xmm-memory binop shape, load direction only - the register-
     register iform carries an explicit `_0F28`/`_0F10` suffix in XED's own naming, disambiguating
     it from the (unadmitted) `_0F29`/`_0F11` store direction the way MOVAPD's own iform already
     does. Confirmed against real GNU as (i686-linux-gnu-as 2.44): `0F 28`/`0F 10` for MOVAPS/
     MOVUPS (no mandatory prefix, XED extension SSE), `66 0F 10` for MOVUPD (SSE2, like MOVAPD). *)
  | Ok { iform = Some "MOVAPS_XMMps_XMMps_0F28"; _ } ->
      xmm_binop_rr_form ~form_id:"MOVAPS_XMMps_XMMps_0F28" ~mnemonic:"movaps" rec_
  | Ok { iform = Some "MOVUPS_XMMps_XMMps_0F10"; _ } ->
      xmm_binop_rr_form ~form_id:"MOVUPS_XMMps_XMMps_0F10" ~mnemonic:"movups" rec_
  | Ok { iform = Some "MOVUPD_XMMpd_XMMpd_0F10"; _ } ->
      xmm_binop_rr_form ~form_id:"MOVUPD_XMMpd_XMMpd_0F10" ~mnemonic:"movupd" rec_
  | Ok { iform = Some "MOVAPS_XMMps_MEMps"; _ } ->
      xmm_binop_rm_form ~form_id:"MOVAPS_XMMps_MEMps" ~mnemonic:"movaps" rec_
  | Ok { iform = Some "MOVUPS_XMMps_MEMps"; _ } ->
      xmm_binop_rm_form ~form_id:"MOVUPS_XMMps_MEMps" ~mnemonic:"movups" rec_
  | Ok { iform = Some "MOVUPD_XMMpd_MEMpd"; _ } ->
      xmm_binop_rm_form ~form_id:"MOVUPD_XMMpd_MEMpd" ~mnemonic:"movupd" rec_
  (* {!Opcode.Addsd}'s packed-arithmetic siblings: the prefix square for opcodes
     0x58/0x59/0x5C/0x5E, no mandatory prefix (ps, XED extension SSE) and 66 mandatory prefix
     (pd, SSE2) - the same plain xmm-xmm/xmm-memory binop shape, the same way the ANDPS and
     MOVAPS families completed the prefix square for their own opcode groups. Confirmed against
     real GNU as (i686-linux-gnu-as/x86_64-linux-gnu-as 2.44): `0F 58/59/5C/5E` for the ps forms,
     `66 0F 58/59/5C/5E` for the pd forms. *)
  | Ok { iform = Some "ADDPS_XMMps_XMMps"; _ } ->
      xmm_binop_rr_form ~form_id:"ADDPS_XMMps_XMMps" ~mnemonic:"addps" rec_
  | Ok { iform = Some "SUBPS_XMMps_XMMps"; _ } ->
      xmm_binop_rr_form ~form_id:"SUBPS_XMMps_XMMps" ~mnemonic:"subps" rec_
  | Ok { iform = Some "MULPS_XMMps_XMMps"; _ } ->
      xmm_binop_rr_form ~form_id:"MULPS_XMMps_XMMps" ~mnemonic:"mulps" rec_
  | Ok { iform = Some "DIVPS_XMMps_XMMps"; _ } ->
      xmm_binop_rr_form ~form_id:"DIVPS_XMMps_XMMps" ~mnemonic:"divps" rec_
  | Ok { iform = Some "ADDPD_XMMpd_XMMpd"; _ } ->
      xmm_binop_rr_form ~form_id:"ADDPD_XMMpd_XMMpd" ~mnemonic:"addpd" rec_
  | Ok { iform = Some "SUBPD_XMMpd_XMMpd"; _ } ->
      xmm_binop_rr_form ~form_id:"SUBPD_XMMpd_XMMpd" ~mnemonic:"subpd" rec_
  | Ok { iform = Some "MULPD_XMMpd_XMMpd"; _ } ->
      xmm_binop_rr_form ~form_id:"MULPD_XMMpd_XMMpd" ~mnemonic:"mulpd" rec_
  | Ok { iform = Some "DIVPD_XMMpd_XMMpd"; _ } ->
      xmm_binop_rr_form ~form_id:"DIVPD_XMMpd_XMMpd" ~mnemonic:"divpd" rec_
  | Ok { iform = Some "ADDPS_XMMps_MEMps"; _ } ->
      xmm_binop_rm_form ~form_id:"ADDPS_XMMps_MEMps" ~mnemonic:"addps" rec_
  | Ok { iform = Some "SUBPS_XMMps_MEMps"; _ } ->
      xmm_binop_rm_form ~form_id:"SUBPS_XMMps_MEMps" ~mnemonic:"subps" rec_
  | Ok { iform = Some "MULPS_XMMps_MEMps"; _ } ->
      xmm_binop_rm_form ~form_id:"MULPS_XMMps_MEMps" ~mnemonic:"mulps" rec_
  | Ok { iform = Some "DIVPS_XMMps_MEMps"; _ } ->
      xmm_binop_rm_form ~form_id:"DIVPS_XMMps_MEMps" ~mnemonic:"divps" rec_
  | Ok { iform = Some "ADDPD_XMMpd_MEMpd"; _ } ->
      xmm_binop_rm_form ~form_id:"ADDPD_XMMpd_MEMpd" ~mnemonic:"addpd" rec_
  | Ok { iform = Some "SUBPD_XMMpd_MEMpd"; _ } ->
      xmm_binop_rm_form ~form_id:"SUBPD_XMMpd_MEMpd" ~mnemonic:"subpd" rec_
  | Ok { iform = Some "MULPD_XMMpd_MEMpd"; _ } ->
      xmm_binop_rm_form ~form_id:"MULPD_XMMpd_MEMpd" ~mnemonic:"mulpd" rec_
  | Ok { iform = Some "DIVPD_XMMpd_MEMpd"; _ } ->
      xmm_binop_rm_form ~form_id:"DIVPD_XMMpd_MEMpd" ~mnemonic:"divpd" rec_
  (* {!Opcode.Addsd}/{!Opcode.Addss}/{!Opcode.Addps}/{!Opcode.Addpd}'s min/max siblings,
     overlooked in the earlier arithmetic-family survey passes: the same four-prefix-group shape
     at opcodes 0x5D (min)/0x5F (max) rather than 0x58/0x59/0x5C/0x5E, the same plain
     xmm-xmm/xmm-memory binop shape. MAXSS/MINSS/MAXPS/MINPS are XED extension SSE; MAXSD/MINSD/
     MAXPD/MINPD are SSE2. Confirmed against real GNU as (i686-linux-gnu-as/x86_64-linux-gnu-as
     2.44): `F3 0F 5F/5D` (ss), `F2 0F 5F/5D` (sd), `0F 5F/5D` (ps), `66 0F 5F/5D` (pd). *)
  | Ok { iform = Some "MAXSS_XMMss_XMMss"; _ } ->
      xmm_binop_rr_form ~form_id:"MAXSS_XMMss_XMMss" ~mnemonic:"maxss" rec_
  | Ok { iform = Some "MINSS_XMMss_XMMss"; _ } ->
      xmm_binop_rr_form ~form_id:"MINSS_XMMss_XMMss" ~mnemonic:"minss" rec_
  | Ok { iform = Some "MAXSD_XMMsd_XMMsd"; _ } ->
      xmm_binop_rr_form ~form_id:"MAXSD_XMMsd_XMMsd" ~mnemonic:"maxsd" rec_
  | Ok { iform = Some "MINSD_XMMsd_XMMsd"; _ } ->
      xmm_binop_rr_form ~form_id:"MINSD_XMMsd_XMMsd" ~mnemonic:"minsd" rec_
  | Ok { iform = Some "MAXPS_XMMps_XMMps"; _ } ->
      xmm_binop_rr_form ~form_id:"MAXPS_XMMps_XMMps" ~mnemonic:"maxps" rec_
  | Ok { iform = Some "MINPS_XMMps_XMMps"; _ } ->
      xmm_binop_rr_form ~form_id:"MINPS_XMMps_XMMps" ~mnemonic:"minps" rec_
  | Ok { iform = Some "MAXPD_XMMpd_XMMpd"; _ } ->
      xmm_binop_rr_form ~form_id:"MAXPD_XMMpd_XMMpd" ~mnemonic:"maxpd" rec_
  | Ok { iform = Some "MINPD_XMMpd_XMMpd"; _ } ->
      xmm_binop_rr_form ~form_id:"MINPD_XMMpd_XMMpd" ~mnemonic:"minpd" rec_
  | Ok { iform = Some "MAXSS_XMMss_MEMss"; _ } ->
      xmm_binop_rm_form ~form_id:"MAXSS_XMMss_MEMss" ~mnemonic:"maxss" rec_
  | Ok { iform = Some "MINSS_XMMss_MEMss"; _ } ->
      xmm_binop_rm_form ~form_id:"MINSS_XMMss_MEMss" ~mnemonic:"minss" rec_
  | Ok { iform = Some "MAXSD_XMMsd_MEMsd"; _ } ->
      xmm_binop_rm_form ~form_id:"MAXSD_XMMsd_MEMsd" ~mnemonic:"maxsd" rec_
  | Ok { iform = Some "MINSD_XMMsd_MEMsd"; _ } ->
      xmm_binop_rm_form ~form_id:"MINSD_XMMsd_MEMsd" ~mnemonic:"minsd" rec_
  | Ok { iform = Some "MAXPS_XMMps_MEMps"; _ } ->
      xmm_binop_rm_form ~form_id:"MAXPS_XMMps_MEMps" ~mnemonic:"maxps" rec_
  | Ok { iform = Some "MINPS_XMMps_MEMps"; _ } ->
      xmm_binop_rm_form ~form_id:"MINPS_XMMps_MEMps" ~mnemonic:"minps" rec_
  | Ok { iform = Some "MAXPD_XMMpd_MEMpd"; _ } ->
      xmm_binop_rm_form ~form_id:"MAXPD_XMMpd_MEMpd" ~mnemonic:"maxpd" rec_
  | Ok { iform = Some "MINPD_XMMpd_MEMpd"; _ } ->
      xmm_binop_rm_form ~form_id:"MINPD_XMMpd_MEMpd" ~mnemonic:"minpd" rec_
  (* {!Opcode.Sqrtss}'s family: the first genuinely unary member admitted through
     {!xmm_binop_rr_form}/{!xmm_binop_rm_form} - XED's REG0 is still [rw] for the scalar forms
     (a scalar op preserves the destination's upper bits) exactly like every binop above, so
     both normalizers apply unchanged even though the architectural operation itself only reads
     one operand. Same four-prefix-group shape at opcode 0x51. SQRTSS/SQRTPS are XED extension
     SSE; SQRTSD/SQRTPD are SSE2. Confirmed against real GNU as (i686-linux-gnu-as/
     x86_64-linux-gnu-as 2.44): `F3 0F 51` (ss), `F2 0F 51` (sd), `0F 51` (ps), `66 0F 51` (pd). *)
  | Ok { iform = Some "SQRTSS_XMMss_XMMss"; _ } ->
      xmm_binop_rr_form ~form_id:"SQRTSS_XMMss_XMMss" ~mnemonic:"sqrtss" rec_
  | Ok { iform = Some "SQRTSD_XMMsd_XMMsd"; _ } ->
      xmm_binop_rr_form ~form_id:"SQRTSD_XMMsd_XMMsd" ~mnemonic:"sqrtsd" rec_
  | Ok { iform = Some "SQRTPS_XMMps_XMMps"; _ } ->
      xmm_binop_rr_form ~form_id:"SQRTPS_XMMps_XMMps" ~mnemonic:"sqrtps" rec_
  | Ok { iform = Some "SQRTPD_XMMpd_XMMpd"; _ } ->
      xmm_binop_rr_form ~form_id:"SQRTPD_XMMpd_XMMpd" ~mnemonic:"sqrtpd" rec_
  | Ok { iform = Some "SQRTSS_XMMss_MEMss"; _ } ->
      xmm_binop_rm_form ~form_id:"SQRTSS_XMMss_MEMss" ~mnemonic:"sqrtss" rec_
  | Ok { iform = Some "SQRTSD_XMMsd_MEMsd"; _ } ->
      xmm_binop_rm_form ~form_id:"SQRTSD_XMMsd_MEMsd" ~mnemonic:"sqrtsd" rec_
  | Ok { iform = Some "SQRTPS_XMMps_MEMps"; _ } ->
      xmm_binop_rm_form ~form_id:"SQRTPS_XMMps_MEMps" ~mnemonic:"sqrtps" rec_
  | Ok { iform = Some "SQRTPD_XMMpd_MEMpd"; _ } ->
      xmm_binop_rm_form ~form_id:"SQRTPD_XMMpd_MEMpd" ~mnemonic:"sqrtpd" rec_
  (* {!xmm_binop_rr_form}/{!xmm_binop_rm_form}'s own generic REG0/REG1-or-MEM0 shape covers
     {!Opcode.Cvtps2pd}/{!Opcode.Cvtpd2ps} too (none/66 mandatory prefix, {!Cvtsd2ss}/
     {!Cvtss2sd}'s own opcode byte 0x5A) - both directions confirmed unambiguous against real
     GNU as, unlike the VEX sibling below. *)
  | Ok { iform = Some "CVTPS2PD_XMMpd_XMMq"; _ } ->
      xmm_binop_rr_form ~form_id:"CVTPS2PD_XMMpd_XMMq" ~mnemonic:"cvtps2pd" rec_
  | Ok { iform = Some "CVTPS2PD_XMMpd_MEMq"; _ } ->
      xmm_binop_rm_form ~form_id:"CVTPS2PD_XMMpd_MEMq" ~mnemonic:"cvtps2pd" rec_
  | Ok { iform = Some "CVTPD2PS_XMMps_XMMpd"; _ } ->
      xmm_binop_rr_form ~form_id:"CVTPD2PS_XMMps_XMMpd" ~mnemonic:"cvtpd2ps" rec_
  | Ok { iform = Some "CVTPD2PS_XMMps_MEMpd"; _ } ->
      xmm_binop_rm_form ~form_id:"CVTPD2PS_XMMps_MEMpd" ~mnemonic:"cvtpd2ps" rec_
  (* {!Opcode.Andps}'s own mandatory-prefix-free/66 group at opcode 0x14/0x15: the first
     genuine two-source-operand packed binop, the same plain xmm-xmm/xmm-memory shape
     {!xmm_binop_rr_form}/{!xmm_binop_rm_form} already cover generically. UNPCKLPS/UNPCKHPS are
     XED extension SSE; UNPCKLPD/UNPCKHPD are SSE2. Confirmed against real GNU as
     (i686-linux-gnu-as/x86_64-linux-gnu-as 2.44): `0F 14/15` for the ps forms, `66 0F 14/15` for
     the pd forms. *)
  | Ok { iform = Some "UNPCKLPS_XMMps_XMMq"; _ } ->
      xmm_binop_rr_form ~form_id:"UNPCKLPS_XMMps_XMMq" ~mnemonic:"unpcklps" rec_
  | Ok { iform = Some "UNPCKHPS_XMMps_XMMdq"; _ } ->
      xmm_binop_rr_form ~form_id:"UNPCKHPS_XMMps_XMMdq" ~mnemonic:"unpckhps" rec_
  | Ok { iform = Some "UNPCKLPD_XMMpd_XMMq"; _ } ->
      xmm_binop_rr_form ~form_id:"UNPCKLPD_XMMpd_XMMq" ~mnemonic:"unpcklpd" rec_
  | Ok { iform = Some "UNPCKHPD_XMMpd_XMMq"; _ } ->
      xmm_binop_rr_form ~form_id:"UNPCKHPD_XMMpd_XMMq" ~mnemonic:"unpckhpd" rec_
  | Ok { iform = Some "UNPCKLPS_XMMps_MEMdq"; _ } ->
      xmm_binop_rm_form ~form_id:"UNPCKLPS_XMMps_MEMdq" ~mnemonic:"unpcklps" rec_
  | Ok { iform = Some "UNPCKHPS_XMMps_MEMdq"; _ } ->
      xmm_binop_rm_form ~form_id:"UNPCKHPS_XMMps_MEMdq" ~mnemonic:"unpckhps" rec_
  | Ok { iform = Some "UNPCKLPD_XMMpd_MEMdq"; _ } ->
      xmm_binop_rm_form ~form_id:"UNPCKLPD_XMMpd_MEMdq" ~mnemonic:"unpcklpd" rec_
  | Ok { iform = Some "UNPCKHPD_XMMpd_MEMdq"; _ } ->
      xmm_binop_rm_form ~form_id:"UNPCKHPD_XMMpd_MEMdq" ~mnemonic:"unpckhpd" rec_
  (* {!Opcode.Unpcklps}'s integer-SIMD sibling, same plain xmm-xmm/xmm-memory shape:
     PUNPCKLQDQ/PUNPCKHQDQ (opcode 0x6C/0x6D, 66-mandatory-prefix only - no non-66 sibling, XED
     extension SSE2). Confirmed against real GNU as (i686-linux-gnu-as/x86_64-linux-gnu-as
     2.44): `66 0F 6C/6D`. *)
  | Ok { iform = Some "PUNPCKLQDQ_XMMdq_XMMq"; _ } ->
      xmm_binop_rr_form ~form_id:"PUNPCKLQDQ_XMMdq_XMMq" ~mnemonic:"punpcklqdq" rec_
  | Ok { iform = Some "PUNPCKHQDQ_XMMdq_XMMq"; _ } ->
      xmm_binop_rr_form ~form_id:"PUNPCKHQDQ_XMMdq_XMMq" ~mnemonic:"punpckhqdq" rec_
  | Ok { iform = Some "PUNPCKLQDQ_XMMdq_MEMdq"; _ } ->
      xmm_binop_rm_form ~form_id:"PUNPCKLQDQ_XMMdq_MEMdq" ~mnemonic:"punpcklqdq" rec_
  | Ok { iform = Some "PUNPCKHQDQ_XMMdq_MEMdq"; _ } ->
      xmm_binop_rm_form ~form_id:"PUNPCKHQDQ_XMMdq_MEMdq" ~mnemonic:"punpckhqdq" rec_
  (* SHUFPS/SHUFPD ({!xmm_binop_imm_rr_form}'s own doc comment): the first
     XMM-immediate-carrying legacy shape. SHUFPS is XED extension SSE; SHUFPD is SSE2. *)
  | Ok { iform = Some "SHUFPS_XMMps_XMMps_IMMb"; _ } ->
      xmm_binop_imm_rr_form ~form_id:"SHUFPS_XMMps_XMMps_IMMb" ~mnemonic:"shufps" rec_
  | Ok { iform = Some "SHUFPS_XMMps_MEMps_IMMb"; _ } ->
      xmm_binop_imm_rm_form ~form_id:"SHUFPS_XMMps_MEMps_IMMb" ~mnemonic:"shufps" rec_
  | Ok { iform = Some "SHUFPD_XMMpd_XMMpd_IMMb"; _ } ->
      xmm_binop_imm_rr_form ~form_id:"SHUFPD_XMMpd_XMMpd_IMMb" ~mnemonic:"shufpd" rec_
  | Ok { iform = Some "SHUFPD_XMMpd_MEMpd_IMMb"; _ } ->
      xmm_binop_imm_rm_form ~form_id:"SHUFPD_XMMpd_MEMpd_IMMb" ~mnemonic:"shufpd" rec_
  (* CMPSS/CMPSD/CMPPS/CMPPD ({!xmm_binop_imm_rr_form}'s own doc comment): {!Addsd}'s
     own four-mandatory-prefix-group shape at opcode 0xC2, with {!Shufps}'s trailing imm8.
     CMPSS/CMPPS are XED extension SSE; CMPSD/CMPPD are SSE2. Only the canonical
     [cmp{ss,sd,ps,pd} $imm, ...] spelling is admitted, not the [cmpeq]/[cmplt]/etc.
     mnemonic-suffix pseudo-aliases GNU as also accepts - a separately scoped follow-up. Note
     CMPSD's own iform (["CMPSD_XMM_XMMsd_XMMsd_IMMb"]) is disjoint from the unrelated string
     instruction bare iform ["CMPSD"] (REP-prefixed string compare) - no ambiguity here since
     dispatch is by exact iform string, but the two must not be confused when reading exports. *)
  | Ok { iform = Some "CMPSS_XMMss_XMMss_IMMb"; _ } ->
      xmm_binop_imm_rr_form ~form_id:"CMPSS_XMMss_XMMss_IMMb" ~mnemonic:"cmpss" rec_
  | Ok { iform = Some "CMPSS_XMMss_MEMss_IMMb"; _ } ->
      xmm_binop_imm_rm_form ~form_id:"CMPSS_XMMss_MEMss_IMMb" ~mnemonic:"cmpss" rec_
  | Ok { iform = Some "CMPSD_XMM_XMMsd_XMMsd_IMMb"; _ } ->
      xmm_binop_imm_rr_form ~form_id:"CMPSD_XMM_XMMsd_XMMsd_IMMb" ~mnemonic:"cmpsd" rec_
  | Ok { iform = Some "CMPSD_XMM_XMMsd_MEMsd_IMMb"; _ } ->
      xmm_binop_imm_rm_form ~form_id:"CMPSD_XMM_XMMsd_MEMsd_IMMb" ~mnemonic:"cmpsd" rec_
  | Ok { iform = Some "CMPPS_XMMps_XMMps_IMMb"; _ } ->
      xmm_binop_imm_rr_form ~form_id:"CMPPS_XMMps_XMMps_IMMb" ~mnemonic:"cmpps" rec_
  | Ok { iform = Some "CMPPS_XMMps_MEMps_IMMb"; _ } ->
      xmm_binop_imm_rm_form ~form_id:"CMPPS_XMMps_MEMps_IMMb" ~mnemonic:"cmpps" rec_
  | Ok { iform = Some "CMPPD_XMMpd_XMMpd_IMMb"; _ } ->
      xmm_binop_imm_rr_form ~form_id:"CMPPD_XMMpd_XMMpd_IMMb" ~mnemonic:"cmppd" rec_
  | Ok { iform = Some "CMPPD_XMMpd_MEMpd_IMMb"; _ } ->
      xmm_binop_imm_rm_form ~form_id:"CMPPD_XMMpd_MEMpd_IMMb" ~mnemonic:"cmppd" rec_
  (* The first x86 vector-extension (AVX/VEX) admission ({!vex_binop_rrr_form}'s own doc
     comment): VADDSD/VSUBSD/VMULSD/VDIVSD's register-register form, and now also the
     register<-memory sibling ({!vex_binop_rr_mem_form}'s own doc comment) - YMM, three-byte
     VEX and EVEX remain out of scope. *)
  | Ok { iform = Some "VADDSD_XMMdq_XMMdq_XMMq"; _ } ->
      vex_binop_rrr_form ~form_id:"VADDSD_XMMdq_XMMdq_XMMq" ~mnemonic:"vaddsd" rec_
  | Ok { iform = Some "VSUBSD_XMMdq_XMMdq_XMMq"; _ } ->
      vex_binop_rrr_form ~form_id:"VSUBSD_XMMdq_XMMdq_XMMq" ~mnemonic:"vsubsd" rec_
  | Ok { iform = Some "VMULSD_XMMdq_XMMdq_XMMq"; _ } ->
      vex_binop_rrr_form ~form_id:"VMULSD_XMMdq_XMMdq_XMMq" ~mnemonic:"vmulsd" rec_
  | Ok { iform = Some "VDIVSD_XMMdq_XMMdq_XMMq"; _ } ->
      vex_binop_rrr_form ~form_id:"VDIVSD_XMMdq_XMMdq_XMMq" ~mnemonic:"vdivsd" rec_
  | Ok { iform = Some "VADDSD_XMMdq_XMMdq_MEMq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VADDSD_XMMdq_XMMdq_MEMq" ~mnemonic:"vaddsd" rec_
  | Ok { iform = Some "VSUBSD_XMMdq_XMMdq_MEMq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VSUBSD_XMMdq_XMMdq_MEMq" ~mnemonic:"vsubsd" rec_
  | Ok { iform = Some "VMULSD_XMMdq_XMMdq_MEMq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMULSD_XMMdq_XMMdq_MEMq" ~mnemonic:"vmulsd" rec_
  | Ok { iform = Some "VDIVSD_XMMdq_XMMdq_MEMq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VDIVSD_XMMdq_XMMdq_MEMq" ~mnemonic:"vdivsd" rec_
  (* {!Vaddsd}'s scalar-single [VEX.LIG.F3.0F.WIG] sibling group: same two
     [vex_binop_*_form] shapes, XED's own "d" (dword/32-bit) suffix replacing "q"
     (qword/64-bit) since single precision is 32 bits, not a new shape. *)
  | Ok { iform = Some "VADDSS_XMMdq_XMMdq_XMMd"; _ } ->
      vex_binop_rrr_form ~form_id:"VADDSS_XMMdq_XMMdq_XMMd" ~mnemonic:"vaddss" rec_
  | Ok { iform = Some "VSUBSS_XMMdq_XMMdq_XMMd"; _ } ->
      vex_binop_rrr_form ~form_id:"VSUBSS_XMMdq_XMMdq_XMMd" ~mnemonic:"vsubss" rec_
  | Ok { iform = Some "VMULSS_XMMdq_XMMdq_XMMd"; _ } ->
      vex_binop_rrr_form ~form_id:"VMULSS_XMMdq_XMMdq_XMMd" ~mnemonic:"vmulss" rec_
  | Ok { iform = Some "VDIVSS_XMMdq_XMMdq_XMMd"; _ } ->
      vex_binop_rrr_form ~form_id:"VDIVSS_XMMdq_XMMdq_XMMd" ~mnemonic:"vdivss" rec_
  | Ok { iform = Some "VADDSS_XMMdq_XMMdq_MEMd"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VADDSS_XMMdq_XMMdq_MEMd" ~mnemonic:"vaddss" rec_
  | Ok { iform = Some "VSUBSS_XMMdq_XMMdq_MEMd"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VSUBSS_XMMdq_XMMdq_MEMd" ~mnemonic:"vsubss" rec_
  | Ok { iform = Some "VMULSS_XMMdq_XMMdq_MEMd"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMULSS_XMMdq_XMMdq_MEMd" ~mnemonic:"vmulss" rec_
  | Ok { iform = Some "VDIVSS_XMMdq_XMMdq_MEMd"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VDIVSS_XMMdq_XMMdq_MEMd" ~mnemonic:"vdivss" rec_
  (* {!Vaddsd}'s packed-single/-double siblings: same two [vex_binop_*_form]
     shapes, completing the [pp] square (F2/F3/none/66) for VEX opcodes
     0x58/0x59/0x5C/0x5E the way the legacy ADDPS/ADDPD slice completed it for the
     non-VEX encoding. XED uses "dq" (128-bit) throughout since these are packed,
     not scalar-width, operands. *)
  | Ok { iform = Some "VADDPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VADDPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vaddps" rec_
  | Ok { iform = Some "VSUBPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VSUBPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vsubps" rec_
  | Ok { iform = Some "VMULPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VMULPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vmulps" rec_
  | Ok { iform = Some "VDIVPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VDIVPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vdivps" rec_
  | Ok { iform = Some "VADDPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VADDPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vaddps" rec_
  | Ok { iform = Some "VSUBPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VSUBPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vsubps" rec_
  | Ok { iform = Some "VMULPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMULPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vmulps" rec_
  | Ok { iform = Some "VDIVPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VDIVPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vdivps" rec_
  | Ok { iform = Some "VADDPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VADDPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vaddpd" rec_
  | Ok { iform = Some "VSUBPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VSUBPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vsubpd" rec_
  | Ok { iform = Some "VMULPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VMULPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vmulpd" rec_
  | Ok { iform = Some "VDIVPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VDIVPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vdivpd" rec_
  | Ok { iform = Some "VADDPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VADDPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vaddpd" rec_
  | Ok { iform = Some "VSUBPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VSUBPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vsubpd" rec_
  | Ok { iform = Some "VMULPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMULPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vmulpd" rec_
  | Ok { iform = Some "VDIVPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VDIVPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vdivpd" rec_
  (* {!Vaddps}/{!Vaddpd}'s VEX bitwise-logical siblings: same two
     [vex_binop_*_form] shapes, the VEX counterpart of the legacy
     {!Andps}/{!Andnps}/{!Orps}/{!Xorps}/{!Andpd}/{!Andnpd}/{!Orpd}/{!Xorpd}
     family - opcodes 0x54-0x57 instead of 0x58/0x59/0x5C/0x5E, no [Vxorps]
     scalar sibling since AND/OR/XOR are inherently packed-only bitwise ops. *)
  | Ok { iform = Some "VANDPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VANDPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vandps" rec_
  | Ok { iform = Some "VANDNPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VANDNPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vandnps" rec_
  | Ok { iform = Some "VORPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VORPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vorps" rec_
  | Ok { iform = Some "VXORPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VXORPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vxorps" rec_
  | Ok { iform = Some "VANDPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VANDPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vandps" rec_
  | Ok { iform = Some "VANDNPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VANDNPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vandnps" rec_
  | Ok { iform = Some "VORPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VORPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vorps" rec_
  | Ok { iform = Some "VXORPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VXORPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vxorps" rec_
  | Ok { iform = Some "VANDPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VANDPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vandpd" rec_
  | Ok { iform = Some "VANDNPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VANDNPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vandnpd" rec_
  | Ok { iform = Some "VORPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VORPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vorpd" rec_
  | Ok { iform = Some "VXORPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VXORPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vxorpd" rec_
  | Ok { iform = Some "VANDPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VANDPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vandpd" rec_
  | Ok { iform = Some "VANDNPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VANDNPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vandnpd" rec_
  | Ok { iform = Some "VORPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VORPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vorpd" rec_
  | Ok { iform = Some "VXORPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VXORPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vxorpd" rec_
  (* {!Opcode.Vunpcklps}'s own doc comment: the VEX sibling of the legacy
     UNPCKLPS/UNPCKHPS/UNPCKLPD/UNPCKHPD family, opcodes 0x14/0x15 - same two
     [vex_binop_*_form] shapes, a genuine two-source-operand binop like {!Vandps}. XED extension
     AVX. Confirmed against real GNU as (i686-linux-gnu-as/x86_64-linux-gnu-as 2.44). *)
  | Ok { iform = Some "VUNPCKLPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VUNPCKLPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vunpcklps" rec_
  | Ok { iform = Some "VUNPCKHPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VUNPCKHPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vunpckhps" rec_
  | Ok { iform = Some "VUNPCKLPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VUNPCKLPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vunpcklpd" rec_
  | Ok { iform = Some "VUNPCKHPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VUNPCKHPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vunpckhpd" rec_
  | Ok { iform = Some "VUNPCKLPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VUNPCKLPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vunpcklps" rec_
  | Ok { iform = Some "VUNPCKHPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VUNPCKHPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vunpckhps" rec_
  | Ok { iform = Some "VUNPCKLPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VUNPCKLPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vunpcklpd" rec_
  | Ok { iform = Some "VUNPCKHPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VUNPCKHPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vunpckhpd" rec_
  (* {!Opcode.Vpunpcklqdq}'s own doc comment: the VEX sibling of the legacy
     PUNPCKLQDQ/PUNPCKHQDQ family, opcode 0x6C/0x6D, 66-mandatory-prefix only. XED extension
     AVX. Confirmed against real GNU as (i686-linux-gnu-as/x86_64-linux-gnu-as 2.44). *)
  | Ok { iform = Some "VPUNPCKLQDQ_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VPUNPCKLQDQ_XMMdq_XMMdq_XMMdq" ~mnemonic:"vpunpcklqdq" rec_
  | Ok { iform = Some "VPUNPCKHQDQ_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VPUNPCKHQDQ_XMMdq_XMMdq_XMMdq" ~mnemonic:"vpunpckhqdq" rec_
  | Ok { iform = Some "VPUNPCKLQDQ_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VPUNPCKLQDQ_XMMdq_XMMdq_MEMdq" ~mnemonic:"vpunpcklqdq" rec_
  | Ok { iform = Some "VPUNPCKHQDQ_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VPUNPCKHQDQ_XMMdq_XMMdq_MEMdq" ~mnemonic:"vpunpckhqdq" rec_
  (* {!Vaddsd}/{!Vandps}'s min/max siblings: the VEX counterpart of the legacy
     MAXSD/MINSD/MAXSS/MINSS/MAXPS/MINPS/MAXPD/MINPD family (opcodes 0x5F/0x5D instead of
     0x54-0x57/0x58/0x59/0x5C/0x5E), same two [vex_binop_*_form] shapes. Confirmed against
     real GNU as (i686-linux-gnu-as/x86_64-linux-gnu-as 2.44): `c5 f3 5f/5d` (sd, pp=3),
     `c5 f2 5f/5d` (ss, pp=2), `c5 f0 5f/5d` (ps, pp=0), `c5 f1 5f/5d` (pd, pp=1). *)
  | Ok { iform = Some "VMAXSD_XMMdq_XMMdq_XMMq"; _ } ->
      vex_binop_rrr_form ~form_id:"VMAXSD_XMMdq_XMMdq_XMMq" ~mnemonic:"vmaxsd" rec_
  | Ok { iform = Some "VMINSD_XMMdq_XMMdq_XMMq"; _ } ->
      vex_binop_rrr_form ~form_id:"VMINSD_XMMdq_XMMdq_XMMq" ~mnemonic:"vminsd" rec_
  | Ok { iform = Some "VMAXSD_XMMdq_XMMdq_MEMq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMAXSD_XMMdq_XMMdq_MEMq" ~mnemonic:"vmaxsd" rec_
  | Ok { iform = Some "VMINSD_XMMdq_XMMdq_MEMq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMINSD_XMMdq_XMMdq_MEMq" ~mnemonic:"vminsd" rec_
  | Ok { iform = Some "VMAXSS_XMMdq_XMMdq_XMMd"; _ } ->
      vex_binop_rrr_form ~form_id:"VMAXSS_XMMdq_XMMdq_XMMd" ~mnemonic:"vmaxss" rec_
  | Ok { iform = Some "VMINSS_XMMdq_XMMdq_XMMd"; _ } ->
      vex_binop_rrr_form ~form_id:"VMINSS_XMMdq_XMMdq_XMMd" ~mnemonic:"vminss" rec_
  | Ok { iform = Some "VMAXSS_XMMdq_XMMdq_MEMd"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMAXSS_XMMdq_XMMdq_MEMd" ~mnemonic:"vmaxss" rec_
  | Ok { iform = Some "VMINSS_XMMdq_XMMdq_MEMd"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMINSS_XMMdq_XMMdq_MEMd" ~mnemonic:"vminss" rec_
  | Ok { iform = Some "VMAXPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VMAXPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vmaxps" rec_
  | Ok { iform = Some "VMINPS_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VMINPS_XMMdq_XMMdq_XMMdq" ~mnemonic:"vminps" rec_
  | Ok { iform = Some "VMAXPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMAXPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vmaxps" rec_
  | Ok { iform = Some "VMINPS_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMINPS_XMMdq_XMMdq_MEMdq" ~mnemonic:"vminps" rec_
  | Ok { iform = Some "VMAXPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VMAXPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vmaxpd" rec_
  | Ok { iform = Some "VMINPD_XMMdq_XMMdq_XMMdq"; _ } ->
      vex_binop_rrr_form ~form_id:"VMINPD_XMMdq_XMMdq_XMMdq" ~mnemonic:"vminpd" rec_
  | Ok { iform = Some "VMAXPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMAXPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vmaxpd" rec_
  | Ok { iform = Some "VMINPD_XMMdq_XMMdq_MEMdq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VMINPD_XMMdq_XMMdq_MEMdq" ~mnemonic:"vminpd" rec_
  (* {!Vaddsd}/{!Vmaxsd}'s sqrt sibling: the VEX counterpart of the legacy
     SQRTSD/SQRTSS/SQRTPS/SQRTPD family (opcode 0x51). The scalar forms (SD/SS) keep the same
     REG0/REG1/REG2-or-MEM0 shape every other [vex_binop_*_form] family uses - confirmed against
     real GNU as that [src1] ([vvvv]) is a real, required operand there even though the CPU only
     uses it to merge the destination's upper bits, not as a second arithmetic input, so the
     byte-level operand-to-field mapping is identical to {!Vmaxsd}'s. The packed forms (PS/PD)
     are genuinely two-operand instead - real GNU as rejects a third operand outright - so those
     use the new {!vex_unop_rr_form}/{!vex_unop_rr_mem_form} pair. Confirmed against real GNU as
     (i686-linux-gnu-as/x86_64-linux-gnu-as 2.44): `c5 eb 51 cb`/`c5 ea 51 cb` (sd/ss
     register-register), `c5 f8 51 ca`/`c5 f9 51 ca` (ps/pd register-register), `c5 eb 51 08`
     (sd register<-memory), `c5 f8 51 08` (ps register<-memory). *)
  | Ok { iform = Some "VSQRTSD_XMMdq_XMMdq_XMMq"; _ } ->
      vex_binop_rrr_form ~form_id:"VSQRTSD_XMMdq_XMMdq_XMMq" ~mnemonic:"vsqrtsd" rec_
  | Ok { iform = Some "VSQRTSD_XMMdq_XMMdq_MEMq"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VSQRTSD_XMMdq_XMMdq_MEMq" ~mnemonic:"vsqrtsd" rec_
  | Ok { iform = Some "VSQRTSS_XMMdq_XMMdq_XMMd"; _ } ->
      vex_binop_rrr_form ~form_id:"VSQRTSS_XMMdq_XMMdq_XMMd" ~mnemonic:"vsqrtss" rec_
  | Ok { iform = Some "VSQRTSS_XMMdq_XMMdq_MEMd"; _ } ->
      vex_binop_rr_mem_form ~form_id:"VSQRTSS_XMMdq_XMMdq_MEMd" ~mnemonic:"vsqrtss" rec_
  | Ok { iform = Some "VSQRTPS_XMMdq_XMMdq"; _ } ->
      vex_unop_rr_form ~form_id:"VSQRTPS_XMMdq_XMMdq" ~mnemonic:"vsqrtps" rec_
  | Ok { iform = Some "VSQRTPS_XMMdq_MEMdq"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VSQRTPS_XMMdq_MEMdq" ~mnemonic:"vsqrtps" rec_
  | Ok { iform = Some "VSQRTPD_XMMdq_XMMdq"; _ } ->
      vex_unop_rr_form ~form_id:"VSQRTPD_XMMdq_XMMdq" ~mnemonic:"vsqrtpd" rec_
  | Ok { iform = Some "VSQRTPD_XMMdq_MEMdq"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VSQRTPD_XMMdq_MEMdq" ~mnemonic:"vsqrtpd" rec_
  (* {!Vmovaps}'s own doc comment: the VEX sibling of the legacy
     MOVAPS/MOVUPS/MOVAPD/MOVUPD family (opcodes 0x28/0x10), reusing {!vex_unop_rr_form}/
     {!vex_unop_rr_mem_form} unchanged - the same genuinely-two-operand shape {!Vsqrtps} already
     established. Only the low-numbered register-register iform (XED's own [_28]/[_10] suffix)
     and the register<-memory load direction are admitted here, matching the legacy
     {!Movapd}/{!Movaps} precedent exactly: the [_29]/[_11]-suffixed register-register iform is
     the same redundant alternate encoding real GNU as never selects, and the real
     [MEMdq<-XMMdq] store direction (a genuinely distinct, separately admittable opcode, not a
     redundancy) is left as a named follow-up. Confirmed against real GNU as
     (i686-linux-gnu-as/x86_64-linux-gnu-as 2.44): `c5 f8 28 ca` (vmovaps register-register),
     `c5 f8 28 08`/`c5 f8 10 08`/`c5 f9 28 08`/`c5 f9 10 08` (register<-memory). *)
  | Ok { iform = Some "VMOVAPS_XMMdq_XMMdq_28"; _ } ->
      vex_unop_rr_form ~form_id:"VMOVAPS_XMMdq_XMMdq_28" ~mnemonic:"vmovaps" rec_
  | Ok { iform = Some "VMOVAPS_XMMdq_MEMdq"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VMOVAPS_XMMdq_MEMdq" ~mnemonic:"vmovaps" rec_
  | Ok { iform = Some "VMOVUPS_XMMdq_XMMdq_10"; _ } ->
      vex_unop_rr_form ~form_id:"VMOVUPS_XMMdq_XMMdq_10" ~mnemonic:"vmovups" rec_
  | Ok { iform = Some "VMOVUPS_XMMdq_MEMdq"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VMOVUPS_XMMdq_MEMdq" ~mnemonic:"vmovups" rec_
  | Ok { iform = Some "VMOVAPD_XMMdq_XMMdq_28"; _ } ->
      vex_unop_rr_form ~form_id:"VMOVAPD_XMMdq_XMMdq_28" ~mnemonic:"vmovapd" rec_
  | Ok { iform = Some "VMOVAPD_XMMdq_MEMdq"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VMOVAPD_XMMdq_MEMdq" ~mnemonic:"vmovapd" rec_
  | Ok { iform = Some "VMOVUPD_XMMdq_XMMdq_10"; _ } ->
      vex_unop_rr_form ~form_id:"VMOVUPD_XMMdq_XMMdq_10" ~mnemonic:"vmovupd" rec_
  | Ok { iform = Some "VMOVUPD_XMMdq_MEMdq"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VMOVUPD_XMMdq_MEMdq" ~mnemonic:"vmovupd" rec_
  (* {!Opcode.Vcomisd}'s own doc comment: the VEX sibling of the legacy
     COMISD/UCOMISD/COMISS/UCOMISS family (opcodes 0x2F/0x2E), reusing {!vex_unop_rr_form}/
     {!vex_unop_rr_mem_form} unchanged - both operands are read-only here (XED's REG0 is
     [rw = "r"], not "w": the real result goes to EFLAGS, not a register), but
     {!vex_unop_rr_form} reads [role_of_rw] generically off each operand's own [rw] fact rather
     than assuming the destination is writable, so this is a clean reuse. Confirmed against real
     GNU as (i686-linux-gnu-as/x86_64-linux-gnu-as 2.44): `c5 f9 2f ca`/`c5 f9 2e ca` (vcomisd/
     vucomisd register-register), `c5 f8 2f ca`/`c5 f8 2e ca` (vcomiss/vucomiss
     register-register), `c5 f9 2f 08`/`c5 f9 2e 08`/`c5 f8 2f 08`/`c5 f8 2e 08`
     (register<-memory, same order). *)
  | Ok { iform = Some "VCOMISD_XMMq_XMMq"; _ } ->
      vex_unop_rr_form ~form_id:"VCOMISD_XMMq_XMMq" ~mnemonic:"vcomisd" rec_
  | Ok { iform = Some "VCOMISD_XMMq_MEMq"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VCOMISD_XMMq_MEMq" ~mnemonic:"vcomisd" rec_
  | Ok { iform = Some "VUCOMISD_XMMdq_XMMq"; _ } ->
      vex_unop_rr_form ~form_id:"VUCOMISD_XMMdq_XMMq" ~mnemonic:"vucomisd" rec_
  | Ok { iform = Some "VUCOMISD_XMMdq_MEMq"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VUCOMISD_XMMdq_MEMq" ~mnemonic:"vucomisd" rec_
  | Ok { iform = Some "VCOMISS_XMMd_XMMd"; _ } ->
      vex_unop_rr_form ~form_id:"VCOMISS_XMMd_XMMd" ~mnemonic:"vcomiss" rec_
  | Ok { iform = Some "VCOMISS_XMMd_MEMd"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VCOMISS_XMMd_MEMd" ~mnemonic:"vcomiss" rec_
  | Ok { iform = Some "VUCOMISS_XMMdq_XMMd"; _ } ->
      vex_unop_rr_form ~form_id:"VUCOMISS_XMMdq_XMMd" ~mnemonic:"vucomiss" rec_
  | Ok { iform = Some "VUCOMISS_XMMdq_MEMd"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VUCOMISS_XMMdq_MEMd" ~mnemonic:"vucomiss" rec_
  (* {!Opcode.Vcvtps2pd}'s own doc comment: the VEX sibling of the legacy
     CVTPS2PD/CVTPD2PS family (opcode 0x5A), reusing {!vex_unop_rr_form}/{!vex_unop_rr_mem_form}
     unchanged. {!Vcvtps2pd} admits both directions (confirmed unambiguous against real GNU as:
     `c5 f8 5a ca` register-register, `c5 f8 5a 08` register<-memory); {!Vcvtpd2ps} admits only
     the register-register iform (`c5 f9 5a ca`) - its two vex-space memory iforms
     (VCVTPD2PS_XMMdq_MEMdq's VEX.128 m128 source and VCVTPD2PS_XMMdq_MEMqq's VEX.256 m256
     source) render as the identical bare GAS spelling [vcvtpd2ps mem, %xmmN], which real GNU as
     rejects outright ("operand size mismatch") absent the [x]/[y]-suffix disambiguation this
     project's parser does not implement - left as a named follow-up, matching
     {!Opcode.Vcvtpd2ps}'s own doc comment. *)
  | Ok { iform = Some "VCVTPS2PD_XMMdq_XMMq"; _ } ->
      vex_unop_rr_form ~form_id:"VCVTPS2PD_XMMdq_XMMq" ~mnemonic:"vcvtps2pd" rec_
  | Ok { iform = Some "VCVTPS2PD_XMMdq_MEMq"; _ } ->
      vex_unop_rr_mem_form ~form_id:"VCVTPS2PD_XMMdq_MEMq" ~mnemonic:"vcvtps2pd" rec_
  | Ok { iform = Some "VCVTPD2PS_XMMdq_XMMdq"; _ } ->
      vex_unop_rr_form ~form_id:"VCVTPD2PS_XMMdq_XMMdq" ~mnemonic:"vcvtpd2ps" rec_
  (* VSHUFPS/VSHUFPD ({!vex_binop_imm_rrr_form}'s own doc comment): only the plain
     [vex]-space 128-bit iforms - the [evex]-space AVX-512 masked iforms sharing the same
     native name are out of scope. Both are XED extension AVX. *)
  | Ok { iform = Some "VSHUFPS_XMMdq_XMMdq_XMMdq_IMMb"; _ } ->
      vex_binop_imm_rrr_form ~form_id:"VSHUFPS_XMMdq_XMMdq_XMMdq_IMMb" ~mnemonic:"vshufps" rec_
  | Ok { iform = Some "VSHUFPS_XMMdq_XMMdq_MEMdq_IMMb"; _ } ->
      vex_binop_imm_rr_mem_form ~form_id:"VSHUFPS_XMMdq_XMMdq_MEMdq_IMMb" ~mnemonic:"vshufps" rec_
  | Ok { iform = Some "VSHUFPD_XMMdq_XMMdq_XMMdq_IMMb"; _ } ->
      vex_binop_imm_rrr_form ~form_id:"VSHUFPD_XMMdq_XMMdq_XMMdq_IMMb" ~mnemonic:"vshufpd" rec_
  | Ok { iform = Some "VSHUFPD_XMMdq_XMMdq_MEMdq_IMMb"; _ } ->
      vex_binop_imm_rr_mem_form ~form_id:"VSHUFPD_XMMdq_XMMdq_MEMdq_IMMb" ~mnemonic:"vshufpd" rec_
  (* VCMPSS/VCMPSD/VCMPPS/VCMPPD ({!vex_binop_imm_rrr_form}'s own doc comment): the VEX
     sibling of the legacy CMPSS/CMPSD/CMPPS/CMPPD family, opcode 0xC2. Only the plain
     [vex]-space 128-bit iforms - the [evex]-space AVX-512 masked iforms sharing the same
     native name are out of scope. All four are XED extension AVX. Note VCMPSS/VCMPSD's own
     third-operand [oc2] is scalar ([XMMd]/[MEMd], [XMMq]/[MEMq]) rather than VSHUFPS's packed
     [XMMdq]/[MEMdq] - {!vex_binop_imm_rrr_form}/{!vex_binop_imm_rr_mem_form} only read the
     operand's [op_name]/[rw] facts, not its [oc2] width, so this needs no normalizer change. *)
  | Ok { iform = Some "VCMPSS_XMMdq_XMMdq_XMMd_IMMb"; _ } ->
      vex_binop_imm_rrr_form ~form_id:"VCMPSS_XMMdq_XMMdq_XMMd_IMMb" ~mnemonic:"vcmpss" rec_
  | Ok { iform = Some "VCMPSS_XMMdq_XMMdq_MEMd_IMMb"; _ } ->
      vex_binop_imm_rr_mem_form ~form_id:"VCMPSS_XMMdq_XMMdq_MEMd_IMMb" ~mnemonic:"vcmpss" rec_
  | Ok { iform = Some "VCMPSD_XMMdq_XMMdq_XMMq_IMMb"; _ } ->
      vex_binop_imm_rrr_form ~form_id:"VCMPSD_XMMdq_XMMdq_XMMq_IMMb" ~mnemonic:"vcmpsd" rec_
  | Ok { iform = Some "VCMPSD_XMMdq_XMMdq_MEMq_IMMb"; _ } ->
      vex_binop_imm_rr_mem_form ~form_id:"VCMPSD_XMMdq_XMMdq_MEMq_IMMb" ~mnemonic:"vcmpsd" rec_
  | Ok { iform = Some "VCMPPS_XMMdq_XMMdq_XMMdq_IMMb"; _ } ->
      vex_binop_imm_rrr_form ~form_id:"VCMPPS_XMMdq_XMMdq_XMMdq_IMMb" ~mnemonic:"vcmpps" rec_
  | Ok { iform = Some "VCMPPS_XMMdq_XMMdq_MEMdq_IMMb"; _ } ->
      vex_binop_imm_rr_mem_form ~form_id:"VCMPPS_XMMdq_XMMdq_MEMdq_IMMb" ~mnemonic:"vcmpps" rec_
  | Ok { iform = Some "VCMPPD_XMMdq_XMMdq_XMMdq_IMMb"; _ } ->
      vex_binop_imm_rrr_form ~form_id:"VCMPPD_XMMdq_XMMdq_XMMdq_IMMb" ~mnemonic:"vcmppd" rec_
  | Ok { iform = Some "VCMPPD_XMMdq_XMMdq_MEMdq_IMMb"; _ } ->
      vex_binop_imm_rr_mem_form ~form_id:"VCMPPD_XMMdq_XMMdq_MEMdq_IMMb" ~mnemonic:"vcmppd" rec_
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
            ADD/OR/ADC/SBB/AND/SUB/XOR/CMP memory/immb and memory/immz forms, the byte-width \
            ADD/OR/ADC/SBB/AND/SUB/XOR/CMP register/immb, memory/immb, and accumulator/immb forms, \
            the SSE2 ADDSD/SUBSD/MULSD/DIVSD register-register and register<-memory forms, the \
            plain xmm-xmm/xmm-memory binop shape's MULSS/DIVSS/COMISD/UCOMISD/COMISS/XORPD/PXOR/ \
            MOVAPD/CVTSD2SS/CVTSS2SD register-register and register<-memory forms, the MOVSD/MOVSS \
            load/store forms, the mixed-GPR/XMM CVTSI2SD/CVTSI2SS/CVTTSD2SI register-register and \
            register<-memory forms, the packed bitwise-logical ANDPS/ANDNPS/ORPS/XORPS/ \
            ANDPD/ANDNPD/ORPD register-register and register<-memory forms, the MOVAPS/ \
            MOVUPS/MOVUPD load-direction register-register and register<-memory forms, and the \
            packed-arithmetic ADDPS/SUBPS/MULPS/DIVPS/ADDPD/SUBPD/MULPD/DIVPD register-register \
            and register<-memory forms, and the VEX-encoded VADDSD/VSUBSD/VMULSD/VDIVSD/ \
            VADDSS/VSUBSS/VMULSS/VDIVSS/VADDPS/VSUBPS/VMULPS/VDIVPS/VADDPD/VSUBPD/VMULPD/VDIVPD \
            register-register and register<-memory forms, the XMM-immediate-carrying SHUFPS/SHUFPD \
            register-register and register<-memory forms, their non-EVEX VEX-space VSHUFPS/VSHUFPD \
            register-register and register<-memory forms, the XMM-immediate-carrying \
            CMPSS/CMPSD/CMPPS/CMPPD register-register and register<-memory forms, and their \
            non-EVEX VEX-space VCMPSS/VCMPSD/VCMPPS/VCMPPD register-register and register<-memory \
            forms; %s is not one of them"
           other)
  | Ok { iform = None; _ } -> err "missing-iform" "XED record has no provenance.iform"
  | Error msg -> err "not-a-xed-record" msg
