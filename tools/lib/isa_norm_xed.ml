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
let role_of_rw = function "r" -> In | "w" -> Out | _ -> In_out

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
  | Ok { iform = Some other; _ } ->
      err "unhandled-iform"
        (Printf.sprintf
           "Isa_norm_xed only normalizes the frozen pilot iforms plus the register/register and \
            register/immediate legacy ADD/MOV forms; %s is not one of them"
           other)
  | Ok { iform = None; _ } -> err "missing-iform" "XED record has no provenance.iform"
  | Error msg -> err "not-a-xed-record" msg
