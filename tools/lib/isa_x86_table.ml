module R = Isa_source_record
open Isa_norm_model

type rclass = Gpr8 | Gpr16 | Gpr32 | Gpr64 | Gprv | Xmm | Ymm | Zmm | Mmx | Kmask | St | Tmm
type field = Modrm_reg | Modrm_rm | Vvvv | Is4 | Opcode_low

type operand =
  | Reg of { cls : rclass; field : field }
  | Mem of { bits : int }
  | Imm of { bytes : int }
  | Fixed_reg of string
  | Dfv  (** APX CCMP/CTEST's default flags [{dfv=...}], in vvvv *)
  | One  (** the implied count 1 of a shift/rotate D0/D1 form, spelled [$1] *)
  | Rounding of { sae_only : bool }  (** EVEX embedded rounding [{rn-sae}], or [{sae}] *)
  | Vsib of { cls : rclass }  (** a VSIB address: its index a vector register of [cls] *)

type spec = {
  record_id : string;
  iform : string;
  isa_set : string;
  mnemonic : string;
  space : [ `Legacy | `Vex | `Evex | `Xop ];
  map : int;
  opcode : int;
  prefix : int;
  osz : bool;
  w : int;
  l : int;
  digit : int;
  operands : operand list;
  mode : int;
  evex_p2 : int;  (** APX map 4: ND (0x10) and NF (0x04) *)
  mask : int;  (** EVEX opmask: 0 none, 1 merge or zero, 2 merge only, 3 required *)
  rm : int;  (** a fixed ModR/M.rm of a register-form encoding with no rm operand, or -1 *)
  disp8n : int;  (** EVEX's disp8*N scale; 1 elsewhere *)
  sized : bool;  (** an integer form spelled with its operand-size suffix, added by {!expand} *)
  suffix_isa : string;
  pseudo : string;
  df64 : bool;  (** DF64(): 64-bit operand size by default in 64-bit mode, no REX.W *)
  no_acc : int list;  (** AT&T positions that must not be the accumulator *)
  widths : int list;  (** the operand sizes a width-variable (GPRv) form takes *)
}

let starts_with ~prefix s =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let ends_with ~suffix s =
  let n = String.length s and k = String.length suffix in
  n >= k && String.sub s (n - k) k = suffix

let after ~prefix s = String.sub s (String.length prefix) (String.length s - String.length prefix)

(* The pattern tokens this rule understands, with what each says. Any other
   token leaves the record to hand-written work: an unknown constraint must
   never be silently dropped. *)
type pattern = {
  vex : bool;
  evex : bool;
  xop : bool;  (** VEXVALID=3 *)
  esize : int;  (** ESIZE_n_BITS() *)
  nelem : string;  (** NELEM_x(), the EVEX tuple type *)
  vex_prefix : int option;
  vl : int;
  rexw : int;
  memory : bool option;  (** [Some true]: MOD!=3; [Some false]: MOD=3 *)
  digit : int;
  mode64 : bool;
  modrm : bool;
  osz : int;  (** OSZ=: -1 unconstrained, 0, 1 *)
  rep : int;  (** REP=: -1 unconstrained, 0, 2 (F2), 3 (F3) *)
  refining66 : bool;  (** the 0x66 is a mandatory prefix, not an operand size *)
  not64 : bool;  (** MODE!=2: the form does not exist in 64-bit mode *)
  free_reg : bool;  (** REG[rrr] with no register operand behind it: GNU encodes 0 *)
  srm_nonzero : bool;  (** SRM!=0: the opcode-embedded register is not the accumulator *)
  bcrc : bool;  (** BCRC=1: EVEX.b set *)
  nozero : bool;  (** ZEROING=0: no {z} *)
  rm : int;  (** RM[0bxxx]: a fixed ModR/M.rm, or -1 *)
  nd : bool;  (** APX ND=1: a new data destination in vvvv *)
  nf : bool;  (** APX NF=1: flags untouched *)
  df64 : bool;  (** DF64() *)
  lock : bool;  (** LOCK=1: the F0 prefix *)
  scc : int;  (** APX CCMP/CTEST (EVAPX_SCC()): the condition, in P2's low nibble; else -1 *)
  vsib : rclass option;  (** VMODRM_XMM() and kin: the memory operand's index class *)
  round : [ `None | `Rc | `Sae ];  (** AVX512_ROUND() / SAE(): what EVEX.b means here *)
}

let parse_pattern pattern =
  let tokens = String.split_on_char ' ' pattern |> List.filter (fun t -> t <> "") in
  let binary s = int_of_string_opt ("0b" ^ s) in
  List.fold_left
    (fun acc t ->
      match acc with
      | None -> None
      | Some p -> (
          if starts_with ~prefix:"0x" t || starts_with ~prefix:"0b" t then Some p
          else
            match t with
            | "VEXVALID=1" -> Some { p with vex = true }
            | "VEXVALID=2" -> Some { p with evex = true }
            | "VEXVALID=3" -> Some { p with xop = true }
            | "VL=2" -> Some { p with vl = 2 }
            (* EVEX's defaults: U = 1, no zeroing, no broadcast or rounding, no mask *)
            | "ZEROING=0" -> Some { p with nozero = true }
            | "UBIT=1" | "BCRC=0" | "MASK=0" | "VEXDEST4=0b0" -> Some p
            | "BCRC=1" -> Some { p with bcrc = true }
            (* MASK=4: NF in EVEX.aaa, which NF=1 already states *)
            | "EVAPX()" | "ND=0" | "NF=0" | "MASK=4" | "ONE()" -> Some p
            | "DF64()" -> Some { p with df64 = true }
            | "LOCK=1" -> Some { p with lock = true }
            (* CCMP/CTEST: SCC= is the condition; its NF= and MASK= bits restate it *)
            | "EVAPX_SCC()" -> Some { p with scc = max 0 p.scc }
            | "MASK=1" | "MASK=2" | "MASK=3" | "MASK=5" | "MASK=6" | "MASK=7" -> Some p
            | _ when starts_with ~prefix:"SCC=" t ->
                Option.map
                  (fun n -> { p with scc = n })
                  (int_of_string_opt (after ~prefix:"SCC=" t))
            | "ND=1" -> Some { p with nd = true }
            | "NF=1" -> Some { p with nf = true }
            | "VMODRM_XMM()" | "UISA_VMODRM_XMM()" -> Some { p with vsib = Some Xmm }
            | "VMODRM_YMM()" | "UISA_VMODRM_YMM()" -> Some { p with vsib = Some Ymm }
            | "UISA_VMODRM_ZMM()" -> Some { p with vsib = Some Zmm }
            (* VSIB's SIB escape; any address size but 16-bit *)
            | "RM=4" | "EASZ!=1" -> Some p
            | _ when starts_with ~prefix:"RM[0b" t && String.length t = 9 ->
                Option.map (fun r -> { p with rm = r; modrm = true }) (binary (String.sub t 5 3))
            | "AVX512_ROUND()" -> Some { p with round = `Rc }
            | "SAE()" -> Some { p with round = `Sae }
            | "FIX_ROUND_LEN512()" -> Some { p with vl = 2 }
            (* EVEX.R' = 1 for a GPR in ModR/M.reg: the encoder's constant *)
            | "EVEXR4_ONE()" -> Some p
            (* a scalar form's length field is ignored and encoded as 128 bits *)
            | "FIX_ROUND_LEN128()" -> Some { p with vl = 0 }
            | _ when starts_with ~prefix:"ESIZE_" t && ends_with ~suffix:"_BITS()" t ->
                Option.map
                  (fun e -> { p with esize = e })
                  (int_of_string_opt (String.sub t 6 (String.length t - 13)))
            | _ when starts_with ~prefix:"NELEM_" t && ends_with ~suffix:"()" t ->
                Some { p with nelem = String.sub t 6 (String.length t - 8) }
            | "MOD[mm]" | "RM[nnn]" | "REG[rrr]" | "SKIP_OSZ=1" | "MODRM()" | "SE_IMM8()"
            | "UIMM8()" | "VEXDEST3=0b1" | "VEXDEST210=0b111" ->
                Some
                  {
                    p with
                    modrm = p.modrm || t = "MOD[mm]" || t = "RM[nnn]" || t = "REG[rrr]";
                    free_reg = p.free_reg || t = "REG[rrr]";
                  }
            | "MOD!=3" -> Some { p with memory = Some true; modrm = true }
            | "MOD=3" | "MOD[0b11]" -> Some { p with memory = Some false; modrm = true }
            | "MODE=2" -> Some { p with mode64 = true }
            | "MODE!=2" -> Some { p with not64 = true }
            (* 32-bit mode only; MODE=0 (16-bit mode) is DEC-X86-MODE16's *)
            | "MODE=1" -> Some { p with not64 = true }
            | "SRM!=0" -> Some { p with srm_nonzero = true }
            | "VL=0" -> Some { p with vl = 0 }
            | "VL=1" -> Some { p with vl = 1 }
            | "REXW=0" -> Some { p with rexw = 0 }
            | "REXW=1" -> Some { p with rexw = 1 }
            | "OSZ=0" -> Some { p with osz = 0 }
            | "OSZ=1" -> Some { p with osz = 1 }
            | "REP=0" -> Some { p with rep = 0 }
            | "REP=2" -> Some { p with rep = 2 }
            | "REP=3" -> Some { p with rep = 3 }
            (* no constraint on the encoding: a 0x66 is tolerated, REX2 is APX's *)
            | "REFINING66()" -> Some { p with refining66 = true }
            (* TZCNT=1/LZCNT=1 select the F3 form from bsf/bsr, which REP=3 already states;
               REP!=3 is the absence of that prefix *)
            | "TZCNT=1" | "LZCNT=1" | "REP!=3" -> Some p
            (* a string op's segment override is its default without a prefix *)
            | "OVERRIDE_SEG0()" | "OVERRIDE_SEG1()" -> Some p
            | "IGNORE66()" | "NOREX2=1" | "REX2=0" | "SIMM8()" | "SRM[rrr]" | "LOCK=0"
            | "IMMUNE66()" | "SIMMz()" | "UIMM16()" | "UIMM32()" ->
                Some p
            | _ when starts_with ~prefix:"VEX_PREFIX=" t ->
                Option.map
                  (fun v -> { p with vex_prefix = Some v })
                  (int_of_string_opt (after ~prefix:"VEX_PREFIX=" t))
            | _ when starts_with ~prefix:"MAP=" t -> Some p
            | _ when starts_with ~prefix:"REG[0b" t && String.length t = 10 ->
                Option.map (fun d -> { p with digit = d; modrm = true }) (binary (String.sub t 6 3))
            | _ -> None))
    (Some
       {
         vex = false;
         evex = false;
         xop = false;
         esize = 0;
         nelem = "";
         vex_prefix = None;
         vl = -1;
         rexw = -1;
         memory = None;
         digit = -1;
         mode64 = false;
         modrm = false;
         osz = -1;
         rep = -1;
         refining66 = false;
         not64 = false;
         free_reg = false;
         srm_nonzero = false;
         bcrc = false;
         nozero = false;
         rm = -1;
         nd = false;
         nf = false;
         scc = -1;
         df64 = false;
         lock = false;
         vsib = None;
         round = `None;
       })
    tokens

let class_of_lookup lookup =
  let classes =
    [
      ("ZMM_", (Zmm : rclass));
      ("XMM_", Xmm);
      ("YMM_", Ymm);
      (* y: 32 or 64 bits by VEX.W, expanded like GPRv *)
      ("VGPRy_", Gprv);
      ("VGPR32_", Gpr32);
      ("VGPR64_", Gpr64);
      ("GPR32_", Gpr32);
      ("GPR64_", Gpr64);
      ("GPRv_", Gprv);
      ("GPR8_", Gpr8);
      ("GPR16_", Gpr16);
      ("MMX_", Mmx);
      ("MASK_", Kmask);
      ("TMM_", Tmm);
    ]
  in
  match List.find_opt (fun (p, _) -> starts_with ~prefix:p lookup) classes with
  | None -> None
  | Some (p, cls) -> (
      (* the 3 suffix is EVEX's 32-register range of the same field *)
      let f = after ~prefix:p lookup in
      let f = if ends_with ~suffix:"3" f then String.sub f 0 (String.length f - 1) else f in
      match f with
      | "R" -> Some (cls, Modrm_reg)
      | "B" -> Some (cls, Modrm_rm)
      | "N" -> Some (cls, Vvvv)
      | "SE" -> Some (cls, Is4)
      | "SB" -> Some (cls, Opcode_low)
      | _ -> None)

let mem_bits = function
  | Some "b" -> 8
  | Some "w" -> 16
  | Some "d" -> 32
  | Some "q" -> 64
  | Some "dq" -> 128
  | Some "qq" -> 256
  | _ -> 0

(* XED lists the destination first; AT&T lists it last. A width-variable memory operand ([v])
   has bits = -1 and a [z] immediate bytes = 0 until {!expand} fixes the operand size. *)
let operand_of (o : R.x86_operand) =
  if o.visibility = "SUPPRESSED" then Some None
    (* the broadcast element marker of a VEX broadcast: not an operand *)
  else if o.op_name = "BCAST" && o.lookupfn_name = None then Some None
    (* EVEX's optional write mask: absent means k0, the unmasked form this rule admits *)
  else if o.lookupfn_name = Some "MASK1" || o.lookupfn_name = Some "MASKNOT0" then Some None
  else if o.visibility = "IMPLICIT" then
    match (o.op_type, o.bits) with
    | "imm_const", Some "1" when starts_with ~prefix:"IMM0" o.op_name -> Some (Some One)
    | "nt_lookup_fn", _ when o.lookupfn_name = Some "OrAX" -> Some (Some (Fixed_reg "?ax"))
    | ( "reg",
        Some
          (( "XED_REG_CL" | "XED_REG_AL" | "XED_REG_AX" | "XED_REG_EAX" | "XED_REG_RAX"
           | "XED_REG_DX" ) as r) ) ->
        Some (Some (Fixed_reg (String.lowercase_ascii (after ~prefix:"XED_REG_" r))))
    | _ -> None
  else if o.visibility <> "DEFAULT" then None
  else
    match (o.op_type, o.lookupfn_name) with
    | "imm_const", _ when starts_with ~prefix:"MEM0" o.op_name && o.oc2 = Some "v" ->
        Some (Some (Mem { bits = -1 }))
    | "imm_const", _ when starts_with ~prefix:"IMM0" o.op_name && o.oc2 = Some "z" ->
        Some (Some (Imm { bytes = 0 }))
    | "imm_const", _ when starts_with ~prefix:"IMM0" o.op_name && o.oc2 = Some "w" ->
        Some (Some (Imm { bytes = 2 }))
    | "imm_const", _ when starts_with ~prefix:"IMM0" o.op_name && o.oc2 = Some "d" ->
        Some (Some (Imm { bytes = 4 }))
    | "imm_const", _ when starts_with ~prefix:"MEM0" o.op_name && o.oc2 = Some "y" ->
        Some (Some (Mem { bits = -1 }))
    | "nt_lookup_fn", Some lookup ->
        Option.map (fun (cls, field) -> Some (Reg { cls; field })) (class_of_lookup lookup)
    | "imm_const", _ when starts_with ~prefix:"MEM0" o.op_name ->
        Some (Some (Mem { bits = mem_bits o.oc2 }))
    | "imm_const", _ when starts_with ~prefix:"IMM0" o.op_name && o.oc2 = Some "b" ->
        Some (Some (Imm { bytes = 1 }))
    | _ -> None

(* VEX forms whose AT&T spelling GNU as resolves to the EVEX twin by default: the VEX encoding
   takes a {vex} pseudo-prefix. *)
let evex_preferred = [ "AVX_VNNI"; "AVX_IFMA"; "AVX_NE_CONVERT" ]

(* The AT&T spelling of an iclass: lower case, XED's 64-bit-operand string
   compares spelled with GNU's [q] suffix, and the narrowing conversions whose
   memory source width AT&T states with an [x]/[y] suffix. *)
(* A conversion whose destination elements are narrower than its source's: from memory into an
   xmm register, AT&T states the source width with an x (128), y (256) or z (512) suffix. *)
let narrowing iclass =
  let size t =
    List.assoc_opt t
      [
        ("PD", 64);
        ("QQ", 64);
        ("UQQ", 64);
        ("PS", 32);
        ("DQ", 32);
        ("UDQ", 32);
        ("PH", 16);
        ("PHX", 16);
        ("BF16", 16);
        ("W", 16);
        ("BF8", 8);
        ("HF8", 8);
        ("BF8S", 8);
        ("HF8S", 8);
        ("DQS", 32);
        ("UDQS", 32);
      ]
  in
  starts_with ~prefix:"VCVT" iclass
  &&
  match String.index_opt iclass '2' with
  | None -> false
  | Some i -> (
      let left = String.sub iclass 0 i
      and right = String.sub iclass (i + 1) (String.length iclass - i - 1) in
      let src =
        List.find_map
          (fun t -> if ends_with ~suffix:t left then size t else None)
          [ "UQQ"; "QQ"; "PD"; "PS"; "UDQ"; "DQ"; "PH"; "W" ]
      in
      match (src, size right) with Some a, Some b -> a > b | _ -> false)

let mem_to_xmm operands =
  match List.filter (function Imm _ -> false | _ -> true) operands with
  | [ Mem _; Reg { cls = Xmm; _ } ] -> true
  | _ -> false

let att_mnemonic ?(vl = -1) ~iclass operands =
  let lower = String.lowercase_ascii iclass in
  match iclass with
  | "VPCMPESTRI64" | "VPCMPESTRM64" | "PCMPESTRI64" | "PCMPESTRM64" ->
      String.sub lower 0 (String.length lower - 2) ^ "q"
  (* GNU as has no q spelling for these: they are twins of the W-ignored forms *)
  | "VPCMPISTRI64" | "VPCMPISTRM64" | "PCMPISTRI64" | "PCMPISTRM64" ->
      String.sub lower 0 (String.length lower - 2)
  (* an integer source in memory states its width: vcvtsi2sdl / vcvtsi2sdq *)
  | ("VCVTSI2SD" | "VCVTSI2SS" | "VCVTUSI2SD" | "VCVTUSI2SS" | "VCVTSI2SH" | "VCVTUSI2SH")
    when List.exists (function Mem { bits = 32 | 64 } -> true | _ -> false) operands ->
      lower
      ^
      if List.exists (function Mem { bits = 64 } -> true | _ -> false) operands then "q" else "l"
  | _ when narrowing iclass && vl >= 0 && mem_to_xmm operands -> (
      lower ^ match vl with 0 -> "x" | 1 -> "y" | _ -> "z")
  (* a class test of memory into a mask register states the vector width the same way *)
  | _
    when (starts_with ~prefix:"VFPCLASSP" iclass || starts_with ~prefix:"VFPCLASSBF16" iclass)
         && vl >= 0
         && List.exists (function Mem _ -> true | _ -> false) operands -> (
      lower ^ match vl with 0 -> "x" | 1 -> "y" | _ -> "z")
  (* XED disambiguates a few iclasses with a suffix GNU does not spell: MOVSD_XMM, PEXTRW_SSE4 *)
  | _ when String.contains lower '_' -> String.sub lower 0 (String.index lower '_')
  | _ -> lower

(* A form the x86-32 export lists that 32-bit mode cannot encode: a legacy REX.W, a 64-bit GPR,
   or a 64-bit-mode-only pattern. GNU as rejects each ("bad register name %rax"). *)
let iclass_of_iform iform =
  match String.index_opt iform '_' with Some i -> String.sub iform 0 i | None -> iform

let inherit_suffix_rule (records : R.t list) specs =
  let legacy = Hashtbl.create 256 in
  List.iter
    (fun (r : R.t) ->
      match (r.encoding, r.provenance) with
      | R.X86_encoding { space = "legacy"; _ }, R.Xed_provenance { isa_set = Some isa; _ }
        when not (starts_with ~prefix:"APX" isa) ->
          Hashtbl.replace legacy r.native_name isa
      | _ -> ())
    records;
  List.map
    (fun s ->
      (* a ZU form is its own spelling, with no suffix *)
      if s.space = `Evex && s.map = 4 && not (ends_with ~suffix:"_ZU" s.iform) then
        match Hashtbl.find_opt legacy (iclass_of_iform s.iform) with
        | Some isa ->
            {
              s with
              suffix_isa = isa;
              (* GNU encodes the plain spelling as the legacy instruction *)
              pseudo = (if s.pseudo = "" && s.evex_p2 = 0 then "evex" else s.pseudo);
            }
        | None -> s
      else s)
    specs

let not_in_32bit_mode (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; pattern; operands; _ } ->
      let tokens = String.split_on_char ' ' pattern in
      List.mem "MODE=2" tokens
      || (space = "legacy" && List.mem "REXW=1" tokens)
      || List.exists
           (fun (o : R.x86_operand) ->
             match o.lookupfn_name with
             | Some l -> starts_with ~prefix:"GPR64_" l || starts_with ~prefix:"VGPR64_" l
             | None -> false)
           operands
  | _ -> false

(* The integer shapes the rule spells: one operand size throughout (so a single suffix names
   it) and no mixed-width moves. *)
(* The AT&T spelling of an integer iclass. XED's string-op dword forms end in D where AT&T says
   l (GNU as warns "assuming movsl" and would read movsd as SSE); the 16-bit iret/pushf/popf
   take w; far return and sysret state their operand size; a REP/REPE/REPNE iclass is the
   prefix, a space and the string op. *)
let integer_mnemonic ~rep native =
  let op n =
    match n with
    | "MOVSD" | "CMPSD" | "STOSD" | "LODSD" | "SCASD" | "INSD" | "OUTSD" ->
        String.lowercase_ascii (String.sub n 0 (String.length n - 1)) ^ "l"
    | "IRET" | "PUSHF" | "POPF" -> String.lowercase_ascii n ^ "w"
    | "PUSHA" | "POPA" -> String.lowercase_ascii n ^ "w"
    | "PUSHAD" | "POPAD" -> String.lowercase_ascii (String.sub n 0 (String.length n - 1)) ^ "l"
    | "IRETD" | "PUSHFD" | "POPFD" ->
        String.lowercase_ascii (String.sub n 0 (String.length n - 1)) ^ "l"
    | "RET_FAR" -> "lretl"
    | "SYSRET" | "SYSRET_AMD" -> "sysretl"
    | "SYSRET64" -> "sysretq"
    | "SYSCALL_AMD" -> "syscall"
    (* the no-wait x87 forms: fsetpm/fdisi/feni would add an FWAIT *)
    | "PREFETCH_EXCLUSIVE" -> "prefetch"
    | "FSETPM287_NOP" -> "fnsetpm"
    | "FDISI8087_NOP" -> "fndisi"
    | "FENI8087_NOP" -> "fneni"
    | _ -> String.lowercase_ascii n
  in
  match String.index_opt native '_' with
  | Some i when List.mem (String.sub native 0 i) [ "REP"; "REPE"; "REPNE" ] ->
      (* XED also lists an F2-prefixed REP_MOVS/STOS/LODS/INS/OUTS: GNU spells it repne *)
      (if String.sub native 0 i = "REP" && rep = 2 then "repne"
       else String.lowercase_ascii (String.sub native 0 i))
      ^ " "
      ^ op (String.sub native (i + 1) (String.length native - i - 1))
  (* XED's BTC_LOCK: the lock prefix, a space and the instruction *)
  | _ when ends_with ~suffix:"_LOCK" native ->
      "lock " ^ op (String.sub native 0 (String.length native - 5))
  | _ -> op native

(* movzbl, movswq: a zero/sign extension spells its source width in its stem and its
   destination width as the suffix *)
let movx iclass = iclass = "MOVZX" || iclass = "MOVSX"

let gpr_ok operands ~iclass =
  let classes =
    List.sort_uniq compare
      (List.filter_map
         (function
           | Reg { cls = (Gpr8 | Gpr16 | Gpr32 | Gpr64 | Gprv) as c; _ } -> Some c
           | Mem { bits = -1 } | Fixed_reg "?ax" -> Some Gprv
           | Mem { bits = 8 } -> Some Gpr8
           | Mem { bits = 16 } -> Some Gpr16
           | Mem { bits = 32 } -> Some Gpr32
           | Mem { bits = 64 } -> Some Gpr64
           | Fixed_reg "al" -> Some Gpr8
           | _ -> None)
         operands)
  in
  (List.length classes = 1
  || operands = [] || movx iclass
  || List.for_all (function Mem _ -> true | _ -> false) operands
     (* far transfers take a *-marked memory operand; the reserved prefetch hints have no
        GNU spelling *)
     && (not (List.mem iclass [ "CALL_FAR"; "JMP_FAR" ]))
     && not (starts_with ~prefix:"PREFETCH_RESERVED" iclass))
  (* no 16-to-16 movzww/movsww *)
  && (not (movx iclass && classes = [ Gpr16 ]))
  && (not (List.mem iclass [ "MOVSXD"; "BSWAP" ]))
  (* GNU as keeps bound's Intel operand order in AT&T syntax *)
  && iclass <> "BOUND"
  (* the reserved-NOP register pairs have no GNU spelling: nop takes one operand *)
  && (not (iclass = "NOP" && List.length operands > 1))
  && List.for_all (function Reg { field = Vvvv | Is4; _ } -> false | _ -> true) operands

(* The concrete rows of a spec: a width-variable integer form becomes its 16-, 32- and 64-bit
   rows (0x66 / none / REX.W; a [z] immediate is 2 or 4 bytes), each spelled with its size
   suffix. Any other spec is itself. *)
(* The ISA sets whose integer mnemonics GNU as spells with a size suffix *)
let classic_isa isa_set =
  List.mem isa_set [ "PENTIUMREAL"; "PPRO"; "LONGMODE"; "I486REAL"; "PENTIUMMMX" ]
  || (String.length isa_set > 1 && isa_set.[0] = 'I' && isa_set.[1] >= '0' && isa_set.[1] <= '9')

let expand spec =
  if not spec.sized then [ spec ]
  else
    let variable =
      List.exists
        (function
          | Reg { cls = Gprv; _ } | Mem { bits = -1 } | Imm { bytes = 0 } -> true | _ -> false)
        spec.operands
    in
    let classic = classic_isa spec.suffix_isa in
    (* GNU as rejects a size suffix on most newer integer instructions: a register operand or
       the instruction itself states the size there. invlpg's byte operand is an address, and
       invlpgb is another instruction. *)
    let has_gpr_reg =
      List.exists
        (function Reg { cls = Gpr8 | Gpr16 | Gpr32 | Gpr64 | Gprv; _ } -> true | _ -> false)
        spec.operands
    in
    let has_imm = List.exists (function Imm _ -> true | _ -> false) spec.operands in
    let suffix w =
      if
        ((not classic) && (has_gpr_reg || ((not variable) && not has_imm)))
        || spec.iform = "INVLPG_MEMb"
      then ""
      else match w with 8 -> "b" | 16 -> "w" | 32 -> "l" | _ -> "q"
    in
    (* a mixed-width move's suffix is its destination's width, AT&T's last operand *)
    let sized_by =
      if
        ends_with ~suffix:"MEMw" spec.iform
        || String.length spec.mnemonic = 5
           && (starts_with ~prefix:"movz" spec.mnemonic || starts_with ~prefix:"movs" spec.mnemonic)
      then List.rev spec.operands
      else spec.operands
    in
    if not variable then
      let width =
        List.find_map
          (function
            | Reg { cls = Gpr8; _ } -> Some 8
            | Reg { cls = Gpr16; _ } -> Some 16
            | Reg { cls = Gpr32; _ } -> Some 32
            | Reg { cls = Gpr64; _ } -> Some 64
            | Mem { bits } when bits > 0 -> Some bits
            | Fixed_reg "al" -> Some 8
            | _ -> None)
          sized_by
      in
      [
        {
          spec with
          mnemonic = spec.mnemonic ^ Option.fold ~none:"" ~some:suffix width;
          sized = false;
        };
      ]
    else
      List.map
        (fun width ->
          let operands =
            List.map
              (function
                | Reg { cls = Gprv; field } ->
                    Reg { cls = (match width with 16 -> Gpr16 | 32 -> Gpr32 | _ -> Gpr64); field }
                | Mem { bits = -1 } -> Mem { bits = width }
                | Imm { bytes = 0 } -> Imm { bytes = (if width = 16 then 2 else 4) }
                | Fixed_reg "?ax" ->
                    Fixed_reg (match width with 16 -> "ax" | 32 -> "eax" | _ -> "rax")
                | o -> o)
              spec.operands
          in
          {
            spec with
            mnemonic = spec.mnemonic ^ suffix width;
            operands;
            osz = width = 16 && spec.space = `Legacy;
            (* VEX.W is the operand size of a y-width form *)
            w =
              (if width = 64 then if spec.df64 then -1 else 1
               else if spec.space <> `Legacy then 0
               else spec.w);
            (* a DF64 form is 64-bit in 64-bit mode, so its 32-bit row is 32-bit mode's *)
            mode = (if width = 64 then 64 else if spec.df64 && width = 32 then 32 else spec.mode);
            sized = false;
          })
        (List.filter (fun w -> w <> 64 || spec.w <> 0) spec.widths)

(* An opcode written as five bits and a register ([0b0100_0 SRM[rrr]], the short inc) is
   exported as those five bits; the row wants the byte with the register bits clear. *)
let opcode_byte ~pattern opcode =
  match String.split_on_char ' ' pattern |> List.filter (fun t -> t <> "") with
  | t :: _ when starts_with ~prefix:"0b" t ->
      let bits = String.concat "" (String.split_on_char '_' (after ~prefix:"0b" t)) in
      if String.length bits = 5 then opcode lsl 3 else opcode
  | _ -> opcode

(* EVEX's disp8*N scale from the tuple type (Intel SDM, "Compressed Displacement"), for the
   non-broadcast memory form: FULL/FULLMEM the vector, HALF(MEM) half of it, QUARTER(MEM) and
   EIGHTHMEM a quarter and an eighth, ONE/TUPLE1 one element, TUPLEn n elements, MEM128 16 bytes,
   MOVDDUP 8 bytes at 128 bits and the vector above. *)
let disp8_scale p =
  let vector = 16 lsl max 0 p.vl and element = p.esize / 8 in
  match p.nelem with
  | "FULL" | "FULLMEM" -> vector
  | "HALF" | "HALFMEM" -> vector / 2
  | "QUARTER" | "QUARTERMEM" -> vector / 4
  | "EIGHTHMEM" -> vector / 8
  | "ONE" | "SCALAR" -> max 1 element
  | "TUPLE2" -> 2 * element
  | "TUPLE4" -> 4 * element
  | "TUPLE8" -> 8 * element
  | "MEM128" | "TUPLE1_4X" -> 16
  | "MOVDDUP" -> if p.vl = 0 then 8 else vector
  | _ -> 1

(* x87 (DEC-X86-TABLE): a stack register st(i) in ModR/M.rm, the implied %st spelled only
   beside another stack register of an arithmetic or conditional-move form, and a memory
   operand whose XED type gives GNU's suffix: s/l/t for 32/64/80-bit reals, s/l/ll for
   16/32/64-bit integers. The 14- and 94-byte (16-bit) environment images are left out. *)
let x87_spec (rec_ : R.t) ~iform ~isa_set ~opcode_map ~opcode ~pattern ~operands =
  let unspelled_st0 =
    [
      "FLD"; "FST"; "FSTP"; "FXCH"; "FCOM"; "FCOMP"; "FUCOM"; "FUCOMP"; "FFREE"; "FFREEP"; "FSTPNCE";
    ]
  in
  let has_x87 = List.exists (fun (o : R.x86_operand) -> o.lookupfn_name = Some "X87") operands in
  let suffix = ref "" and ok = ref true in
  let convert (o : R.x86_operand) =
    if o.visibility = "SUPPRESSED" then None
    else if o.lookupfn_name = Some "X87" then Some (Reg { cls = St; field = Modrm_rm })
    else if o.visibility = "IMPLICIT" then (
      match o.bits with
      | Some "XED_REG_ST0" when has_x87 && not (List.mem rec_.native_name unspelled_st0) ->
          Some (Fixed_reg "st")
      | Some "XED_REG_ST0" -> None
      | Some "XED_REG_AX" -> Some (Fixed_reg "ax")
      | _ ->
          ok := false;
          None)
    else if starts_with ~prefix:"MEM0" o.op_name then (
      let integer = starts_with ~prefix:"FI" rec_.native_name in
      (match o.oc2 with
      | Some "mem32real" -> suffix := "s"
      | Some "m64real" -> suffix := "l"
      | Some "mem80real" -> suffix := "t"
      | Some "mem16int" when integer -> suffix := "s"
      | Some "mem32int" when integer -> suffix := "l"
      | Some "m64int" when integer -> suffix := "ll"
      | Some ("mem80dec" | "mem16" | "mem28" | "mem108") -> ()
      | _ -> ok := false);
      Some (Mem { bits = 0 }))
    else (
      ok := false;
      None)
  in
  let xed_order = List.filter_map convert operands in
  match (parse_pattern pattern, int_of_string_opt opcode) with
  | Some p, Some opcode when !ok && opcode_map = 0 && p.rexw <> 1 && p.osz <> 1 ->
      let has_mem = List.exists (function Mem _ -> true | _ -> false) xed_order in
      if p.memory = Some true <> has_mem then None
      else
        Some
          {
            record_id = rec_.record_id;
            iform;
            isa_set;
            suffix_isa = isa_set;
            pseudo = "";
            df64 = false;
            mnemonic =
              (* GNU's AT&T x87 quirk: with %st(i) the destination, fsub and fsubr (fdiv and
                 fdivr) trade spellings *)
              (let m = integer_mnemonic ~rep:(-1) rec_.native_name in
               if
                 ends_with ~suffix:"_X87_ST0" iform
                 && (starts_with ~prefix:"fsub" m || starts_with ~prefix:"fdiv" m)
               then
                 let stem = String.sub m 0 4 and rest = String.sub m 4 (String.length m - 4) in
                 if starts_with ~prefix:"r" rest then stem ^ after ~prefix:"r" rest
                 else stem ^ "r" ^ rest
               else m)
              ^ !suffix;
            space = `Legacy;
            map = 0;
            opcode;
            prefix = 0;
            osz = false;
            w = -1;
            l = -1;
            digit = p.digit;
            rm = p.rm;
            operands = List.rev xed_order;
            mode = (if p.mode64 then 64 else if p.not64 then 32 else 0);
            disp8n = 1;
            evex_p2 = 0;
            mask = 0;
            sized = false;
            no_acc = [];
            widths = [];
          }
  | _ -> None

let spec_of_record (rec_ : R.t) =
  match (rec_.encoding, rec_.provenance) with
  | ( R.X86_encoding
        { space = ("vex" | "evex" | "xop") as space; opcode_map; opcode; pattern; operands },
      R.Xed_provenance { iform = Some iform; isa_set = Some isa_set; _ } ) -> (
      match (parse_pattern pattern, int_of_string_opt opcode) with
      | Some p, Some opcode
        when (match space with "vex" -> p.vex | "evex" -> p.evex | _ -> p.xop)
             && (p.modrm
                || List.for_all (fun (o : R.x86_operand) -> o.visibility <> "DEFAULT") operands)
        -> (
          let ops = List.map operand_of operands in
          if List.mem None ops then None
          else
            let xed_order = List.filter_map Fun.id (List.filter_map Fun.id ops) in
            let has_mem = List.exists (function Mem _ -> true | _ -> false) xed_order in
            let xed_order =
              match p.vsib with
              | Some cls -> List.map (function Mem _ -> Vsib { cls } | o -> o) xed_order
              | None -> xed_order
            in
            let prefix =
              match p.vex_prefix with
              | Some 1 -> Some 0x66
              | Some 2 -> Some 0xf2
              | Some 3 -> Some 0xf3
              | Some 0 -> Some 0
              | _ -> None
            in
            (* EVEX.b on a register form is embedded rounding: AT&T writes it after any
               immediate, before the registers - but after the GPR source of an integer-to-float
               conversion (GNU as: "misplaced {rn-sae}" before it) *)
            let att =
              let ops = List.rev xed_order in
              (* CCMP/CTEST's {dfv=} comes first *)
              let ops = if p.scc >= 0 then Dfv :: ops else ops in
              match p.round with
              | `None -> ops
              | (`Rc | `Sae) as r -> (
                  let rc = Rounding { sae_only = r = `Sae } in
                  let imms, rest = List.partition (function Imm _ -> true | _ -> false) ops in
                  match rest with
                  | (Reg { cls = Gpr32 | Gpr64; _ } as gpr) :: tail -> imms @ (gpr :: rc :: tail)
                  | _ -> imms @ (rc :: rest))
            in
            let variable =
              List.exists
                (function Reg { cls = Gprv; _ } | Mem { bits = -1 } -> true | _ -> false)
                xed_order
            in
            let apx = space = "evex" && opcode_map = 4 in
            (* REG[rrr] with no register behind it: GNU encodes 0 *)
            let digit =
              if
                p.digit < 0 && p.free_reg
                && not
                     (List.exists
                        (function Reg { field = Modrm_reg; _ } -> true | _ -> false)
                        att)
              then 0
              else p.digit
            in
            match prefix with
            | None -> None
            | Some _ when p.memory = Some true <> has_mem -> None
            (* EVEX.b means rounding only on a register form, and only where the pattern says so *)
            | Some _ when p.bcrc && (p.round = `None || has_mem) -> None
            | Some _ when (not p.bcrc) && p.round <> `None -> None
            (* the ZU (zero-upper) forms GNU spells setzu<cc> (register only, no {nf}) and imulzu (16-bit only:
               the wider ones gain nothing, and GNU rejects them) *)
            | Some prefix
              when apx && ends_with ~suffix:"_ZU" iform
                   && not
                        ((starts_with ~prefix:"SET" rec_.native_name && (not p.nf) && not has_mem)
                        || (rec_.native_name = "IMUL" && prefix = 0x66)) ->
                None
            | Some prefix ->
                Some
                  {
                    record_id = rec_.record_id;
                    iform;
                    isa_set;
                    (* an APX promotion's suffix rule and {evex} need are its legacy
                       instruction's, filled in by inherit_suffix_rule *)
                    suffix_isa = (if apx then "" else isa_set);
                    (* CFCMOV's NF bit selects its store form, spelled plainly *)
                    df64 = p.df64;
                    pseudo =
                      (if p.nf && p.scc < 0 && not (starts_with ~prefix:"CFCMOV" rec_.native_name)
                       then "nf"
                       else "");
                    mnemonic =
                      (if apx && ends_with ~suffix:"_ZU" iform then
                         if rec_.native_name = "IMUL" then "imulzu"
                         else
                           "setzu" ^ String.lowercase_ascii (after ~prefix:"SET" rec_.native_name)
                       else att_mnemonic ~vl:p.vl ~iclass:rec_.native_name (List.rev xed_order));
                    space = (match space with "vex" -> `Vex | "evex" -> `Evex | _ -> `Xop);
                    map = opcode_map;
                    opcode;
                    prefix;
                    osz = false;
                    w = p.rexw;
                    (* a rounding mode takes L'L's place *)
                    l = (if p.bcrc then -1 else p.vl);
                    digit;
                    rm = (if p.vsib = None then p.rm else -1);
                    operands = att;
                    mode = (if p.mode64 then 64 else if p.not64 then 32 else 0);
                    disp8n = (if space = "evex" then disp8_scale p else 1);
                    evex_p2 =
                      (if p.scc >= 0 then p.scc
                       else (if p.nd then 0x10 else 0) lor if p.nf then 0x04 else 0);
                    mask =
                      (let has l =
                         List.exists (fun (o : R.x86_operand) -> o.lookupfn_name = Some l) operands
                       in
                       if has "MASKNOT0" then 3
                       else if has "MASK1" then if p.nozero then 2 else 1
                       else 0);
                    (* a y-width GPR form is a 32-bit (W0) and a 64-bit (W1) row; an APX map-4 form
                       is a legacy instruction's EVEX promotion, spelled with its size suffix *)
                    sized = variable || (space = "evex" && opcode_map = 4);
                    no_acc = [];
                    widths =
                      (match p.rexw with
                      | 0 -> [ 32 ]
                      | 1 -> [ 64 ]
                      (* APX's 16-bit forms are EVEX.pp = 66 *)
                      | _ when space = "evex" && opcode_map = 4 && prefix = 0x66 -> [ 16 ]
                      | _ -> [ 32; 64 ]);
                  })
      | _ -> None)
  | ( R.X86_encoding { space = "legacy"; opcode_map; opcode; pattern; operands },
      R.Xed_provenance
        {
          iform = Some iform;
          isa_set = Some (("X87" | "FCMOV" | "FCOMI" | "SSE3X87") as isa_set);
          _;
        } ) ->
      x87_spec rec_ ~iform ~isa_set ~opcode_map ~opcode ~pattern ~operands
  | ( R.X86_encoding { space = "legacy"; opcode_map; opcode; pattern; operands },
      R.Xed_provenance { iform = Some iform; isa_set = Some isa_set; _ } ) -> (
      match
        (parse_pattern pattern, Option.map (opcode_byte ~pattern) (int_of_string_opt opcode))
      with
      (* map 4 is 3DNow!'s 0F 0F escape, its opcode byte trailing *)
      | Some p, Some opcode when (not p.vex) && opcode_map <= 4 -> (
          let ops = List.map operand_of operands in
          if List.mem None ops then None
          else
            let xed_order = List.filter_map Fun.id (List.filter_map Fun.id ops) in
            let has_mem = List.exists (function Mem _ -> true | _ -> false) xed_order in
            let has_gpr =
              List.exists
                (function Reg { cls = Gpr8 | Gpr16 | Gpr32 | Gpr64; _ } -> true | _ -> false)
                xed_order
            in
            let has_xmm =
              List.exists (function Reg { cls = Xmm | Mmx; _ } -> true | _ -> false) xed_order
            in
            (* the SSE shape first: xmm operands, the mandatory prefix from REP/OSZ *)
            let prefix =
              match (p.rep, p.osz) with
              | 2, (-1 | 0) -> Some 0xf2
              | 3, (-1 | 0) -> Some 0xf3
              | (-1 | 0), 1 -> Some 0x66
              | (-1 | 0), (-1 | 0) -> Some 0
              | _ -> None
            in
            match prefix with
            | Some prefix
              when has_xmm && p.modrm
                   && (not (has_mem && has_gpr))
                   && p.memory = Some true = has_mem
                   && List.for_all
                        (function Reg { field = Vvvv | Is4; _ } -> false | _ -> true)
                        xed_order ->
                Some
                  {
                    record_id = rec_.record_id;
                    iform;
                    isa_set;
                    suffix_isa = isa_set;
                    df64 = p.df64;
                    pseudo = "";
                    mnemonic = att_mnemonic ~vl:p.vl ~iclass:rec_.native_name (List.rev xed_order);
                    space = `Legacy;
                    map = opcode_map;
                    opcode;
                    prefix;
                    osz = false;
                    w = p.rexw;
                    l = -1;
                    digit = p.digit;
                    rm = (if p.vsib = None then p.rm else -1);
                    operands = List.rev xed_order;
                    mode = (if p.mode64 then 64 else if p.not64 then 32 else 0);
                    disp8n = 1;
                    evex_p2 = 0;
                    mask = 0;
                    sized = false;
                    no_acc = [];
                    widths = [];
                  }
            | _
              when (not has_xmm)
                   && ((not p.modrm) || p.memory = Some true = has_mem)
                   && gpr_ok xed_order ~iclass:rec_.native_name ->
                (* OSZ=1 on a form whose width is fixed at 32/64 bits is a mandatory 0x66 (adcx,
                   tpause), not an operand size *)
                let sized16 =
                  List.exists
                    (function
                      | Reg { cls = Gpr16 | Gprv; _ } | Mem { bits = -1 | 16 } -> true | _ -> false)
                    xed_order
                in
                let mandatory66 = p.refining66 || (p.osz = 1 && not sized16) in
                let word_source =
                  List.exists
                    (function Reg { cls = Gpr16; _ } | Mem { bits = 16 } -> true | _ -> false)
                    xed_order
                in
                let prefix =
                  (* LOCK=1 on a plain iclass (xchg) only permits the prefix *)
                  if p.lock && ends_with ~suffix:"_LOCK" rec_.native_name then 0xf0
                  else
                    match p.rep with 2 -> 0xf2 | 3 -> 0xf3 | _ -> if mandatory66 then 0x66 else 0
                in
                let digit =
                  if
                    p.digit < 0 && p.free_reg
                    && not
                         (List.exists
                            (function Reg { field = Modrm_reg; _ } -> true | _ -> false)
                            xed_order)
                  then 0
                  else p.digit
                in
                (* a fixed 16-bit operand takes 0x66 only where the pattern says so (lldt, ltr
                   and verr have none) *)
                let fixed16 =
                  p.osz = 1 && (not mandatory66)
                  && List.exists (function Reg { cls = Gpr16; _ } -> true | _ -> false) xed_order
                in
                Some
                  {
                    record_id = rec_.record_id;
                    iform;
                    isa_set;
                    suffix_isa = isa_set;
                    df64 = p.df64;
                    pseudo = "";
                    mnemonic =
                      (if movx rec_.native_name then
                         (if rec_.native_name = "MOVZX" then "movz" else "movs")
                         ^
                         if
                           List.exists
                             (function
                               | Reg { cls = Gpr8; _ } | Mem { bits = 8 } -> true | _ -> false)
                             xed_order
                         then "b"
                         else "w"
                       else integer_mnemonic ~rep:p.rep rec_.native_name);
                    space = `Legacy;
                    map = opcode_map;
                    opcode;
                    prefix;
                    (* a REP prefix with a mandatory 0x66 keeps both: rep movsw is 66 F3 A5 *)
                    osz = fixed16 || (mandatory66 && p.rep >= 2);
                    w = p.rexw;
                    l = -1;
                    digit;
                    rm = p.rm;
                    operands = List.rev xed_order;
                    mode = (if p.mode64 then 64 else if p.not64 then 32 else 0);
                    disp8n = 1;
                    evex_p2 = 0;
                    mask = 0;
                    sized = true;
                    no_acc =
                      (if p.srm_nonzero then
                         List.concat
                           (List.mapi
                              (fun k o ->
                                match o with Reg { field = Opcode_low; _ } -> [ k ] | _ -> [])
                              (List.rev xed_order))
                       else []);
                    (* GNU as spells lar/lsl/sldt/str with a q suffix but emits no REX.W *)
                    widths =
                      List.filter
                        (fun w ->
                          (w <> 64
                          || not
                               (List.mem rec_.native_name
                                  [ "LAR"; "LSL"; "SLDT"; "STR"; "LFS"; "LGS"; "LSS" ]))
                          (* nor is there a 16-to-16 movzww *)
                          && not (w = 16 && movx rec_.native_name && word_source))
                        (match p.osz with
                        | _ when mandatory66 -> [ 32; 64 ]
                        | 1 -> [ 16 ]
                        | 0 -> [ 32; 64 ]
                        | _ -> [ 16; 32; 64 ]);
                  }
            | _ -> None)
      | _ -> None)
  | _ -> None

let operand_name i = Printf.sprintf "op%d" i

(* XED lists an EVEX form's embedded-rounding register variant under the plain form's iform; a
   differential case must name one or the other, so the rounding one is keyed apart. *)
let rounding_suffix = "#er"

(* likewise an APX form's {nf} variant *)
let nf_suffix = "#nf"

let lookup_key (rec_ : R.t) =
  match (rec_.provenance, rec_.encoding) with
  | R.Xed_provenance { iform = Some iform; _ }, R.X86_encoding { pattern; _ } ->
      let tokens = String.split_on_char ' ' pattern in
      if List.mem "BCRC=1" tokens && List.mem "MOD=3" tokens then iform ^ rounding_suffix
      else if List.mem "NF=1" tokens && not (List.mem "EVAPX_SCC()" tokens) then iform ^ nf_suffix
      else iform
  | R.Xed_provenance { iform = Some iform; _ }, _ -> iform
  | _ -> ""

let spec_lookup_key spec =
  if List.exists (function Rounding _ -> true | _ -> false) spec.operands then
    spec.iform ^ rounding_suffix
  else if spec.evex_p2 land 4 <> 0 && not (List.mem Dfv spec.operands) then spec.iform ^ nf_suffix
  else spec.iform

(* The row a normalized form and its first case describe: the 32-bit one of a width-variable
   integer form. *)
let canonical spec =
  let rows = expand spec in
  match
    List.find_opt
      (fun r ->
        spec.sized
        && List.exists
             (function Reg { cls = Gpr32; _ } | Mem { bits = 32 } -> true | _ -> false)
             r.operands)
      rows
  with
  | Some r -> r
  | None -> List.hd rows

let form ~requirement (rec_ : R.t) spec =
  let spec = canonical spec in
  let class_ = function
    | Xmm -> X86_xmm
    | Ymm -> X86_ymm
    | Zmm -> X86_zmm
    | St -> X87_st
    | Tmm -> X86_tmm
    | Mmx -> X86_mmx
    | Kmask -> X86_kmask
    | Gpr8 | Gpr16 | Gpr32 | Gpr64 | Gprv -> X86_gpr
  in
  let operands =
    List.concat
    @@ List.mapi
         (fun i o ->
           let op_kind =
             match o with
             | Fixed_reg _ -> None
             | Rounding _ -> Some Rounding_mode
             | One | Dfv -> None
             | Vsib _ -> Some (Memory { width_bits = None })
             | Reg { cls; _ } -> Some (Register { class_ = class_ cls; excluded = [] })
             | Mem { bits } ->
                 Some (Memory { width_bits = (if bits <= 0 then None else Some bits) })
             | Imm { bytes } ->
                 Some
                   (Immediate
                      {
                        width_bits = 8 * bytes;
                        signed = false;
                        implicit_low_zero_bits = 0;
                        nonzero = false;
                        runs = [];
                      })
           in
           Option.to_list
             (Option.map
                (fun op_kind -> { op_name = operand_name i; op_kind; role = In; explicit = true })
                op_kind))
         spec.operands
  in
  let syntax =
    List.concat
    @@ List.mapi
         (fun i o ->
           match o with
           (* {dfv=...} takes no comma after it: it is spelled with the mnemonic *)
           | Dfv -> []
           | o -> [ (i, o) ])
         spec.operands
    |> List.map (fun (i, o) ->
        match o with
        | Reg _ -> Syn_decorated ("%", Syn_operand (operand_name i))
        (* spelled whole, braces included: {rn-sae} *)
        | Rounding _ | Vsib _ | Dfv -> Syn_operand (operand_name i)
        | One -> Syn_decorated ("$", Syn_operand (operand_name i))
        | Imm _ -> Syn_decorated ("$", Syn_operand (operand_name i))
        | Mem _ -> Syn_operand (operand_name i)
        (* an implied register is assigned per case: its spelling follows the operand size *)
        | Fixed_reg _ -> Syn_decorated ("%", Syn_operand (operand_name i)))
  in
  {
    form_id = "x86:" ^ spec.iform;
    arch = X86;
    native_name = rec_.native_name;
    source_record_ids = [ rec_.record_id ];
    requirement;
    encoding =
      X86_encoding
        {
          space =
            (match spec.space with
            | `Vex -> "vex"
            | `Evex -> "evex"
            | `Xop -> "xop"
            | `Legacy -> "legacy");
          opcode_map = spec.map;
          opcode = Printf.sprintf "0x%02X" spec.opcode;
          pattern = (match rec_.encoding with R.X86_encoding { pattern; _ } -> pattern | _ -> "");
        };
    operands;
    syntax = { dialect = "gas-att"; mnemonic = spec.mnemonic; operands = syntax };
    concreteness = Concrete;
    facts =
      [
        {
          label = Upstream;
          note = "space, map, opcode, pattern and operand lookups taken verbatim from the record";
        };
        {
          label = Inferred;
          note =
            "AT&T operand order is XED's explicit order reversed; the mnemonic is the XED iclass \
             in lower case (DEC-X86-TABLE)";
        };
      ];
    diagnostics = [];
  }

(* Two forms spelled the same way with the same operand shape (VMOVAPD's 0x28
   and 0x29 register forms, FMA4's W0 and W1 register forms): GNU as reaches
   only one of them from that spelling. The first in export order is the table
   form; each later one maps to it. *)
let twin_primaries specs =
  let specs = List.concat_map expand specs in
  let shape spec =
    ( spec.mnemonic,
      List.map
        (function
          | Reg { cls; _ } -> `Reg cls
          | Mem _ -> `Mem
          | Imm { bytes } -> `Imm bytes
          | Fixed_reg n -> `Fixed n
          | Rounding _ -> `Rounding
          | One -> `One
          | Dfv -> `Dfv
          | Vsib { cls } -> `Vsib cls)
        spec.operands )
  in
  let groups = Hashtbl.create 64 in
  List.iter
    (fun spec ->
      let k = shape spec in
      Hashtbl.replace groups k (spec :: Option.value (Hashtbl.find_opt groups k) ~default:[]))
    specs;
  let secondaries = Hashtbl.create 16 in
  Hashtbl.iter
    (fun _ group ->
      let group = List.rev group in
      let records = List.sort_uniq compare (List.map (fun s -> s.record_id) group) in
      if List.length records > 1 then
        (* GNU as encodes an is4 register form with VEX.W = 1 (the last source in ModR/M.rm) *)
        let is4 s =
          List.exists (function Reg { field = Is4; _ } -> true | _ -> false) s.operands
        in
        (* GNU as's choice among same-spelled forms, in order: an is4 form takes VEX.W = 1
           (last source in ModR/M.rm) unless it also carries an immediate (vpermil2ps: W = 0) or
           is XOP (vpcmov: W = 0);
           a register move puts its destination in ModR/M.reg (the load opcode); otherwise
           the W0 / W-ignored form. *)
        let has_imm s = List.exists (function Imm _ -> true | _ -> false) s.operands in
        let dest_in_reg s =
          match List.rev s.operands with Reg { field = Modrm_reg; _ } :: _ -> true | _ -> false
        in
        let dest_in_rm s =
          match List.rev s.operands with Reg { field = Modrm_rm; _ } :: _ -> true | _ -> false
        in
        let integer s =
          List.for_all
            (function Reg { cls = Xmm | Ymm | Zmm | Mmx | Kmask; _ } -> false | _ -> true)
            s.operands
        in
        let short s =
          List.exists (function Reg { field = Opcode_low; _ } -> true | _ -> false) s.operands
        in
        (* integer templates list the register-destination form second (0x00 before 0x02) while
           SIMD moves list the load form first (0x28 before 0x29); the short 0x40+r form wins
           where it exists; the multi-byte NOP is 0F 1F *)
        let rank s =
          ( (* GNU picks VEX for a spelling both could encode, but EVEX over the VEX-only AVX
               VNNI/IFMA/NE-CONVERT sets *)
            (if List.mem s.isa_set evex_preferred then false else s.space <> `Evex),
            (* an APX {nf} form only through its pseudo-prefix *)
            s.evex_p2 land 4 = 0,
            (if is4 s then if has_imm s || s.space = `Xop then s.w <> 1 else s.w = 1 else true),
            short s,
            (* integer forms take ModR/M.rm as the destination, but movbe loads *)
            (if integer s && not (starts_with ~prefix:"MOVBE" s.iform) then dest_in_rm s
             else dest_in_reg s),
            s.opcode = 0x1f,
            s.w <> 1,
            (* last, the lower opcode: EVEX vmovd is 6E/7E, not XED's F3 7E / 66 D6 aliases *)
            -((s.map * 256) + s.opcode),
            (* and the lower fixed ModR/M.reg: shl is /4, not its /6 alias *)
            -s.digit )
        in
        (* best first; a tie keeps the listed order *)
        let ranked = List.stable_sort (fun a b -> compare (rank b) (rank a)) group in
        let primary = List.hd ranked in
        List.iteri
          (fun i s ->
            if s.record_id <> primary.record_id && not (Hashtbl.mem secondaries s.record_id) then
              Hashtbl.replace secondaries s.record_id (s, primary, i))
          ranked)
    groups;
  secondaries

let twins specs =
  let t = Hashtbl.create 64 in
  Hashtbl.iter
    (fun id (_, (primary : spec), _) -> Hashtbl.replace t id primary.iform)
    (twin_primaries specs);
  t

let twin_rank specs =
  let t = Hashtbl.create 64 in
  Hashtbl.iter (fun id (_, _, i) -> Hashtbl.replace t id i) (twin_primaries specs);
  t

(* The pseudo-prefix that makes GNU as pick a twin over its primary: the encoding space, or
   which ModR/M field holds the destination. Only a twin with its own iform can be told apart
   from its primary by a case keyed on the iform. *)
let pseudo_prefix ~(primary : spec) (s : spec) =
  let dest x = match List.rev x.operands with Reg { field; _ } :: _ -> Some field | _ -> None in
  if s.pseudo <> "" && primary.pseudo <> s.pseudo then Some s.pseudo
  else if s.iform = primary.iform then None
  else if s.space <> primary.space then
    match s.space with `Evex -> Some "evex" | `Vex -> Some "vex" | `Legacy | `Xop -> None
  else
    match (dest s, dest primary) with
    | Some Modrm_reg, Some Modrm_rm -> Some "load"
    | Some Modrm_rm, Some Modrm_reg -> Some "store"
    | _ -> None

(* A pseudo-prefix reaches the best-ranked twin it allows, so of several twins sharing a prefix
   only that one is reachable. *)
let reachable_twins specs =
  let all = twin_primaries specs in
  let best = Hashtbl.create 64 in
  Hashtbl.iter
    (fun _ ((s : spec), (primary : spec), i) ->
      match pseudo_prefix ~primary s with
      | Some p -> (
          let k = (primary.record_id, s.mnemonic, p) in
          match Hashtbl.find_opt best k with
          | Some (j, _) when j <= i -> ()
          | _ -> Hashtbl.replace best k (i, s.record_id))
      | None -> ())
    all;
  let t = Hashtbl.create 64 in
  Hashtbl.iter (fun (_, _, p) (_, id) -> Hashtbl.replace t id p) best;
  t

(* The accumulator guard: for each integer spec, the AT&T positions where GNU as would pick an
   accumulator-specific form of the same instruction (a sibling record with an implicit
   AL/AX/EAX/RAX and the same operand kinds elsewhere). [xchg] is symmetric: either operand. *)
let accumulator_positions (records : R.t list) spec =
  let kind = function
    | Reg { cls = Gpr8; _ } -> `Gpr 8
    | Reg { cls = Gpr16; _ } -> `Gpr 16
    | Reg { cls = Gpr32; _ } -> `Gpr 32
    | Reg { cls = Gpr64; _ } -> `Gpr 64
    | Reg { cls = Gprv; _ } -> `Gpr 0
    | Reg _ -> `Vec
    | Mem _ -> `Mem
    (* the accumulator forms take a full-size immediate; an imm8 form is not their twin *)
    | Imm { bytes } -> `Imm (bytes = 1)
    | Fixed_reg n -> `Fixed n
    | Rounding _ -> `Rounding
    | One -> `One
    | Dfv -> `Dfv
    | Vsib _ -> `Vsib
  in
  (* the accumulator a sibling names, against the register width at the same position *)
  let covers acc width =
    match (acc, width) with
    | "al", 8 | "ax", 16 | "eax", 32 | "rax", 64 -> true
    | "?ax", (16 | 32 | 64) -> true
    | _ -> false
  in
  let iclass =
    match String.index_opt spec.iform '_' with
    | Some i -> String.sub spec.iform 0 i
    | None -> spec.iform
  in
  let siblings =
    List.filter_map
      (fun (r : R.t) ->
        if r.native_name <> iclass then None
        else
          match r.encoding with
          | R.X86_encoding { operands; _ } ->
              let ops = List.map operand_of operands in
              if List.mem None ops then None
              else
                Some
                  (List.rev (List.map kind (List.filter_map Fun.id (List.filter_map Fun.id ops))))
          | _ -> None)
      records
  in
  let mine = List.map kind spec.operands in
  let same a b = match (a, b) with `Gpr _, `Gpr _ -> true | _ -> a = b in
  List.sort_uniq compare
    (List.concat_map
       (fun sib ->
         if List.length sib <> List.length mine then []
         else
           let positions = List.mapi (fun i (a, b) -> (i, a, b)) (List.combine sib mine) in
           let acc =
             List.filter
               (fun (_, a, b) -> match (a, b) with `Fixed n, `Gpr w -> covers n w | _ -> false)
               positions
           in
           let rest_ok =
             List.for_all
               (fun (_, a, b) -> (match a with `Fixed _ -> true | _ -> false) || same a b)
               positions
           in
           if acc = [] || not rest_ok then []
           else if iclass = "XCHG" then
             List.filter_map
               (fun (i, _, b) -> match b with `Gpr _ -> Some i | _ -> None)
               positions
           else List.map (fun (i, _, _) -> i) acc)
       siblings)

(* Relative near branches (jcc and jmp with rel8 or rel32, call rel32): the displacement is a
   signed immediate GNU as resolves from a label, and the assembler's relaxation picks the
   width, so a case places its label at a distance only that width reaches. *)
let branch (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { operands; _ } -> (
      let n = rec_.native_name in
      let conditional =
        String.length n >= 2
        && n.[0] = 'J'
        && not (List.mem n [ "JMP"; "JMP_FAR"; "JCXZ"; "JECXZ"; "JRCXZ"; "JMPABS" ])
      in
      match List.find_opt (fun (o : R.x86_operand) -> o.op_name = "RELBR") operands with
      | Some o when conditional || n = "JMP" || n = "CALL_NEAR" -> (
          let mnemonic = if n = "CALL_NEAR" then "call" else String.lowercase_ascii n in
          match o.oc2 with
          | Some "b" -> Some (mnemonic, 8)
          | Some "d" -> Some (mnemonic, 32)
          | _ -> None)
      | _ -> None)
  | _ -> None

let branch_form ~requirement (rec_ : R.t) =
  match (branch rec_, rec_.encoding, rec_.provenance) with
  | ( Some (mnemonic, bits),
      R.X86_encoding { space; opcode_map; opcode; pattern; _ },
      R.Xed_provenance { iform = Some iform; _ } ) ->
      Some
        {
          form_id = "x86:" ^ iform;
          arch = X86;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding = X86_encoding { space; opcode_map; opcode; pattern };
          operands =
            [
              {
                op_name = "target";
                op_kind =
                  Immediate
                    {
                      width_bits = bits;
                      signed = true;
                      implicit_low_zero_bits = 0;
                      nonzero = false;
                      runs = [];
                    };
                role = In;
                explicit = true;
              };
            ];
          syntax = { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "target" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "opcode, pattern and displacement width taken verbatim from the record";
              };
              {
                label = Inferred;
                note =
                  "GAS resolves the displacement from a label; cases place it where only this \
                   width reaches (DEC-X86-TABLE)";
              };
            ];
          diagnostics = [];
        }
  | _ -> None
