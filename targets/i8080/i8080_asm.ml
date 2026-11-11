(* Typed builders for the Intel 8080 instructions, and programs of them.

   A builder selects its {!I8080_ops} entry by mnemonic and wraps its operands, so
   no opcode byte is listed here; the encoder's lowering still checks ranges. The
   byte registers are {!I8080_encode.Reg.t}; the register pairs are closed
   polymorphic variants, so a pair where a byte register is expected, or [sp]
   where [push] wants [psw], does not compile. *)

open Foundation
open Asm_core
module E = I8080_encode
module Ops = I8080_ops
module Operand = E.Operand
module Reg = E.Reg

type t = E.Instruction.t
type reg = Reg.t
type pair = [ `Bc | `De | `Hl | `Sp ]
type stack_pair = [ `Bc | `De | `Hl | `Psw ]
type ind_pair = [ `Bc | `De ]

(* {1 Operands} *)

let b = Reg.B
let c = Reg.C
let d = Reg.D
let e = Reg.E
let h = Reg.H
let l = Reg.L
let m = Reg.M
let a = Reg.A
let bc = `Bc
let de = `De
let hl = `Hl
let sp = `Sp
let psw = `Psw
let symbol s = Expr.Symbol s
let const n = Expr.Const (Bigint.of_int n)
let plus x y = Expr.Binary (Expr.Add, x, y)
let minus x y = Expr.Binary (Expr.Sub, x, y)
let low x = Expr.Binary (Expr.And, x, const 0xff)
let high x = Expr.Binary (Expr.And, Expr.Binary (Expr.Shr, x, const 8), const 0xff)
let reg_op r = Operand.Reg r

let pair_op : [< `Bc | `De | `Hl | `Sp | `Psw ] -> Operand.t = function
  | `Bc -> Operand.Reg Reg.B
  | `De -> Operand.Reg Reg.D
  | `Hl -> Operand.Reg Reg.H
  | `Sp -> Operand.Sp
  | `Psw -> Operand.Psw

let imm_op n = Operand.Imm (Bigint.of_int n)

(* A constant expression is a constant, so range checks see its value. *)
let expr_op x =
  match Expr.fold Expr.no_env x with
  | Ok (Expr.Const v) -> Operand.Imm v
  | Ok _ | Error _ -> Operand.Sym x

let entry name =
  match Ops.find_entry name with
  | Some e -> e
  | None -> invalid_arg ("I8080_asm: no table entry for " ^ name)

let mk name args : t = E.Instruction.Insn { entry = entry name; args }

(* {1 Instructions} *)

let nop () = mk "nop" []
let hlt () = mk "hlt" []
let ret () = mk "ret" []
let rnz () = mk "rnz" []
let rz () = mk "rz" []
let rnc () = mk "rnc" []
let rc () = mk "rc" []
let rpo () = mk "rpo" []
let rpe () = mk "rpe" []
let rp () = mk "rp" []
let rm () = mk "rm" []
let xchg () = mk "xchg" []
let xthl () = mk "xthl" []
let sphl () = mk "sphl" []
let pchl () = mk "pchl" []
let ei () = mk "ei" []
let di () = mk "di" []
let daa () = mk "daa" []
let cma () = mk "cma" []
let stc () = mk "stc" []
let cmc () = mk "cmc" []
let rlc () = mk "rlc" []
let rrc () = mk "rrc" []
let ral () = mk "ral" []
let rar () = mk "rar" []
let mov r s = mk "mov" [ reg_op r; reg_op s ]
let inr r = mk "inr" [ reg_op r ]
let dcr r = mk "dcr" [ reg_op r ]
let add r = mk "add" [ reg_op r ]
let adc r = mk "adc" [ reg_op r ]
let sub r = mk "sub" [ reg_op r ]
let sbb r = mk "sbb" [ reg_op r ]
let ana r = mk "ana" [ reg_op r ]
let xra r = mk "xra" [ reg_op r ]
let ora r = mk "ora" [ reg_op r ]
let cmp r = mk "cmp" [ reg_op r ]
let mvi r n = mk "mvi" [ reg_op r; imm_op n ]
let mvi_e r x = mk "mvi" [ reg_op r; expr_op x ]
let lxi p n = mk "lxi" [ pair_op p; imm_op n ]
let lxi_e p x = mk "lxi" [ pair_op p; expr_op x ]
let inx p = mk "inx" [ pair_op p ]
let dcx p = mk "dcx" [ pair_op p ]
let dad p = mk "dad" [ pair_op p ]
let ldax p = mk "ldax" [ pair_op p ]
let stax p = mk "stax" [ pair_op p ]
let push p = mk "push" [ pair_op p ]
let pop p = mk "pop" [ pair_op p ]
let rst n = mk "rst" [ imm_op n ]
let imm1 name n = mk name [ imm_op n ]
let expr1 name x = mk name [ expr_op x ]
let adi = imm1 "adi"
let adi_e = expr1 "adi"
let aci = imm1 "aci"
let aci_e = expr1 "aci"
let sui = imm1 "sui"
let sui_e = expr1 "sui"
let sbi = imm1 "sbi"
let sbi_e = expr1 "sbi"
let ani = imm1 "ani"
let ani_e = expr1 "ani"
let xri = imm1 "xri"
let xri_e = expr1 "xri"
let ori = imm1 "ori"
let ori_e = expr1 "ori"
let cpi = imm1 "cpi"
let cpi_e = expr1 "cpi"
let in_ = imm1 "in"
let in_e = expr1 "in"
let out = imm1 "out"
let out_e = expr1 "out"
let jmp = imm1 "jmp"
let jmp_e = expr1 "jmp"
let jnz = imm1 "jnz"
let jnz_e = expr1 "jnz"
let jz = imm1 "jz"
let jz_e = expr1 "jz"
let jnc = imm1 "jnc"
let jnc_e = expr1 "jnc"
let jc = imm1 "jc"
let jc_e = expr1 "jc"
let jpo = imm1 "jpo"
let jpo_e = expr1 "jpo"
let jpe = imm1 "jpe"
let jpe_e = expr1 "jpe"
let jp = imm1 "jp"
let jp_e = expr1 "jp"
let jm = imm1 "jm"
let jm_e = expr1 "jm"
let call = imm1 "call"
let call_e = expr1 "call"
let cnz = imm1 "cnz"
let cnz_e = expr1 "cnz"
let cz = imm1 "cz"
let cz_e = expr1 "cz"
let cnc = imm1 "cnc"
let cnc_e = expr1 "cnc"
let cc = imm1 "cc"
let cc_e = expr1 "cc"
let cpo = imm1 "cpo"
let cpo_e = expr1 "cpo"
let cpe = imm1 "cpe"
let cpe_e = expr1 "cpe"
let cp = imm1 "cp"
let cp_e = expr1 "cp"
let cm = imm1 "cm"
let cm_e = expr1 "cm"
let sta = imm1 "sta"
let sta_e = expr1 "sta"
let lda = imm1 "lda"
let lda_e = expr1 "lda"
let shld = imm1 "shld"
let shld_e = expr1 "shld"
let lhld = imm1 "lhld"
let lhld_e = expr1 "lhld"

(* {1 Program items} *)

type item =
  | Insn of t
  | Label of string
  | Data of { width : int; items : Operand.t list }
  | Space of int

let insn i = Insn i
let label name = Label name
let db values = Data { width = 1; items = List.map imm_op values }
let db_e values = Data { width = 1; items = List.map expr_op values }
let dw values = Data { width = 2; items = List.map imm_op values }
let dw_e values = Data { width = 2; items = List.map expr_op values }
let ascii s = Data { width = 1; items = [ Operand.Str s ] }
let space n = Space n

module Program = struct
  type image = { origin : int; bytes : string; symbols : (string * int) list }

  let address_space = 0x10000
  let synthesized = Origin.synthesized ~pass:"i8080"

  let fail kind =
    Diag.fail ~pos:__POS__ [ E.error_diagnostic (Target_error.make ~origin:(synthesized ()) kind) ]

  let bind ~origin laid =
    let open Err.Syntax in
    let* img = Image.bind_image laid ~addresses:[ (".text", Int64.of_int origin) ] in
    let symbols =
      List.sort
        (fun (n1, a1) (n2, a2) -> if a1 <> a2 then compare a1 a2 else String.compare n1 n2)
        (List.map (fun (n, a) -> (n, Int64.to_int a)) img.Image.exports)
    in
    match img.Image.segments with
    | [] -> Ok { origin; bytes = ""; symbols }
    | [ s ] ->
        if origin + String.length s.Image.bytes + s.Image.zero_fill > address_space then
          fail (`Address_space (String.length s.Image.bytes + s.Image.zero_fill, origin))
        else Ok { origin; bytes = s.Image.bytes ^ String.make s.Image.zero_fill '\000'; symbols }
    | _ -> fail `Many_sections

  module P = Driver_direct.Pipeline_direct.Make (E)

  let normalized ~unit_name items =
    let origin = synthesized () in
    let section =
      Normalized_ast.Directive
        {
          directive = Directive.Section { name = ".text"; perms = Perms.rx; nobits = false };
          origin;
        }
    in
    let item = function
      | Insn insn -> [ Normalized_ast.Instruction { insn; origin } ]
      | Label name ->
          [
            Normalized_ast.Directive { directive = Directive.Global { name }; origin };
            Normalized_ast.Label { name; origin };
          ]
      | Data { width; items } ->
          [ Normalized_ast.Instruction { insn = E.Instruction.Data { width; items }; origin } ]
      | Space length ->
          [ Normalized_ast.Directive { directive = Directive.Zero { length }; origin } ]
    in
    { Normalized_ast.unit_name; items = section :: List.concat_map item items }

  let assemble ?(origin = 0) ?(unit_name = "i8080") items =
    let open Err.Syntax in
    let stage r = Diag.stage ~pos:__POS__ Err.Action.Map r in
    let bad_origin = origin < 0 || origin >= address_space
    and bad_space =
      List.find_map
        (function Space n when n < 0 || n >= address_space -> Some n | _ -> None)
        items
    in
    if bad_origin then
      fail (`Out_of_range { what = "origin"; value = Bigint.of_int origin; low = 0; high = 0xffff })
    else
      match bad_space with
      | Some n ->
          fail (`Out_of_range { what = "space"; value = Bigint.of_int n; low = 0; high = 0xffff })
      | None ->
          let* state = P.initial_state None in
          let* low = stage (P.lower ~state (normalized ~unit_name items)) in
          let* laid = stage (P.plan low) in
          bind ~origin laid

  let symbol image name = List.assoc_opt name image.symbols
end
