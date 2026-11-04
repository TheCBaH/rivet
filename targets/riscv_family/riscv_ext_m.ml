module Opcode = Riscv_types.Opcode

type form = {
  op : Opcode.t;
  mnemonic : string;
  feature : string;
  opcode : int;
  funct3 : int;
  funct7 : int;
  rv64_only : bool;
  source : string;
}

(* Read from Riscv_ops: the forms either component owns, with their R-format fields. *)
let forms =
  List.filter_map
    (fun (e : Riscv_ops.entry) ->
      match (e.ext, e.enc, e.src) with
      | [ feature ], Some (R { opcode; funct3; funct7; _ }), Some source
        when String.equal feature "m" || String.equal feature "zmmul" ->
          Some
            {
              op = e.op;
              mnemonic = Opcode.mnemonic e.op;
              feature;
              opcode;
              funct3;
              funct7;
              rv64_only = e.xlen = Rv64;
              source;
            }
      | _ -> None)
    (Array.to_list Riscv_ops.all)

let find op = List.find_opt (fun f -> f.op = op) forms

let component ~id ~feature ~requires ~summary =
  {
    Target_component.id;
    feature;
    requires;
    conflicts = [];
    implies = [];
    default_on = true;
    xlens = [];
    not_applicable = None;
    summary;
    forms =
      List.filter_map
        (fun f ->
          if not (String.equal f.feature feature) then None
          else
            Some
              {
                Target_component.label = f.mnemonic;
                mnemonics = [ f.mnemonic ];
                sources = [ { upstream = "riscv-opcodes"; name = f.source } ];
                legal = None;
              })
        forms;
  }

let zmmul =
  component ~id:"riscv.zmmul" ~feature:"zmmul" ~requires:[]
    ~summary:"integer multiply: mul, mulh, mulhsu, mulhu, and RV64 mulw"

let m =
  component ~id:"riscv.m" ~feature:"m" ~requires:[ "zmmul" ]
    ~summary:
      "integer division and remainder: div, divu, rem, remu, and RV64 divw, divuw, remw, remuw"
