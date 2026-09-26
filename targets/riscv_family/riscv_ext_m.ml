type form = {
  mnemonic : string;
  feature : string;
  opcode : int;
  funct3 : int;
  funct7 : int;
  rv64_only : bool;
  source : string;
}

let form ?(rv64_only = false) mnemonic feature opcode funct3 funct7 source =
  { mnemonic; feature; opcode; funct3; funct7; rv64_only; source }

let forms =
  [
    form "mul" "zmmul" 0x33 0 0x01 "rv_m/mul";
    form ~rv64_only:true "mulw" "zmmul" 0x3b 0 0x01 "rv64_m/mulw";
    form "remu" "m" 0x33 7 0x01 "rv_m/remu";
  ]

let find mnemonic = List.find_opt (fun f -> String.equal f.mnemonic mnemonic) forms

let component ~id ~feature ~requires ~summary =
  {
    Target_component.id;
    feature;
    requires;
    conflicts = [];
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
              })
        forms;
  }

let zmmul =
  component ~id:"riscv.zmmul" ~feature:"zmmul" ~requires:[]
    ~summary:"integer multiply: mul, and RV64 mulw"

let m =
  component ~id:"riscv.m" ~feature:"m" ~requires:[ "zmmul" ] ~summary:"integer remainder: remu"
