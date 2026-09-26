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
    form "mulh" "zmmul" 0x33 1 0x01 "rv_m/mulh";
    form "mulhsu" "zmmul" 0x33 2 0x01 "rv_m/mulhsu";
    form "mulhu" "zmmul" 0x33 3 0x01 "rv_m/mulhu";
    form "div" "m" 0x33 4 0x01 "rv_m/div";
    form "divu" "m" 0x33 5 0x01 "rv_m/divu";
    form "rem" "m" 0x33 6 0x01 "rv_m/rem";
    form "remu" "m" 0x33 7 0x01 "rv_m/remu";
    form ~rv64_only:true "divw" "m" 0x3b 4 0x01 "rv64_m/divw";
    form ~rv64_only:true "divuw" "m" 0x3b 5 0x01 "rv64_m/divuw";
    form ~rv64_only:true "remw" "m" 0x3b 6 0x01 "rv64_m/remw";
    form ~rv64_only:true "remuw" "m" 0x3b 7 0x01 "rv64_m/remuw";
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
    ~summary:"integer multiply: mul, mulh, mulhsu, mulhu, and RV64 mulw"

let m =
  component ~id:"riscv.m" ~feature:"m" ~requires:[ "zmmul" ]
    ~summary:
      "integer division and remainder: div, divu, rem, remu, and RV64 divw, divuw, remw, remuw"
