type operand_shape = Memory | Symbol | Memory_or_symbol

type memory_form = {
  mnemonic : string;
  label : string;
  priority : int;
  opcode_byte : int;
  ext : int;
  shape : operand_shape;
  source : string;
}

let mem ?(shape = Memory) mnemonic priority opcode_byte ext source =
  { mnemonic; label = mnemonic; priority; opcode_byte; ext; shape; source }

let memory_forms =
  [
    mem "fldl" 50 0xDD 0 "FLD_ST0_MEMm64real";
    mem "fstpl" 51 0xDD 3 "FSTP_MEMm64real_ST0";
    mem "fstps" 52 0xD9 3 "FSTP_MEMmem32real_ST0";
    mem ~shape:Memory_or_symbol "flds" 54 0xD9 0 "FLD_ST0_MEMmem32real";
    mem "fildll" 56 0xDF 5 "FILD_ST0_MEMm64int";
    mem ~shape:Symbol "fadds" 57 0xD8 0 "FADD_MEMmem32real";
    mem "fnstcw" 58 0xD9 7 "FNSTCW_MEMmem16";
    mem "fldcw" 59 0xD9 5 "FLDCW_MEMmem16";
    mem "fistpll" 60 0xDF 7 "FISTP_MEMm64int_ST0";
    mem ~shape:Symbol "fsubs" 61 0xD8 4 "FSUB_ST0_MEMmem32real";
  ]

type fixed_form = {
  mnemonic : string;
  label : string;
  priority : int;
  word : int64;
  bits : int;
  source : string;
}

let fixed_forms =
  [
    {
      mnemonic = "fucomp";
      label = "fucomp";
      priority = 55;
      word = 0xDDE9L;
      bits = 16;
      source = "FUCOMP_ST0_X87";
    };
    {
      mnemonic = "fnstsw";
      label = "fnstsw";
      priority = 62;
      word = 0xDFE0L;
      bits = 16;
      source = "FNSTSW_AX";
    };
  ]

let fadd_st0 =
  {
    mnemonic = "fadd";
    label = "fadd-st0-x87";
    priority = 65;
    opcode_byte = 0xD8;
    ext = 0;
    shape = Memory;
    source = "FADD_ST0_X87";
  }

let find_memory mnemonic =
  List.find_opt (fun (f : memory_form) -> String.equal f.mnemonic mnemonic) memory_forms

let owns mnemonic =
  Option.is_some (find_memory mnemonic)
  || String.equal mnemonic fadd_st0.mnemonic
  || List.exists (fun (f : fixed_form) -> String.equal f.mnemonic mnemonic) fixed_forms

let component : Target_component.t =
  let form ~label ~mnemonic ~source =
    {
      Target_component.label;
      mnemonics = [ mnemonic ];
      sources = [ { upstream = "xed"; name = source } ];
    }
  in
  {
    id = "x86.x87";
    feature = "x87";
    requires = [];
    conflicts = [];
    summary = "x87 loads, stores, single-precision add/subtract, control and status words";
    forms =
      List.map
        (fun (f : memory_form) -> form ~label:f.label ~mnemonic:f.mnemonic ~source:f.source)
        memory_forms
      @ List.map
          (fun (f : fixed_form) -> form ~label:f.label ~mnemonic:f.mnemonic ~source:f.source)
          fixed_forms
      @ [ form ~label:fadd_st0.label ~mnemonic:fadd_st0.mnemonic ~source:fadd_st0.source ];
  }
