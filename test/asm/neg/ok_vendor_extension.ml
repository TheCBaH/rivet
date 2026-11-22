(* requires: rivet.riscv_family rivet.riscv32_encode rivet.riscv_family_encode rivet.text_pipeline rivet.target_features *)
module Row = Rivet_riscv_family_encode.Riscv_table_row
module Ext = Rivet_riscv_family_encode.Extensions

let component : Rivet_target_features.Component.t =
  {
    id = "vendor.xfoo";
    feature = "xfoo";
    requires = [];
    conflicts = [];
    implies = [];
    default_on = false;
    xlens = [];
    not_applicable = None;
    summary = "";
    forms =
      [
        {
          Rivet_target_features.Component.label = "xfoo";
          mnemonics = [ "xfoo" ];
          sources = [ { upstream = "vendor"; name = "xfoo" } ];
          legal = None;
        };
      ];
  }

let row : Row.row =
  {
    mnemonic = "xfoo";
    mask = 0xfe00707fL;
    match_ = 0x0bL;
    operands = [ Row.Gpr { lsb = 7; nonzero = false } ];
    xlen = 0;
    feature = "xfoo";
    source = "vendor/xfoo";
  }

let validated =
  match Ext.make ~xlen:32 { components = [ component ]; rows = [| row |] } with
  | Ok v -> v
  | Error _ -> failwith "invalid"

module C =
  Rivet_riscv_family.Make_with
    (Rivet_riscv32_encode.Profile)
    (struct
      let definition = validated
    end)

module D = Rivet_text_pipeline.Pipeline.Make (C)

let _text ~address bytes =
  match Rivet_target_features.Config.parse_spec "+xfoo" with
  | Ok features -> D.dump_disasm_canonical ~features ~address bytes
  | Error m -> failwith m
