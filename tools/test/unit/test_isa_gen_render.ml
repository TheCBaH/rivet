(* Isa_gen_render: syntax-recipe
   substitution against synthetic recipes covering every syntax_token
   constructor, plus real pilot recipes taken from Isa_norm_riscv/
   Isa_norm_xed's own normalize functions on inline records - no filesystem
   dependency, matching the isa-norm-riscv/isa-norm-xed unit suites'
   convention. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let check_eq name ~expected ~actual =
  incr checks;
  if String.equal expected actual then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n    expected: %S\n    actual:   %S\n" name expected actual;
    incr failures)

let recipe operands : Isa_norm_model.syntax_recipe =
  { dialect = "gas-att"; mnemonic = "op"; operands }

let test_literal_and_operand () =
  match
    Isa_gen_render.render_line (recipe [ Isa_norm_model.Syn_operand "a" ]) ~operands:[ ("a", "x0") ]
  with
  | Ok line -> check_eq "bare operand substitutes" ~expected:"op x0" ~actual:line
  | Error msg -> check ("bare operand substitutes (error: " ^ msg ^ ")") false

let test_group_and_decorated () =
  let s =
    recipe
      [
        Isa_norm_model.Syn_decorated ("$", Isa_norm_model.Syn_operand "imm");
        Isa_norm_model.Syn_group
          [
            Isa_norm_model.Syn_operand "offset";
            Isa_norm_model.Syn_literal "(";
            Isa_norm_model.Syn_operand "base";
            Isa_norm_model.Syn_literal ")";
          ];
      ]
  in
  match
    Isa_gen_render.render_line s ~operands:[ ("imm", "1"); ("offset", "4"); ("base", "sp") ]
  with
  | Ok line -> check_eq "decorated + group render" ~expected:"op $1, 4(sp)" ~actual:line
  | Error msg -> check ("decorated + group render (error: " ^ msg ^ ")") false

let test_no_operands () =
  match Isa_gen_render.render_line (recipe []) ~operands:[] with
  | Ok line -> check_eq "no operands renders bare mnemonic" ~expected:"op" ~actual:line
  | Error msg -> check ("no operands renders bare mnemonic (error: " ^ msg ^ ")") false

let test_missing_operand_is_an_error () =
  match
    Isa_gen_render.render_line (recipe [ Isa_norm_model.Syn_operand "missing" ]) ~operands:[]
  with
  | Error _ -> check "a recipe operand with no assignment is an error" true
  | Ok _ -> check "a recipe operand with no assignment is an error" false

let test_render_source () =
  check_eq "render_source wraps in .text" ~expected:".text\nadd a0, a1, a2\n"
    ~actual:(Isa_gen_render.render_source "add a0, a1, a2")

(* sw's own frozen recipe (Isa_norm_riscv), against Isa_gen_case_build's
   riscv operand-shape convention - a real form, not just a synthetic one. *)
let sw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":25,"name":"imm12hi","width":7},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5},{"lsb":7,"name":"imm12lo","width":5}],"kind":"fixed_bits","mask":"0x707f","value":"0x2023","width_bits":32},"kind":"instruction-form","native_name":"sw","origin":{"line":20,"path":"extensions/rv_i"},"provenance":{"extension":"rv_i","operands":["imm12hi","rs1","rs2","imm12lo"],"raw":{"line":"sw     imm12hi rs1 rs2 imm12lo 14..12=2 6..2=0x08 1..0=3","tokens":["sw","imm12hi","rs1","rs2","imm12lo","14..12=2","6..2=0x08","1..0=3"]},"upstream-resolved":{"mask":"0x707f","match":"0x2023","variable_fields":["imm12hi","rs1","rs2","imm12lo"]}},"record_id":"riscv-opcodes:rv_i:sw@L20","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let test_sw_real_recipe () =
  match Isa_source_record.of_line sw_json with
  | Error msg -> check ("sw decodes (error: " ^ msg ^ ")") false
  | Ok rec_ -> (
      match Isa_norm_riscv.normalize rec_ with
      | Error _ -> check "sw normalizes" false
      | Ok (form : Isa_norm_model.form) -> (
          match
            Isa_gen_render.render_line form.syntax
              ~operands:[ ("value", "a0"); ("base", "sp"); ("offset", "8") ]
          with
          | Ok line ->
              check_eq "sw's real recipe renders with concrete operands" ~expected:"sw a0, 8(sp)"
                ~actual:line
          | Error msg ->
              check ("sw's real recipe renders with concrete operands (error: " ^ msg ^ ")") false))

let () =
  print_endline "isa-gen-render:";
  test_literal_and_operand ();
  test_group_and_decorated ();
  test_no_operands ();
  test_missing_operand_is_an_error ();
  test_render_source ();
  test_sw_real_recipe ();
  if !failures > 0 then (
    Printf.printf "isa-gen-render: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-render: all %d checks passed\n" !checks
