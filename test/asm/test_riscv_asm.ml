(* The RISC-V builders: every definition's sample encodes like its text, and those bytes read back
   as text that assembles to them again. [la] differs by design: without PIC it loads the target's
   absolute address, which the unbound encoding leaves zero and the text's bound image fills in. *)

module Ops = Riscv_family_encode.Riscv_ops

module type ENC = module type of Riscv64_encode

(* Every component a hand-written opcode needs. *)
let features =
  String.concat ","
    (List.sort_uniq compare
       (List.concat_map (fun (e : Ops.entry) -> List.map (( ^ ) "+") e.ext) (Array.to_list Ops.all)))

let text (e : Ops.entry) imm =
  let operand i : Ops.Kind.t -> string = function
    | X | Creg -> Printf.sprintf "x%d" (8 + i)
    | F -> Printf.sprintf "f%d" (8 + i)
    | V -> Printf.sprintf "v%d" (8 + i)
    | Imm -> string_of_int imm
    | Mem | Cmem -> Printf.sprintf "0(x%d)" (8 + i)
    | Spmem -> "0(sp)"
    | Sym -> "l"
    | Rm -> "rtz"
  in
  let ops = List.mapi operand e.shape in
  let m = Riscv_family_encode.Riscv_types.Opcode.mnemonic e.op in
  ".option rvc\nl:\n\t" ^ if ops = [] then m else m ^ " " ^ String.concat ", " ops

module Check (E : ENC) = struct
  module H = Harness.Make (E)

  let state =
    match Target_config.parse_spec features with
    | Ok s ->
        { (E.initial_state (Result.get_ok (Target_config.resolve E.components s))) with rvc = true }
    | Error m -> failwith m

  let encode insn =
    match E.lower_instruction state insn with
    | Error _ -> None
    | Ok ls ->
        List.fold_left
          (fun acc l ->
            Option.bind acc (fun b ->
                match E.encode_in state l with
                | Ok (`Fixed f) | Ok (`Relax (f :: _)) -> Some (b ^ f.Asm_core.Lowered_ast.bytes)
                | _ -> None))
          (Some "") ls

  (* The first immediate the sample lowers with. *)
  let sample (e : Ops.entry) =
    List.find_map
      (fun imm -> Option.map (fun b -> (imm, b)) (encode (e.sample imm)))
      [ 1; 16; 64; 4 ]

  let here (e : Ops.entry) =
    match e.xlen with Any -> true | Rv32 -> E.xlen = 32 | Rv64 -> E.xlen = 64

  let report label =
    let entries = List.filter here (Array.to_list Ops.all) in
    let name (e : Ops.entry) = E.Opcode.name e.op in
    let results =
      List.map
        (fun e ->
          match sample e with
          | None -> (e, `Unlowered)
          | Some (imm, b) -> (
              match H.assemble ~features (text e imm) with
              | Error _ -> (e, `Unassembled)
              | Ok b' when not (String.equal b b') -> (e, `Different)
              | Ok _ -> (
                  match H.decode ~features b with
                  | Error _ -> (e, `Undecoded)
                  | Ok back -> (
                      match H.assemble ~features (".option rvc\n\t" ^ back) with
                      | Ok b'' when String.equal b b'' -> (e, `Same)
                      | _ -> (e, `Not_round_trip)))))
        entries
    in
    let only k =
      String.concat " "
        (List.filter_map (fun (e, r) -> if r = k then Some (name e) else None) results)
    in
    Printf.printf
      "%s: %d builders, same %d\n\
      \  unlowered [%s]\n\
      \  unassembled [%s]\n\
      \  different [%s]\n\
      \  undecoded [%s]\n\
      \  not round trip [%s]\n"
      label (List.length entries)
      (List.length (List.filter (fun (_, r) -> r = `Same) results))
      (only `Unlowered) (only `Unassembled) (only `Different) (only `Undecoded)
      (only `Not_round_trip)
end

let%expect_test "every builder's sample encodes like its text and round-trips" =
  let module C32 = Check (Riscv32_encode) in
  let module C64 = Check (Riscv64_encode) in
  C32.report "riscv32";
  C64.report "riscv64";
  [%expect
    {|
    riscv32: 239 builders, same 238
      unlowered []
      unassembled []
      different [la]
      undecoded []
      not round trip []
    riscv64: 291 builders, same 290
      unlowered []
      unassembled []
      different [la]
      undecoded []
      not round trip [] |}]

let%expect_test "builders read like the text" =
  let module H = Harness.Make (Riscv32_encode) in
  let open Riscv_asm.Rv32 in
  List.iter
    (fun (i, t) -> H.check i t)
    [
      (add x1 x2 x3, "add x1, x2, x3");
      (lw a0 (off sp 8), "lw a0, 8(sp)");
      (addi t0 t1 (-3), "addi t0, t1, -3");
      (addi_e t0 t1 (Asm_core.Expr.Const (Foundation.Bigint.of_int 7)), "addi t0, t1, 7");
      (fcvt_w_s a0 fa0, "fcvt.w.s a0, fa0");
      (sw a1 (off s0 (-4)), "sw a1, -4(s0)");
    ];
  [%expect
    {|
    riscv32  add x1, x2, x3               b3 00 31 00              same
    riscv32  lw a0, 8(sp)                 03 25 81 00              same
    riscv32  addi t0, t1, -3              93 02 d3 ff              same
    riscv32  addi t0, t1, 7               93 02 73 00              same
    riscv32  fcvt.w.s a0, fa0             53 75 05 c0              same
    riscv32  sw a1, -4(s0)                23 2e b4 fe              same |}]
