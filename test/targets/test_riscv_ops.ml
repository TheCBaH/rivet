(* Riscv_ops agrees with the encoder's own tables on every hand-written opcode, both XLENs:
   encoding fields, XLEN, class, components, rounding default and the operand lists lowering
   takes. *)

module Ops = Riscv_family_encode.Riscv_ops

module type ENC = module type of Riscv64_encode

module Check (E : ENC) = struct
  let state = { E.default_state with E.rvc = true }

  (* The operand kinds lowering is tried with; a compressed register or memory operand is an x8
     register or a memory operand based on one. *)
  let atom_of_kind : Ops.Kind.t -> string = function
    | Creg -> "x"
    | Cmem -> "mem"
    | k -> Ops.Kind.name k

  let atoms = [ "x"; "f"; "v"; "imm"; "mem"; "spmem"; "sym"; "rm" ]

  let operand i imm = function
    | "x" -> E.Operand.Reg (E.Reg.x (8 + i))
    | "f" -> E.Operand.Reg (E.Reg.f (8 + i))
    | "v" -> E.Operand.Reg (E.Reg.v (8 + i))
    | "imm" -> E.Operand.Imm (Foundation.Bigint.of_int imm)
    | "mem" ->
        E.Operand.Mem
          { E.Mem.offset = Asm_core.Expr.Const Foundation.Bigint.zero; base = E.Reg.x (8 + i) }
    | "spmem" ->
        E.Operand.Mem
          { E.Mem.offset = Asm_core.Expr.Const Foundation.Bigint.zero; base = E.Reg.x 2 }
    | "sym" -> E.Operand.Sym (Asm_core.Expr.Symbol "l")
    | _ -> E.Operand.Sym (Asm_core.Expr.Symbol "rtz")

  let rec lists n =
    if n = 0 then [ [] ]
    else List.concat_map (fun rest -> List.map (fun k -> k :: rest) atoms) (lists (n - 1))

  let all_lists = List.concat_map (fun n -> List.map List.rev (lists n)) [ 0; 1; 2; 3; 4 ]

  let lower op l imm =
    E.lower_instruction_ungated state
      (E.Instruction.mk op (List.mapi (fun i k -> operand i imm k) l))

  let lowers op l = List.exists (fun imm -> Result.is_ok (lower op l imm)) [ 1; 16; 64; 4 ]

  (* A trailing [rm] where a symbol is also taken is the same operand list. *)
  let accepted op =
    let acc = List.filter (lowers op) all_lists in
    List.filter
      (fun l ->
        not
          (List.exists
             (fun i ->
               List.nth l i = "rm"
               && List.mem (List.mapi (fun j k -> if j = i then "sym" else k) l) acc)
             (List.init (List.length l) Fun.id)))
      acc

  let gated op =
    List.find_map
      (fun l ->
        match lower op l 1 with
        | Error e -> (
            match Target_error.kind (Err.Error.kind e) with
            | `Rv64_only _ -> Some Ops.Rv64
            | `Rv32_only _ -> Some Ops.Rv32
            | _ -> None)
        | Ok _ -> None)
      all_lists

  let encoder_enc op =
    let i = function None -> "-" | Some n -> string_of_int n in
    match
      ( E.Desc.r op,
        E.Desc.rbs op,
        E.Desc.r4 op,
        E.unary_imm_desc op,
        E.Desc.i op,
        E.Desc.s op,
        E.Desc.b op,
        E.Desc.u op,
        E.Desc.ca op,
        E.Desc.cr op,
        E.Desc.c_funct3 op )
    with
    | Some d, _, _, _, _, _, _, _, _, _, _ ->
        Printf.sprintf "R %d %d %d %s" d.opcode d.funct3 d.funct7 (i d.rs2)
    | _, Some f7, _, _, _, _, _, _, _, _, _ -> Printf.sprintf "Rbs %d" f7
    | _, _, Some (o, fmt), _, _, _, _, _, _, _, _ -> Printf.sprintf "R4 %d %d" o fmt
    | _, _, _, Some (o, f3, f12), _, _, _, _, _, _, _ -> Printf.sprintf "Unary %d %d %d" o f3 f12
    | _, _, _, _, Some d, _, _, _, _, _, _ ->
        Printf.sprintf "I %d %d %d %s" d.opcode d.funct3 d.funct_hi (i d.shamt_bits)
    | _, _, _, _, _, Some (o, f3), _, _, _, _, _ -> Printf.sprintf "S %d %d" o f3
    | _, _, _, _, _, _, Some f3, _, _, _, _ -> Printf.sprintf "B %d" f3
    | _, _, _, _, _, _, _, Some o, _, _, _ -> Printf.sprintf "U %d" o
    | _, _, _, _, _, _, _, _, Some (f6, f2), _, _ -> Printf.sprintf "Ca %d %d" f6 f2
    | _, _, _, _, _, _, _, _, _, Some f4, _ -> Printf.sprintf "Cr %d" f4
    | _, _, _, _, _, _, _, _, _, _, Some f3 -> Printf.sprintf "C %d" f3
    | _ -> "-"

  let table_enc (e : Ops.entry) =
    let enc = if E.xlen = 64 && Option.is_some e.enc64 then e.enc64 else e.enc in
    match enc with
    | None -> "-"
    | Some (R { opcode; funct3; funct7; rs2 }) ->
        Printf.sprintf "R %d %d %d %s" opcode funct3 funct7
          (match rs2 with None -> "-" | Some n -> string_of_int n)
    | Some (Rbs { funct7 }) -> Printf.sprintf "Rbs %d" funct7
    | Some (R4 { opcode; fmt }) -> Printf.sprintf "R4 %d %d" opcode fmt
    | Some (Unary { opcode; funct3; funct12 }) ->
        Printf.sprintf "Unary %d %d %d" opcode funct3 funct12
    | Some (I { opcode; funct3 }) -> Printf.sprintf "I %d %d 0 -" opcode funct3
    | Some (Shift { funct3; funct6 }) ->
        Printf.sprintf "I 19 %d %d %d" funct3 (if E.xlen = 64 then funct6 else funct6 * 2) E.xlen
    | Some (Shiftw { funct3; funct7 }) -> Printf.sprintf "I 27 %d %d 32" funct3 funct7
    | Some (S { opcode; funct3 }) -> Printf.sprintf "S %d %d" opcode funct3
    | Some (B { funct3 }) -> Printf.sprintf "B %d" funct3
    | Some (U { opcode }) -> Printf.sprintf "U %d" opcode
    | Some (Ca { funct6; funct2 }) -> Printf.sprintf "Ca %d %d" funct6 funct2
    | Some (Cr { funct4 }) -> Printf.sprintf "Cr %d" funct4
    | Some (C { funct3 }) -> Printf.sprintf "C %d" funct3

  let exts op =
    let name = E.Opcode.name op in
    List.filter_map
      (fun (c : Target_component.t) ->
        if List.mem name (Target_component.mnemonics c) then Some c.feature else None)
      E.components

  let here (e : Ops.entry) =
    match e.xlen with Any -> true | Rv32 -> E.xlen = 32 | Rv64 -> E.xlen = 64

  (* Each disagreement as [opcode:what]. *)
  let disagreements () =
    let ops = Array.to_list Ops.all in
    let order =
      if List.map (fun (e : Ops.entry) -> e.op) ops = E.Opcode.all then [] else [ "order" ]
    in
    order
    @ List.concat_map
        (fun (e : Ops.entry) ->
          let op = e.op in
          let bad what ok = if ok then [] else [ E.Opcode.name op ^ ":" ^ what ] in
          let gate = Option.value (gated op) ~default:Ops.Any in
          let fp_ok =
            match (E.f_shape op, e.shape) with
            | None, _ -> e.rm = None
            | Some s, k0 :: k1 :: _ -> s.rm = e.rm && s.rd_f = (k0 = F) && s.rs1_f = (k1 = F)
            | Some _, _ -> false
          in
          let shapes = List.map (List.map atom_of_kind) (e.shape :: e.also) in
          bad "xlen" (if here e then gate = Any || gate = e.xlen else gate <> Any)
          @
          if not (here e) then []
          else
            bad "enc" (encoder_enc op = table_enc e)
            @ bad "cls" (E.Cls.of_opcode op = e.cls)
            @ bad "ext" (exts op = e.ext)
            @ bad "fp" fp_ok
            @ bad "shapes" (accepted op = shapes))
        ops
end

let%expect_test "Riscv_ops agrees with the encoder" =
  let module C32 = Check (Riscv32_encode) in
  let module C64 = Check (Riscv64_encode) in
  Printf.printf "entries %d\nriscv32 [%s]\nriscv64 [%s]\n" (Array.length Ops.all)
    (String.concat " " (C32.disagreements ()))
    (String.concat " " (C64.disagreements ()));
  [%expect {|
    entries 304
    riscv32 []
    riscv64 [] |}]
