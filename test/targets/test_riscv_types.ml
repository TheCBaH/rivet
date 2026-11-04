(* The typed RISC-V operand kinds: each value reaches the encoding and comes back from it. *)

module Types = Riscv_family_encode.Riscv_types
module Rm = Types.Rm

let%expect_test "every rounding mode round-trips through fcvt.w.s" =
  List.iter (fun rm -> Test_zicsr.round_trip "riscv64" ("fcvt.w.s a0, fa0, " ^ Rm.name rm)) Rm.all;
  [%expect
    {|
    fcvt.w.s a0, fa0, rne        c0050553 fcvt.w.s x10, f10, rne       same
    fcvt.w.s a0, fa0, rtz        c0051553 fcvt.w.s x10, f10, rtz       same
    fcvt.w.s a0, fa0, rdn        c0052553 fcvt.w.s x10, f10, rdn       same
    fcvt.w.s a0, fa0, rup        c0053553 fcvt.w.s x10, f10, rup       same
    fcvt.w.s a0, fa0, rmm        c0054553 fcvt.w.s x10, f10, rmm       same
    fcvt.w.s a0, fa0, dyn        c0057553 fcvt.w.s x10, f10            same |}]

let%expect_test "reserved rounding-mode encodings are not instructions" =
  List.iter
    (fun f3 ->
      let w = Int64.logor 0xc0050553L (Int64.of_int (f3 lsl 12)) in
      Printf.printf "funct3 %d: of_funct3 %s, %08Lx -> %s\n" f3
        (match Rm.of_funct3 f3 with Some r -> Rm.name r | None -> "none")
        w
        (match Test_zicsr.decoded "riscv64" (Test_zicsr.word_bytes w) with
        | Ok t -> t
        | Error _ -> Printf.sprintf ".word 0x%08Lx" w))
    [ 5; 6; 7 ];
  [%expect
    {|
    funct3 5: of_funct3 none, c0055553 -> .word 0xc0055553
    funct3 6: of_funct3 none, c0056553 -> .word 0xc0056553
    funct3 7: of_funct3 dyn, c0057553 -> fcvt.w.s x10, f10 |}]

(* {1 Register classes} *)

let%expect_test "register indices are range-checked by class" =
  let show = function Some _ -> "some" | None -> "none" in
  Printf.printf "Xreg 31 %s, Xreg 32 %s, Xreg -1 %s, Creg 7 %s, Creg 8 %s, Creg 15 %s, Creg 16 %s\n"
    (show (Types.Xreg.of_int 31))
    (show (Types.Xreg.of_int 32))
    (show (Types.Xreg.of_int (-1)))
    (show (Types.Creg.of_int 7))
    (show (Types.Creg.of_int 8))
    (show (Types.Creg.of_int 15))
    (show (Types.Creg.of_int 16));
  [%expect
    {| Xreg 31 some, Xreg 32 none, Xreg -1 none, Creg 7 none, Creg 8 some, Creg 15 some, Creg 16 none |}]

let%expect_test "an FP instruction given integer registers is refused" =
  let module E = Riscv64_encode in
  let x n = E.Operand.Reg (E.Reg.x n) in
  let insn = E.Instruction.mk E.Opcode.Fadd_s [ x 1; x 2; x 3 ] in
  (match E.lower_instruction E.default_state insn with
  | Ok _ -> print_endline "accepted"
  | Error e ->
      let d = E.error_diagnostic (Err.Error.kind e) in
      Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d));
  [%expect {| riscv64.lower: no fadd.s form takes these operands |}]

(* {1 Every hand-written opcode}

   Each opcode of [Opcode.all] gets the first operand list, shortest first, that it lowers to a
   single form; that form must encode, decode and re-encode to the same bytes. The auipc pairs
   decode only through the codec; they are listed. *)

module type ENC = module type of Riscv64_encode

module Sweep (E : ENC) = struct
  let state = { E.default_state with E.rvc = true }

  let atoms i =
    let n = 8 + i in
    [
      E.Operand.Reg (E.Reg.x n);
      E.Operand.Reg (E.Reg.f n);
      E.Operand.Reg (E.Reg.v n);
      E.Operand.Imm (Foundation.Bigint.of_int 1);
      E.Operand.Mem { E.Mem.offset = Asm_core.Expr.Const Foundation.Bigint.zero; base = E.Reg.x n };
      E.Operand.Mem { E.Mem.offset = Asm_core.Expr.Const Foundation.Bigint.zero; base = E.Reg.x 2 };
      E.Operand.Sym (Asm_core.Expr.Symbol "l");
    ]

  let rec shapes n =
    if n = 0 then [ [] ]
    else
      List.concat_map (fun rest -> List.map (fun a -> a :: rest) (atoms (n - 1))) (shapes (n - 1))

  let candidates = List.concat_map (fun n -> List.map List.rev (shapes n)) [ 0; 1; 2; 3; 4 ]

  let lowered op ops =
    match E.lower_instruction_ungated state (E.Instruction.mk op ops) with
    | Ok [ l ] -> (
        match E.encode_ungated l with
        | Ok (`Fixed f) -> Some (l, f.Asm_core.Lowered_ast.bytes)
        | _ -> None)
    | _ -> None

  (* The first sample of [op], as its lowered form and bytes (a fixup's field left zero). *)
  let sample op = List.find_map (lowered op) candidates

  let reencoded bytes =
    match E.decode_ungated { E.state; address = 0L } bytes ~pos:0 with
    | Ok (insn, _, len) when len = String.length bytes -> (
        match E.lower_instruction_ungated state insn with
        | Ok [ l ] -> (
            match E.encode_ungated l with
            | Ok (`Fixed f) -> Ok f.Asm_core.Lowered_ast.bytes
            | _ -> Error `Reencode)
        | _ -> Error `Relower)
    | _ -> Error `Decode

  let samples = List.map (fun op -> (op, sample op)) E.Opcode.all
end

module S64 = Sweep (Riscv64_encode)
module S32 = Sweep (Riscv32_encode)

(* Each opcode with its sample from either XLEN. *)
let samples =
  List.map2
    (fun (op, a) (_, b) ->
      ( op,
        match (a, b) with
        | Some (l, bytes), _ -> Some (`Rv64 l, bytes)
        | None, Some (l, bytes) -> Some (`Rv32 l, bytes)
        | None, None -> None ))
    S64.samples S32.samples

let%expect_test "every sampled opcode round-trips through its encoding" =
  let name = Riscv64_encode.Opcode.name in
  let unsampled =
    List.filter_map (fun (op, s) -> if s = None then Some (name op) else None) samples
  in
  let back =
    List.filter_map
      (fun (op, s) ->
        let r =
          match s with
          | Some (`Rv64 _, bytes) -> Some (bytes, S64.reencoded bytes)
          | Some (`Rv32 _, bytes) -> Some (bytes, S32.reencoded bytes)
          | None -> None
        in
        Option.map (fun (bytes, r) -> (name op, bytes, r)) r)
      samples
  in
  let names p =
    String.concat " " (List.filter_map (fun (n, b, r) -> if p b r then Some n else None) back)
  in
  Printf.printf "opcodes %d, sampled %d\nunsampled: %s\nnot decoded: %s\ndiffers: %s\n"
    (List.length samples)
    (List.length samples - List.length unsampled)
    (String.concat " " unsampled)
    (names (fun _ r -> r = Error `Decode))
    (names (fun b r -> match r with Ok b' -> b' <> b | Error `Decode -> false | Error _ -> true));
  [%expect
    {|
    opcodes 304, sampled 303
    unsampled: la
    not decoded: call tail lla
    differs: |}]

(* The hand-written opcodes the decoder had no reading of before it read them back from their own
   encodings: each round-trips on every XLEN that has it. *)
let%expect_test "formerly undecodable opcodes round-trip on both XLENs" =
  let names =
    "bclr bext binv bset bclri bexti binvi bseti clmul clmulh clmulr czero.eqz czero.nez sm3p0 \
     sm3p1 sm4ed sm4ks xperm4 xperm8 sha256sum0 sha256sum1 sha256sig0 sha256sig1 sha512sum0 \
     sha512sum1 sha512sig0 sha512sig1 sha512sum0r sha512sum1r sha512sig0l sha512sig1l sha512sig0h \
     sha512sig1h aes64ds aes64dsm aes64es aes64esm aes64ks2 aes64im aes64ks1i aes32dsi aes32dsmi \
     aes32esi aes32esmi"
  in
  let names = List.filter (( <> ) "") (String.split_on_char ' ' names) in
  let run label samples reencoded name =
    let back =
      List.filter_map
        (fun (op, s) ->
          if List.mem (name op) names then
            Option.map (fun (_, bytes) -> (name op, bytes, reencoded bytes)) s
          else None)
        samples
    in
    let failed = List.filter_map (fun (n, b, r) -> if r = Ok b then None else Some n) back in
    Printf.printf "%s: %d of %d sampled, failed [%s]\n" label (List.length back) (List.length names)
      (String.concat " " failed)
  in
  run "riscv32" S32.samples S32.reencoded Riscv32_encode.Opcode.name;
  run "riscv64" S64.samples S64.reencoded Riscv64_encode.Opcode.name;
  [%expect {|
    riscv32: 33 of 44 sampled, failed []
    riscv64: 34 of 44 sampled, failed [] |}]

(* The index only reads words no standard row matches, so no row's reading changes. *)
let%expect_test "no hand-written index entry overlaps a standard row" =
  let check (type r e) label (entries : e list) (entry : e -> string * int64 * int64)
      (rows : r list) (row : r -> string * int64 * int64) =
    let overlaps =
      List.concat_map
        (fun e ->
          let n, m, v = entry e in
          List.filter_map
            (fun r ->
              let rn, rm, rv = row r in
              if Int64.(equal (logand (logxor v rv) (logand m rm)) 0L) then Some (n ^ "/" ^ rn)
              else None)
            rows)
        entries
    in
    Printf.printf "%s: %d entries, overlaps [%s]\n" label (List.length entries)
      (String.concat " " overlaps)
  in
  let module E32 = Riscv32_encode in
  let module E64 = Riscv64_encode in
  check "riscv32" E32.By_desc.entries
    (fun e -> (E32.Opcode.name e.op, e.mask, e.match_))
    E32.Row.all
    (fun r ->
      let x = E32.Row.row r in
      (x.mnemonic, x.mask, x.match_));
  check "riscv64" E64.By_desc.entries
    (fun e -> (E64.Opcode.name e.op, e.mask, e.match_))
    E64.Row.all
    (fun r ->
      let x = E64.Row.row r in
      (x.mnemonic, x.mask, x.match_));
  [%expect {|
    riscv32: 33 entries, overlaps []
    riscv64: 34 entries, overlaps [] |}]

(* {1 Rounding modes}

   Every opcode with a rounding field, its sample word with each of the eight [funct3] values:
   the canonical text re-assembles to the same bytes, or the word is declined (a [.word]). *)

let%expect_test "every rounding field value round-trips or is a .word" =
  let module E = Riscv64_encode in
  let rounds op = Option.is_some (E.default_rm op) || Option.is_some (E.f_fma_desc op) in
  let ops = List.filter rounds E.Opcode.all in
  let words = Hashtbl.create 8 and bad = ref [] in
  List.iter
    (fun op ->
      match S64.sample op with
      | None -> bad := (E.Opcode.name op ^ ":unsampled") :: !bad
      | Some (_, bytes) ->
          let w = Test_zicsr.word_of_bytes bytes in
          for f3 = 0 to 7 do
            let w =
              Int64.logor (Int64.logand w (Int64.lognot 0x7000L)) (Int64.of_int (f3 lsl 12))
            in
            let b = Test_zicsr.word_bytes w in
            let outcome =
              match Test_zicsr.decoded "riscv64" b with
              | Error _ -> ".word"
              | Ok text -> (
                  match Test_zicsr.assembled "riscv64" text with
                  | Ok b' when String.equal b b' -> "same"
                  | Ok _ -> "different"
                  | Error _ -> "refused")
            in
            let k = (f3, outcome) in
            Hashtbl.replace words k (1 + Option.value (Hashtbl.find_opt words k) ~default:0);
            if outcome = "different" || outcome = "refused" then
              bad := Printf.sprintf "%s/%d:%s" (E.Opcode.name op) f3 outcome :: !bad
          done)
    ops;
  Printf.printf "opcodes %d\n" (List.length ops);
  for f3 = 0 to 7 do
    let n o = Option.value (Hashtbl.find_opt words (f3, o)) ~default:0 in
    Printf.printf "funct3 %d: same %d, .word %d\n" f3 (n "same") (n ".word")
  done;
  Printf.printf "bad [%s]\n" (String.concat " " (List.rev !bad));
  [%expect
    {|
    opcodes 36
    funct3 0: same 36, .word 0
    funct3 1: same 36, .word 0
    funct3 2: same 36, .word 0
    funct3 3: same 36, .word 0
    funct3 4: same 36, .word 0
    funct3 5: same 0, .word 36
    funct3 6: same 0, .word 36
    funct3 7: same 36, .word 0
    bad [] |}]

(* {1 Derived encoding fields} *)

let%expect_test "no lowering reaches an opcode without fields for its format" =
  let module E = Riscv64_encode in
  let short = List.filter (fun ops -> List.length ops <= 3) S64.candidates in
  let mismatched =
    List.filter_map
      (fun op ->
        let bad ops =
          match E.lower_instruction_ungated S64.state (E.Instruction.mk op ops) with
          | Ok ls ->
              List.exists
                (fun l ->
                  match E.encode_ungated l with
                  | Error e -> (
                      match Err.Error.kind e |> Target_error.kind with
                      | `Format_mismatch _ -> true
                      | _ -> false)
                  | Ok _ -> false)
                ls
          | Error _ -> false
        in
        if List.exists bad short then Some (E.Opcode.name op) else None)
      E.Opcode.all
  in
  Printf.printf "mismatched: [%s]\n" (String.concat " " mismatched);
  [%expect {| mismatched: [] |}]

let%expect_test "a form paired with an opcode of another format is refused" =
  let module E = Riscv64_encode in
  let l =
    E.Lowered.R
      { op = E.Opcode.Lui; rd = E.Reg.x 1; rs1 = E.Reg.x 2; rs2 = Some (E.Reg.x 3); rm = None }
  in
  (match E.encode_ungated l with
  | Ok _ -> print_endline "encoded"
  | Error e ->
      let d = E.error_diagnostic (Err.Error.kind e) in
      Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d));
  [%expect {| riscv64.internal.format: internal: lui has no R-format encoding |}]

(* {1 Named standard rows} *)

(* A name whose rows are all for the other XLEN still names one of them; [Opcode.of_mnemonic]
   has no reading of it here. *)
let%expect_test "every standard row name is its row's spelling" =
  let check (type a) name all (mnemonic : a -> string) (of_mnemonic : string -> a option) =
    let other = ref 0 in
    let wrong =
      List.filter_map
        (fun (m, r) ->
          match of_mnemonic m with
          | _ when not (String.equal (mnemonic r) m) -> Some m
          | Some r' when r' = r -> None
          | Some _ -> Some m
          | None ->
              incr other;
              None)
        all
    in
    Printf.printf "%s: %d names, %d only for the other XLEN, wrong [%s]\n" name (List.length all)
      !other (String.concat " " wrong)
  in
  let rows32 m =
    match Riscv32_encode.Opcode.of_mnemonic m with
    | Some (Riscv32_encode.Opcode.Table r) -> Some r
    | _ -> None
  in
  let rows64 m =
    match Riscv64_encode.Opcode.of_mnemonic m with
    | Some (Riscv64_encode.Opcode.Table r) -> Some r
    | _ -> None
  in
  check "riscv32" Riscv32_encode.Std_row.all Riscv32_encode.Row.mnemonic rows32;
  check "riscv64" Riscv64_encode.Std_row.all Riscv64_encode.Row.mnemonic rows64;
  [%expect
    {|
    riscv32: 1034 names, 24 only for the other XLEN, wrong []
    riscv64: 1034 names, 9 only for the other XLEN, wrong [] |}]

(* {1 Spelling lookup} *)

module Lookup (E : ENC) = struct
  (* The linear search [Opcode.of_mnemonic] did before its table. *)
  let linear s =
    let s = Option.value (List.assoc_opt s E.Opcode.deprecated_mnemonic_aliases) ~default:s in
    match List.find_opt (fun op -> String.equal (E.Opcode.name op) s) E.Opcode.all with
    | Some _ as op -> op
    | None ->
        List.find_map
          (fun r ->
            let row = E.Row.row r in
            if
              String.equal row.Riscv_family_encode.Riscv_table_row.mnemonic s
              && (row.xlen = 0 || row.xlen = E.xlen)
            then Some (E.Opcode.Table r)
            else None)
          E.Row.all

  let check label =
    let spellings =
      List.map E.Opcode.name E.Opcode.all
      @ List.map E.Row.mnemonic E.Row.all
      @ List.concat_map (fun (a, c) -> [ a; c ]) E.Opcode.deprecated_mnemonic_aliases
      @ [ ""; "nope"; "ADD" ]
    in
    let differ = List.filter (fun s -> E.Opcode.of_mnemonic s <> linear s) spellings in
    Printf.printf "%s: %d spellings, %d differ [%s]\n" label (List.length spellings)
      (List.length differ) (String.concat " " differ)
end

let%expect_test "the spelling table reads every spelling as the linear search did" =
  let module L32 = Lookup (Riscv32_encode) in
  let module L64 = Lookup (Riscv64_encode) in
  L32.check "riscv32";
  L64.check "riscv64";
  [%expect {|
    riscv32: 1372 spellings, 0 differ []
    riscv64: 1372 spellings, 0 differ [] |}]

(* {1 Instruction class} *)

module Cls = Types.Cls

let%expect_test "one word per class and compressed quadrant" =
  List.iter
    (fun (xlen, w, what) ->
      Printf.printf "%-10s %08Lx %s\n" what w (Cls.name (Cls.of_word ~xlen w)))
    [
      (64, 0x00000013L, "addi");
      (64, 0x00002003L, "lw");
      (64, 0x00002007L, "flw");
      (64, 0x00000007L, "vle8.v");
      (64, 0x00002023L, "sw");
      (64, 0x00002027L, "fsw");
      (64, 0x00007027L, "vse64.v");
      (64, 0x0000202fL, "amoadd.w");
      (64, 0x00000063L, "beq");
      (64, 0x0000006fL, "jal");
      (64, 0x00000067L, "jalr");
      (64, 0x00001073L, "csrrw");
      (64, 0x00000073L, "ecall");
      (64, 0x0000000fL, "fence");
      (64, 0x00000053L, "fadd.s");
      (64, 0x00000043L, "fmadd.s");
      (64, 0x00000057L, "vadd.vv");
      (64, 0x00000077L, "vaesz.vs");
      (64, 0x0000000bL, "custom-0");
      (64, 0x0000007bL, "custom-3");
      (64, 0x0040L, "c.addi4spn");
      (64, 0x4000L, "c.lw");
      (64, 0x8000L, "c.lbu");
      (64, 0x8800L, "c.sb");
      (64, 0xc000L, "c.sw");
      (64, 0x0001L, "c.nop");
      (32, 0x2001L, "c.jal");
      (64, 0x2001L, "c.addiw");
      (64, 0xa001L, "c.j");
      (64, 0xc001L, "c.beqz");
      (64, 0x0002L, "c.slli");
      (64, 0x4002L, "c.lwsp");
      (64, 0x8082L, "c.jr");
      (64, 0x9082L, "c.jalr");
      (64, 0x8086L, "c.mv");
      (64, 0x9002L, "c.ebreak");
      (64, 0xc002L, "c.swsp");
    ];
  [%expect
    {|
    addi       00000013 alu
    lw         00002003 load
    flw        00002007 load
    vle8.v     00000007 vector-load
    sw         00002023 store
    fsw        00002027 store
    vse64.v    00007027 vector-store
    amoadd.w   0000202f atomic
    beq        00000063 branch
    jal        0000006f jump
    jalr       00000067 jump
    csrrw      00001073 csr
    ecall      00000073 system
    fence      0000000f system
    fadd.s     00000053 fp
    fmadd.s    00000043 fp
    vadd.vv    00000057 vector
    vaesz.vs   00000077 vector
    custom-0   0000000b custom
    custom-3   0000007b custom
    c.addi4spn 00000040 alu
    c.lw       00004000 load
    c.lbu      00008000 load
    c.sb       00008800 store
    c.sw       0000c000 store
    c.nop      00000001 alu
    c.jal      00002001 jump
    c.addiw    00002001 alu
    c.j        0000a001 jump
    c.beqz     0000c001 branch
    c.slli     00000002 alu
    c.lwsp     00004002 load
    c.jr       00008082 jump
    c.jalr     00009082 jump
    c.mv       00008086 alu
    c.ebreak   00009002 system
    c.swsp     0000c002 store |}]

let%expect_test "no standard row is custom" =
  let custom (type a) (all : a list) cls mnemonic =
    List.filter_map (fun r -> if cls r = Cls.Custom then Some (mnemonic r) else None) all
  in
  Printf.printf "riscv32 [%s]\nriscv64 [%s]\n"
    (String.concat " "
       (custom Riscv32_encode.Row.all Riscv32_encode.Row.cls Riscv32_encode.Row.mnemonic))
    (String.concat " "
       (custom Riscv64_encode.Row.all Riscv64_encode.Row.cls Riscv64_encode.Row.mnemonic));
  [%expect {|
    riscv32 []
    riscv64 [] |}]

(* The word an encoding does its work in: the second of an auipc pair. *)
let last_word bytes =
  let n = String.length bytes in
  let at = if n = 8 then 4 else 0 in
  let len = if n = 2 then 2 else 4 in
  let v = ref 0L in
  for i = len - 1 downto 0 do
    v := Int64.logor (Int64.shift_left !v 8) (Int64.of_int (Char.code bytes.[at + i]))
  done;
  !v

let%expect_test "a hand-written opcode's class is its encoding's" =
  let differ =
    List.filter_map
      (fun (op, s) ->
        let xlen, cls =
          match s with
          | Some (`Rv64 _, _) -> (64, Riscv64_encode.Cls.of_opcode op)
          | Some (`Rv32 _, _) -> (32, Riscv32_encode.Cls.of_opcode op)
          | None -> (0, Cls.Alu)
        in
        match s with
        | Some (_, bytes) ->
            let by_word = Cls.of_word ~xlen (last_word bytes) in
            if by_word = cls then None
            else
              Some
                (Printf.sprintf "%s:%s/%s" (Riscv64_encode.Opcode.name op) (Cls.name cls)
                   (Cls.name by_word))
        | None -> None)
      samples
  in
  Printf.printf "differ [%s]\n" (String.concat " " differ);
  [%expect {| differ [] |}]

(* {1 Error payloads} *)

let%expect_test "lowering errors name their opcode" =
  let module E = Riscv32_encode in
  let x n = E.Operand.Reg (E.Reg.x n) in
  let kind insn =
    match E.lower_instruction E.default_state insn with
    | Ok _ -> "accepted"
    | Error e -> (
        match Target_error.kind (Err.Error.kind e) with
        | `Rv64_only E.Opcode.Ld -> "Rv64_only Ld"
        | `Wrong_operands E.Opcode.Add -> "Wrong_operands Add"
        | _ -> "other")
  in
  print_endline (kind (E.Instruction.mk E.Opcode.Ld [ x 1; x 2 ]));
  print_endline (kind (E.Instruction.mk E.Opcode.Add [ x 1 ]));
  [%expect {|
    Rv64_only Ld
    Wrong_operands Add |}]

(* {1 Register names and classes} *)

module R = Types.Reg

(* Every register value by its spelling. *)
let named =
  [
    ("x0", R.x0);
    ("x1", R.x1);
    ("x2", R.x2);
    ("x3", R.x3);
    ("x4", R.x4);
    ("x5", R.x5);
    ("x6", R.x6);
    ("x7", R.x7);
    ("x8", R.x8);
    ("x9", R.x9);
    ("x10", R.x10);
    ("x11", R.x11);
    ("x12", R.x12);
    ("x13", R.x13);
    ("x14", R.x14);
    ("x15", R.x15);
    ("x16", R.x16);
    ("x17", R.x17);
    ("x18", R.x18);
    ("x19", R.x19);
    ("x20", R.x20);
    ("x21", R.x21);
    ("x22", R.x22);
    ("x23", R.x23);
    ("x24", R.x24);
    ("x25", R.x25);
    ("x26", R.x26);
    ("x27", R.x27);
    ("x28", R.x28);
    ("x29", R.x29);
    ("x30", R.x30);
    ("x31", R.x31);
    ("zero", R.zero);
    ("ra", R.ra);
    ("sp", R.sp);
    ("gp", R.gp);
    ("tp", R.tp);
    ("t0", R.t0);
    ("t1", R.t1);
    ("t2", R.t2);
    ("s0", R.s0);
    ("fp", R.fp);
    ("s1", R.s1);
    ("a0", R.a0);
    ("a1", R.a1);
    ("a2", R.a2);
    ("a3", R.a3);
    ("a4", R.a4);
    ("a5", R.a5);
    ("a6", R.a6);
    ("a7", R.a7);
    ("s2", R.s2);
    ("s3", R.s3);
    ("s4", R.s4);
    ("s5", R.s5);
    ("s6", R.s6);
    ("s7", R.s7);
    ("s8", R.s8);
    ("s9", R.s9);
    ("s10", R.s10);
    ("s11", R.s11);
    ("t3", R.t3);
    ("t4", R.t4);
    ("t5", R.t5);
    ("t6", R.t6);
    ("f0", R.f0);
    ("f1", R.f1);
    ("f2", R.f2);
    ("f3", R.f3);
    ("f4", R.f4);
    ("f5", R.f5);
    ("f6", R.f6);
    ("f7", R.f7);
    ("f8", R.f8);
    ("f9", R.f9);
    ("f10", R.f10);
    ("f11", R.f11);
    ("f12", R.f12);
    ("f13", R.f13);
    ("f14", R.f14);
    ("f15", R.f15);
    ("f16", R.f16);
    ("f17", R.f17);
    ("f18", R.f18);
    ("f19", R.f19);
    ("f20", R.f20);
    ("f21", R.f21);
    ("f22", R.f22);
    ("f23", R.f23);
    ("f24", R.f24);
    ("f25", R.f25);
    ("f26", R.f26);
    ("f27", R.f27);
    ("f28", R.f28);
    ("f29", R.f29);
    ("f30", R.f30);
    ("f31", R.f31);
    ("ft0", R.ft0);
    ("ft1", R.ft1);
    ("ft2", R.ft2);
    ("ft3", R.ft3);
    ("ft4", R.ft4);
    ("ft5", R.ft5);
    ("ft6", R.ft6);
    ("ft7", R.ft7);
    ("fs0", R.fs0);
    ("fs1", R.fs1);
    ("fa0", R.fa0);
    ("fa1", R.fa1);
    ("fa2", R.fa2);
    ("fa3", R.fa3);
    ("fa4", R.fa4);
    ("fa5", R.fa5);
    ("fa6", R.fa6);
    ("fa7", R.fa7);
    ("fs2", R.fs2);
    ("fs3", R.fs3);
    ("fs4", R.fs4);
    ("fs5", R.fs5);
    ("fs6", R.fs6);
    ("fs7", R.fs7);
    ("fs8", R.fs8);
    ("fs9", R.fs9);
    ("fs10", R.fs10);
    ("fs11", R.fs11);
    ("ft8", R.ft8);
    ("ft9", R.ft9);
    ("ft10", R.ft10);
    ("ft11", R.ft11);
    ("v0", R.v0);
    ("v1", R.v1);
    ("v2", R.v2);
    ("v3", R.v3);
    ("v4", R.v4);
    ("v5", R.v5);
    ("v6", R.v6);
    ("v7", R.v7);
    ("v8", R.v8);
    ("v9", R.v9);
    ("v10", R.v10);
    ("v11", R.v11);
    ("v12", R.v12);
    ("v13", R.v13);
    ("v14", R.v14);
    ("v15", R.v15);
    ("v16", R.v16);
    ("v17", R.v17);
    ("v18", R.v18);
    ("v19", R.v19);
    ("v20", R.v20);
    ("v21", R.v21);
    ("v22", R.v22);
    ("v23", R.v23);
    ("v24", R.v24);
    ("v25", R.v25);
    ("v26", R.v26);
    ("v27", R.v27);
    ("v28", R.v28);
    ("v29", R.v29);
    ("v30", R.v30);
    ("v31", R.v31);
  ]

let%expect_test "every register name is the parser's, and every parser spelling has a name" =
  let wrong = List.filter_map (fun (n, r) -> if R.find n = Some r then None else Some n) named in
  let spellings =
    List.concat_map (fun p -> List.init 32 (fun i -> p ^ string_of_int i)) [ "x"; "f"; "v" ]
    @ List.map fst R.aliases @ List.map fst R.f_aliases
  in
  let unnamed = List.filter (fun s -> not (List.mem_assoc s named)) spellings in
  Printf.printf "names %d, wrong [%s], unnamed [%s]\n" (List.length named) (String.concat " " wrong)
    (String.concat " " unnamed);
  [%expect {| names 161, wrong [], unnamed [] |}]

module Laws (C : Types.INDEX) = struct
  let check label =
    let some = List.filter (fun n -> C.of_int n <> None) (List.init 66 (fun i -> i - 1)) in
    let ok =
      [
        ("of_int", some = C.numbers);
        ("all", List.map C.to_int C.all = C.numbers);
        ("round trip", List.for_all (fun n -> C.to_int (C.of_int_exn n) = n) C.numbers);
        ("count", C.count = List.length C.numbers);
        ( "mem",
          List.for_all (fun n -> C.mem n = List.mem n C.numbers) (List.init 66 (fun i -> i - 1)) );
        ( "range",
          match C.range with
          | Some (lo, hi) -> C.numbers = List.init (hi - lo + 1) (fun i -> lo + i)
          | None -> false );
      ]
    in
    Printf.printf "%-5s %d..%d %s, failed [%s]\n" label (List.hd C.numbers)
      (List.nth C.numbers (C.count - 1))
      (C.name (List.hd C.all))
      (String.concat " " (List.filter_map (fun (n, b) -> if b then None else Some n) ok))
end

let%expect_test "each register class keeps the REGS laws" =
  let module X = Laws (Types.Xreg) in
  let module F = Laws (Types.Freg) in
  let module V = Laws (Types.Vreg) in
  let module C = Laws (Types.Creg) in
  let module CF = Laws (Types.Cfreg) in
  X.check "Xreg";
  F.check "Freg";
  V.check "Vreg";
  C.check "Creg";
  CF.check "Cfreg";
  [%expect
    {|
    Xreg  0..31 x0, failed []
    Freg  0..31 f0, failed []
    Vreg  0..31 v0, failed []
    Creg  8..15 x8, failed []
    Cfreg 8..15 f8, failed [] |}]

let%expect_test "every register of a class assembles in an instruction of that class" =
  let count spell numbers =
    List.length
      (List.filter (fun n -> Result.is_ok (Test_zicsr.assembled "riscv64" (spell n))) numbers)
  in
  let x n = Printf.sprintf "add x%d, x%d, x%d" n n n in
  let f n = Printf.sprintf "fadd.d f%d, f%d, f%d" n n n in
  let c n = Printf.sprintf ".option rvc\n\tc.and x%d, x%d" n n in
  let cf n = Printf.sprintf ".option rvc\n\tc.fld f%d, 0(x8)" n in
  Printf.printf "Xreg %d/32, Freg %d/32, Creg %d/8, Cfreg %d/8\n" (count x Types.Xreg.numbers)
    (count f Types.Freg.numbers) (count c Types.Creg.numbers) (count cf Types.Cfreg.numbers);
  [%expect {| Xreg 32/32, Freg 32/32, Creg 8/8, Cfreg 8/8 |}]

(* {1 One instruction type} *)

let%expect_test "every instance and the builders share one Instruction.t" =
  let module Ops = Riscv_family_encode.Riscv_ops in
  let built : Types.Instruction.t = Ops.add Types.Xreg.a0 Types.Xreg.a1 Types.Xreg.a2 in
  let as64 (i : Riscv32_encode.Instruction.t) : Riscv64_encode.Instruction.t = i in
  let bytes32 =
    match Riscv32_encode.lower_instruction Riscv32_encode.default_state built with
    | Ok [ l ] -> (
        match Riscv32_encode.encode l with
        | Ok (`Fixed f) -> Test_zicsr.word_of_bytes f.Asm_core.Lowered_ast.bytes
        | _ -> 0L)
    | _ -> 0L
  in
  let bytes64 =
    match Riscv64_encode.lower_instruction Riscv64_encode.default_state (as64 built) with
    | Ok [ l ] -> (
        match Riscv64_encode.encode l with
        | Ok (`Fixed f) -> Test_zicsr.word_of_bytes f.Asm_core.Lowered_ast.bytes
        | _ -> 0L)
    | _ -> 0L
  in
  Printf.printf "%s: riscv32 %08Lx, riscv64 %08Lx\n"
    (Fmt.to_to_string Riscv32_encode.Instruction.pp built)
    bytes32 bytes64;
  [%expect {| add x10, x11, x12: riscv32 00c58533, riscv64 00c58533 |}]
