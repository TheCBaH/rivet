module E = I8080_encode
module Ops = I8080_ops
module S = I8080_sem.Runnable (Compute.Concrete)

let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let render e = Foundation.Diagnostic.render_all (Foundation.Diag.diagnostics e)
let text i = Format.asprintf "%a" E.Instruction.pp i

let built ?origin items =
  match I8080_asm.Program.assemble ?origin items with
  | Ok img -> img
  | Error e -> failwith (render e)

let show ?origin items =
  match I8080_asm.Program.assemble ?origin items with
  | Ok { I8080_asm.Program.origin; bytes; _ } -> Printf.sprintf "%04x: %s" origin (hex bytes)
  | Error e -> "REFUSED " ^ render e

let mnemonic = function E.Instruction.Insn { entry; _ } -> entry.Ops.mnemonic | _ -> "?"

(* Every instruction of the corpus, built, assembles to the bytes of its printed text. *)
let%expect_test "every builder shape encodes like its text counterpart" =
  let bad = ref [] in
  List.iter
    (fun i ->
      match (I8080_asm.Program.assemble [ I8080_asm.insn i ], I8080_text.assemble (text i)) with
      | Ok b, Ok t ->
          if b.I8080_asm.Program.bytes <> t.I8080_text.bytes then
            bad :=
              Printf.sprintf "%s: %s vs %s" (text i) (hex b.bytes) (hex t.I8080_text.bytes) :: !bad
      | Error e, _ | _, Error e -> bad := Printf.sprintf "%s: %s" (text i) (render e) :: !bad)
    I8080_corpus.all;
  Printf.printf "cases=%d mismatches=%d\n" (List.length I8080_corpus.all) (List.length !bad);
  List.iter print_endline (List.rev !bad);
  [%expect {| cases=614 mismatches=0 |}]

let%expect_test "the corpus has a builder for every documented instruction" =
  let have = List.sort_uniq compare (List.map mnemonic I8080_corpus.all) in
  let table = List.sort_uniq compare (List.map (fun e -> e.Ops.mnemonic) Ops.entries) in
  Printf.printf "builders=%d entries=%d missing=[%s] extra=[%s]\n" (List.length have)
    (List.length table)
    (String.concat " " (List.filter (fun m -> not (List.mem m have)) table))
    (String.concat " " (List.filter (fun m -> not (List.mem m table)) have));
  [%expect {| builders=78 entries=78 missing=[] extra=[] |}]

(* A symbol where an immediate goes assembles like the text naming a label there. *)
let%expect_test "expression builders resolve a label like text does" =
  let bad = ref 0 in
  List.iter
    (fun i ->
      let b = built [ I8080_asm.insn i; I8080_asm.label "t" ] in
      let t =
        match I8080_text.assemble (text i ^ "\nt:\n") with
        | Ok t -> t.I8080_text.bytes
        | Error e -> failwith (render e)
      in
      if b.I8080_asm.Program.bytes <> t then (
        incr bad;
        Printf.printf "%s: %s vs %s\n" (text i) (hex b.bytes) (hex t)))
    I8080_corpus.symbolic;
  Printf.printf "cases=%d mismatches=%d\n" (List.length I8080_corpus.symbolic) !bad;
  [%expect {| cases=44 mismatches=0 |}]

let%expect_test "the example program" =
  let open I8080_asm in
  print_endline (show [ insn (lxi sp 0xf000); insn (mvi a 42); insn (sta 0x8000); insn (hlt ()) ]);
  print_endline
    (show ~origin:0x100
       [
         insn (lxi sp 0xf000);
         insn (mvi b 3);
         label "loop";
         insn (dcr b);
         insn (jnz_e (symbol "loop"));
         insn (hlt ());
       ]);
  [%expect {|
    0000: 31 00 f0 3e 2a 32 00 80 76
    0100: 31 00 f0 06 03 05 c2 05 01 76 |}]

let%expect_test "immediate and address boundaries" =
  let open I8080_asm in
  List.iter
    (fun i -> print_endline (show [ insn i ]))
    [
      mvi a 0;
      mvi a 255;
      mvi a (-128);
      lxi hl 0xffff;
      lxi hl (-32768);
      lda 0xffff;
      jmp 0;
      out 255;
      in_ 0;
      rst 7;
      adi_e (plus (const 200) (const 55));
      lxi_e sp (minus (const 0x10000) (const 1));
    ];
  [%expect
    {|
    0000: 3e 00
    0000: 3e ff
    0000: 3e 80
    0000: 21 ff ff
    0000: 21 00 80
    0000: 3a ff ff
    0000: c3 00 00
    0000: d3 ff
    0000: db 00
    0000: ff
    0000: c6 ff
    0000: 31 ff ff |}]

let%expect_test "diagnostics for builders" =
  let open I8080_asm in
  List.iter
    (fun items -> print_endline (show items))
    [
      [ insn (mov m m) ];
      [ insn (mvi a 256) ];
      [ insn (mvi a (-129)) ];
      [ insn (lxi hl 0x10000) ];
      [ insn (lxi hl (-32769)) ];
      [ insn (rst 8) ];
      [ insn (rst (-1)) ];
      [ insn (out 256) ];
      [ insn (jmp_e (symbol "nowhere")) ];
      [ label "x"; insn (nop ()); label "x" ];
      [ db [ 256 ] ];
      [ dw [ 65536 ] ];
      [ space (-1) ];
      [ insn (adi_e (const 300)) ];
    ];
  [%expect
    {|
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: no mov form takes these operands
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.lower]: mvi operand 256 does not fit -128..255
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.lower]: mvi operand -129 does not fit -128..255
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.lower]: lxi operand 65536 does not fit -32768..65535
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.lower]: lxi operand -32769 does not fit -32768..65535
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: rst: operand 1 must be 0..7
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: rst: operand 1 must be 0..7
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.lower]: out operand 256 does not fit -128..255
    REFUSED <synthesized by i8080.encode>: error[image.undefined]: fixup imm references undefined symbol nowhere
    REFUSED <synthesized by image>: error[image.duplicate]: duplicate definition of x
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.lower]: db value 256 does not fit -128..255
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.lower]: dw value 65536 does not fit -32768..65535
    REFUSED <synthesized by i8080>: error[i8080.lower]: space -1 does not fit 0..65535
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.lower]: adi operand 300 does not fit -128..255 |}]

(* The older public AST: a generic instruction with the wrong operands. *)
let%expect_test "malformed generic instructions are diagnostics, not exceptions" =
  let open E in
  let entry n = Option.get (Ops.find_entry n) in
  let imm n = Operand.Imm (Foundation.Bigint.of_int n) in
  let generic n args = I8080_asm.insn (Instruction.Insn { entry = entry n; args }) in
  List.iter
    (fun items ->
      match show items with
      | s -> print_endline s
      | exception ex -> print_endline ("EXCEPTION " ^ Printexc.to_string ex))
    [
      [ generic "mov" [ Operand.Reg Reg.A ] ];
      [ generic "mov" [] ];
      [ generic "nop" [ imm 1 ] ];
      [ generic "mvi" [ imm 1; imm 2 ] ];
      [ generic "mvi" [ Operand.Reg Reg.A; Operand.Reg Reg.B ] ];
      [ generic "lxi" [ Operand.Psw; imm 0 ] ];
      [ generic "push" [ Operand.Sp ] ];
      [ generic "ldax" [ Operand.Reg Reg.H ] ];
      [ generic "rst" [ imm 9 ] ];
      [ generic "rst" [ Operand.Sym (Asm_core.Expr.Symbol "x") ] ];
      [ generic "jmp" [ Operand.Str "x" ] ];
      [ generic "mov" [ Operand.Reg Reg.M; Operand.Reg Reg.M ] ];
      [ I8080_asm.insn (Instruction.Data { width = 1; items = [ Operand.Reg Reg.A ] }) ];
    ];
  [%expect
    {|
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: no mov form takes these operands
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: no mov form takes these operands
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: no nop form takes these operands
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: mvi: operand 1 must be a register (b c d e h l m a)
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: mvi: operand 2 must be an 8-bit value
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: lxi: operand 1 must be a register pair (b d h sp)
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: push: operand 1 must be a register pair (b d h psw)
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: ldax: operand 1 must be b or d
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: rst: operand 1 must be 0..7
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: rst: operand 1 must be 0..7
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: jmp: operand 1 must be an address
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: no mov form takes these operands
    REFUSED <synthesized by i8080.lower from <synthesized by i8080>>: error[i8080.simplify]: db value: operand 1 must be a value |}];
  (* the printer takes them too *)
  Printf.printf "%s | %s\n"
    (text (Instruction.Insn { entry = entry "mov"; args = [ Operand.Reg Reg.A ] }))
    (text (Instruction.Insn { entry = entry "nop"; args = [ imm 1 ] }));
  [%expect {| mov a | nop 1 |}]

let%expect_test "labels: forward, backward, nonzero origin, symbols" =
  let open I8080_asm in
  let prog =
    [
      insn (jmp_e (symbol "fwd"));
      label "back";
      insn (nop ());
      insn (jmp_e (symbol "back"));
      label "fwd";
      insn (jz_e (symbol "back"));
      insn (call_e (symbol "fwd"));
      insn (hlt ());
    ]
  in
  List.iter
    (fun origin ->
      let img = built ~origin prog in
      Printf.printf "%04x: %s | %s\n" origin (hex img.Program.bytes)
        (String.concat " "
           (List.map (fun (n, a) -> Printf.sprintf "%s=%04x" n a) img.Program.symbols)))
    [ 0; 0x100; 0xfff0 ];
  [%expect
    {|
    0000: c3 07 00 00 c3 03 00 ca 03 00 cd 07 00 76 | back=0003 fwd=0007
    0100: c3 07 01 00 c3 03 01 ca 03 01 cd 07 01 76 | back=0103 fwd=0107
    fff0: c3 f7 ff 00 c3 f3 ff ca f3 ff cd f7 ff 76 | back=fff3 fwd=fff7 |}]

let%expect_test "data, symbols in data, zero storage, address space" =
  let open I8080_asm in
  let img =
    built ~origin:0x200
      [
        insn (lxi_e hl (symbol "table"));
        insn (hlt ());
        label "table";
        dw_e [ symbol "table"; symbol "end"; plus (symbol "table") (const 1) ];
        db [ 1; 2; 255 ];
        db_e [ low (symbol "end"); high (symbol "end") ];
        ascii "ok";
        label "buf";
        space 3;
        label "end";
      ]
  in
  Printf.printf "%s\n" (hex img.Program.bytes);
  List.iter (fun (n, a) -> Printf.printf "%s=%04x " n a) img.Program.symbols;
  print_newline ();
  Printf.printf "%s\n" (show ~origin:0xfffe [ insn (nop ()); insn (nop ()); insn (nop ()) ]);
  Printf.printf "%s\n" (show ~origin:0xfffe [ insn (nop ()); insn (nop ()) ]);
  Printf.printf "%s\n" (show ~origin:0x10000 []);
  Printf.printf "%d bytes at ff00\n"
    (String.length (built ~origin:0xff00 [ space 0x100 ]).I8080_asm.Program.bytes);
  Printf.printf "%s\n" (show ~origin:0xff01 [ space 0x100 ]);
  [%expect
    {|
    21 04 02 76 04 02 14 02 05 02 01 02 ff 14 02 6f 6b 00 00 00
    table=0204 buf=0211 end=0214
    REFUSED <synthesized by i8080>: error[i8080.image]: image of 3 bytes at 0xfffe leaves the 64 KiB address space
    fffe: 00 00
    REFUSED <synthesized by i8080>: error[i8080.lower]: origin 65536 does not fit 0..65535
    256 bytes at ff00
    REFUSED <synthesized by i8080>: error[i8080.image]: image of 256 bytes at 0xff01 leaves the 64 KiB address space |}]

(* The consumer shape: no console, inputs and results in RAM, HLT at the end. *)
let run ?(max = 100_000) ?input ~origin img =
  let st = S.create ~input:(fun _ -> 0xff) ~output:(fun _ _ -> ()) in
  S.poke st origin img;
  Option.iter (fun (addr, v) -> S.poke st addr (String.make 1 (Char.chr v))) input;
  S.start st ~pc:origin ~sp:0xf000;
  while (not (S.halted st)) && S.instructions st < max do
    S.step st
  done;
  (S.halted st, S.memory st)

(* sum(n) = n + sum(n-1), recursive through the stack; n at 0x8000, result at 0x8001. *)
let sum_program =
  let open I8080_asm in
  [
    insn (lxi sp 0xf000);
    insn (lda 0x8000);
    insn (call_e (symbol "sum"));
    insn (sta 0x8001);
    insn (hlt ());
    label "sum";
    insn (ora a);
    insn (rz ());
    insn (push psw);
    insn (dcr a);
    insn (call_e (symbol "sum"));
    insn (mov b a);
    insn (pop psw);
    insn (add b);
    insn (ret ());
  ]

let%expect_test "a recursive program runs to HLT and leaves its result in RAM" =
  List.iter
    (fun origin ->
      let img = built ~origin sum_program in
      List.iter
        (fun n ->
          let halted, mem = run ~origin ~input:(0x8000, n) img.I8080_asm.Program.bytes in
          Printf.printf "origin=%04x n=%d halted=%b result=%d expect=%d\n" origin n halted
            (Char.code (Bytes.get mem 0x8001))
            (n * (n + 1) / 2 land 0xff))
        [ 0; 1; 10; 22 ])
    [ 0; 0x100; 0x4000 ];
  [%expect
    {|
    origin=0000 n=0 halted=true result=0 expect=0
    origin=0000 n=1 halted=true result=1 expect=1
    origin=0000 n=10 halted=true result=55 expect=55
    origin=0000 n=22 halted=true result=253 expect=253
    origin=0100 n=0 halted=true result=0 expect=0
    origin=0100 n=1 halted=true result=1 expect=1
    origin=0100 n=10 halted=true result=55 expect=55
    origin=0100 n=22 halted=true result=253 expect=253
    origin=4000 n=0 halted=true result=0 expect=0
    origin=4000 n=1 halted=true result=1 expect=1
    origin=4000 n=10 halted=true result=55 expect=55
    origin=4000 n=22 halted=true result=253 expect=253 |}]

let%expect_test "a table of addresses resolves" =
  let open I8080_asm in
  let prog =
    [
      insn (lhld_e (symbol "ptr"));
      insn (mov a m);
      insn (sta 0x8000);
      insn (hlt ());
      label "ptr";
      dw_e [ symbol "msg" ];
      label "msg";
      db [ 0x5a ];
    ]
  in
  let img = built ~origin:0x300 prog in
  let halted, mem = run ~origin:0x300 img.Program.bytes in
  Printf.printf "halted=%b ram=%02x msg=%04x\n" halted
    (Char.code (Bytes.get mem 0x8000))
    (Option.get (Program.symbol img "msg"));
  [%expect {| halted=true ram=5a msg=030a |}]
