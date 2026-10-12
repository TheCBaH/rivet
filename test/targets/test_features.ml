(* Feature configuration: an optional component (RISC-V Zmmul/M, x86 x87) can be turned off,
   and once it is, no path emits or accepts its instructions.

   Each section names the path it covers. The text path is the ordinary one; the others are the
   ways to reach an encoder without it - normalized instructions handed to lowering, lowered
   forms handed to the encoder, and bytes handed to the decoder. Feature enforcement that only
   filtered the text front end would pass the first section and fail the rest. *)

let driver target = match Driver.Registry.find target with Some d -> d | None -> failwith target

let render ds =
  String.concat "; "
    (List.map
       (fun d ->
         Printf.sprintf "%s: %s" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
       (Foundation.Diag.diagnostics ds))

let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let spec text =
  match Target_config.parse_spec text with Ok s -> s | Error m -> failwith ("bad spec: " ^ m)

(* Assemble [text] under [features] and bind it at a fixed address. *)
let assembled ?features target text =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  let features = Option.map spec features in
  let source = Foundation.Span.source ~name:"<test>" ~contents:text in
  match D.assemble ?features ~unit_name:"t" ~source () with
  | Error ds -> Error (render ds)
  | Ok laid_out -> (
      let plan = Image.plan_of laid_out in
      let addresses =
        List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0x1000L)) plan.Image.segments
      in
      match Image.bind_image laid_out ~addresses with
      | Error ds -> Error (render ds)
      | Ok img -> (
          match img.Image.segments with
          | s :: _ -> Ok (s.Image.bytes, s.Image.address)
          | [] -> Error "no segments"))

let outcome = function Ok (bytes, _) -> hex bytes | Error m -> "REFUSED " ^ m

let show ?features ?(prelude = "") target line =
  Printf.printf "%-8s [%-9s] %-22s %s\n" target
    (Option.value features ~default:"default")
    line
    (outcome (assembled ?features target ("\t.text\n" ^ prelude ^ "\t" ^ line ^ "\n")))

(* {1 Resolving a configuration} *)

let%expect_test "spec parsing" =
  List.iter
    (fun text ->
      match Target_config.parse_spec text with
      | Ok s -> Printf.printf "%-16S -> %s\n" text (Target_config.spec_to_string s)
      | Error m -> Printf.printf "%-16S -> error: %s\n" text m)
    [ ""; "none"; "none,+m"; "-x87"; " m , -x87 "; "+"; "-"; "a,,b" ];
  [%expect
    {|
    ""               ->
    "none"           -> none
    "none,+m"        -> none,+m
    "-x87"           -> -x87
    " m , -x87 "     -> +m,-x87
    "+"              -> error: '+' needs a feature name
    "-"              -> error: '-' needs a feature name
    "a,,b"           -> error: empty feature item |}]

let%expect_test "the effective configuration of each target" =
  List.iter
    (fun t ->
      let (module D : Target_intf.Target.DRIVER) = driver t in
      List.iter
        (fun text ->
          match D.configure (spec text) with
          | Ok c ->
              Printf.printf "%-8s %-12S -> %s\n" t text
                (String.concat "," (Target_config.features c))
          | Error m -> Printf.printf "%-8s %-12S -> error: %s\n" t text m)
        [ ""; "none"; "none,+m"; "-m"; "-zmmul"; "-m,-zmmul"; "-x87"; "none,+x87"; "bogus" ])
    [ "riscv32"; "riscv64"; "x86_32"; "x86_64"; "arm"; "aarch64" ];
  [%expect
    {|
    riscv32  ""           -> a,d,f,m,q,zabha,zacas,zba,zbb,zbc,zbkb,zbkc,zbkx,zbs,zfa,zfbfmin,zfh,zfhmin,zicond,zknd,zkne,zknh,zksed,zksh,zmmul
    riscv32  "none"       ->
    riscv32  "none,+m"    -> m,zmmul
    riscv32  "-m"         -> a,d,f,q,zabha,zacas,zba,zbb,zbc,zbkb,zbkc,zbkx,zbs,zfa,zfbfmin,zfh,zfhmin,zicond,zknd,zkne,zknh,zksed,zksh,zmmul
    riscv32  "-zmmul"     -> error: feature m requires zmmul, which is disabled
    riscv32  "-m,-zmmul"  -> a,d,f,q,zabha,zacas,zba,zbb,zbc,zbkb,zbkc,zbkx,zbs,zfa,zfbfmin,zfh,zfhmin,zicond,zknd,zkne,zknh,zksed,zksh
    riscv32  "-x87"       -> error: unknown feature x87 (known: zmmul, m, f, d, a, zacas, zabha, q, zfh, zfhmin, zfa, zfbfmin, zba, zbb, zbs, zbc, zbkb, zbkc, zbkx, zknd, zkne, zknh, zksed, zksh, zicond, b, zkn, zks, zk)
    riscv32  "none,+x87"  -> error: unknown feature x87 (known: zmmul, m, f, d, a, zacas, zabha, q, zfh, zfhmin, zfa, zfbfmin, zba, zbb, zbs, zbc, zbkb, zbkc, zbkx, zknd, zkne, zknh, zksed, zksh, zicond, b, zkn, zks, zk)
    riscv32  "bogus"      -> error: unknown feature bogus (known: zmmul, m, f, d, a, zacas, zabha, q, zfh, zfhmin, zfa, zfbfmin, zba, zbb, zbs, zbc, zbkb, zbkc, zbkx, zknd, zkne, zknh, zksed, zksh, zicond, b, zkn, zks, zk)
    riscv64  ""           -> a,d,f,m,q,zabha,zacas,zba,zbb,zbc,zbkb,zbkc,zbkx,zbs,zfa,zfbfmin,zfh,zfhmin,zicond,zknd,zkne,zknh,zksed,zksh,zmmul
    riscv64  "none"       ->
    riscv64  "none,+m"    -> m,zmmul
    riscv64  "-m"         -> a,d,f,q,zabha,zacas,zba,zbb,zbc,zbkb,zbkc,zbkx,zbs,zfa,zfbfmin,zfh,zfhmin,zicond,zknd,zkne,zknh,zksed,zksh,zmmul
    riscv64  "-zmmul"     -> error: feature m requires zmmul, which is disabled
    riscv64  "-m,-zmmul"  -> a,d,f,q,zabha,zacas,zba,zbb,zbc,zbkb,zbkc,zbkx,zbs,zfa,zfbfmin,zfh,zfhmin,zicond,zknd,zkne,zknh,zksed,zksh
    riscv64  "-x87"       -> error: unknown feature x87 (known: zmmul, m, f, d, a, zacas, zabha, q, zfh, zfhmin, zfa, zfbfmin, zba, zbb, zbs, zbc, zbkb, zbkc, zbkx, zknd, zkne, zknh, zksed, zksh, zicond, b, zkn, zks, zk)
    riscv64  "none,+x87"  -> error: unknown feature x87 (known: zmmul, m, f, d, a, zacas, zabha, q, zfh, zfhmin, zfa, zfbfmin, zba, zbb, zbs, zbc, zbkb, zbkc, zbkx, zknd, zkne, zknh, zksed, zksh, zicond, b, zkn, zks, zk)
    riscv64  "bogus"      -> error: unknown feature bogus (known: zmmul, m, f, d, a, zacas, zabha, q, zfh, zfhmin, zfa, zfbfmin, zba, zbb, zbs, zbc, zbkb, zbkc, zbkx, zknd, zkne, zknh, zksed, zksh, zicond, b, zkn, zks, zk)
    x86_32   ""           -> x87
    x86_32   "none"       ->
    x86_32   "none,+m"    -> error: unknown feature m (known: x87)
    x86_32   "-m"         -> error: unknown feature m (known: x87)
    x86_32   "-zmmul"     -> error: unknown feature zmmul (known: x87)
    x86_32   "-m,-zmmul"  -> error: unknown feature m (known: x87)
    x86_32   "-x87"       ->
    x86_32   "none,+x87"  -> x87
    x86_32   "bogus"      -> error: unknown feature bogus (known: x87)
    x86_64   ""           -> x87
    x86_64   "none"       ->
    x86_64   "none,+m"    -> error: unknown feature m (known: x87)
    x86_64   "-m"         -> error: unknown feature m (known: x87)
    x86_64   "-zmmul"     -> error: unknown feature zmmul (known: x87)
    x86_64   "-m,-zmmul"  -> error: unknown feature m (known: x87)
    x86_64   "-x87"       ->
    x86_64   "none,+x87"  -> x87
    x86_64   "bogus"      -> error: unknown feature bogus (known: x87)
    arm      ""           ->
    arm      "none"       ->
    arm      "none,+m"    -> error: unknown feature m (known: (none))
    arm      "-m"         -> error: unknown feature m (known: (none))
    arm      "-zmmul"     -> error: unknown feature zmmul (known: (none))
    arm      "-m,-zmmul"  -> error: unknown feature m (known: (none))
    arm      "-x87"       -> error: unknown feature x87 (known: (none))
    arm      "none,+x87"  -> error: unknown feature x87 (known: (none))
    arm      "bogus"      -> error: unknown feature bogus (known: (none))
    aarch64  ""           ->
    aarch64  "none"       ->
    aarch64  "none,+m"    -> error: unknown feature m (known: (none))
    aarch64  "-m"         -> error: unknown feature m (known: (none))
    aarch64  "-zmmul"     -> error: unknown feature zmmul (known: (none))
    aarch64  "-m,-zmmul"  -> error: unknown feature m (known: (none))
    aarch64  "-x87"       -> error: unknown feature x87 (known: (none))
    aarch64  "none,+x87"  -> error: unknown feature x87 (known: (none))
    aarch64  "bogus"      -> error: unknown feature bogus (known: (none)) |}]

(* Requirements and conflicts are declared per component, so exercise the resolver on a
   synthetic set that has a conflict; the real targets declare none today. *)
let%expect_test "requirements, conflicts and left-to-right order" =
  let component feature requires conflicts : Target_component.t =
    {
      id = "t." ^ feature;
      feature;
      requires;
      conflicts;
      implies = [];
      default_on = true;
      xlens = [];
      not_applicable = None;
      summary = "";
      forms = [];
    }
  in
  let cs =
    [ component "base" [] []; component "ext" [ "base" ] []; component "alt" [] [ "ext" ] ]
  in
  List.iter
    (fun text ->
      match Target_config.resolve cs (spec text) with
      | Ok c -> Printf.printf "%-22S -> %s\n" text (String.concat "," (Target_config.features c))
      | Error m -> Printf.printf "%-22S -> error: %s\n" text m)
    [
      "none,+ext";
      "none,+alt";
      "none,+alt,+ext";
      "none,+ext,+alt";
      "-alt";
      "-base";
      "-ext,-base";
      "none,+ext,-base";
    ];
  [%expect
    {|
    "none,+ext"            -> base,ext
    "none,+alt"            -> alt
    "none,+alt,+ext"       -> error: feature alt conflicts with ext
    "none,+ext,+alt"       -> error: feature alt conflicts with ext
    "-alt"                 -> base,ext
    "-base"                -> error: feature ext requires base, which is disabled
    "-ext,-base"           -> alt
    "none,+ext,-base"      -> error: feature ext requires base, which is disabled |}]

(* The compressed-extension shape: C implies Zca, Zcd when D is on and Zcf when F is on at XLEN
   32; Zcmp conflicts with Zcd. Specialised to each XLEN, as a target does. *)
let compressed ~xlen =
  let base feature : Target_component.t =
    {
      id = "t." ^ feature;
      feature;
      requires = [];
      conflicts = [];
      implies = [];
      default_on = true;
      xlens = [];
      not_applicable = None;
      summary = "";
      forms = [];
    }
  in
  let c =
    {
      (base "c") with
      implies = [ (Always, "zca"); (And [ Has "d" ], "zcd"); (And [ Has "f"; Xlen 32 ], "zcf") ];
    }
  in
  List.map
    (Target_component.specialize ~xlen)
    [
      base "f";
      base "d";
      base "zca";
      base "zcd";
      { (base "zcf") with xlens = [ 32 ] };
      { (base "zcmp") with default_on = false; conflicts = [ "zcd" ]; requires = [ "zca" ] };
      c;
    ]

let%expect_test "conditional implications keep explicit removals" =
  List.iter
    (fun xlen ->
      let cs = compressed ~xlen in
      Printf.printf "xlen %d default: %s\n" xlen
        (String.concat "," (Target_config.features (Target_config.default cs)));
      List.iter
        (fun text ->
          match Target_config.resolve cs (spec text) with
          | Ok c ->
              Printf.printf "  %-22S -> %s\n" text (String.concat "," (Target_config.features c))
          | Error m -> Printf.printf "  %-22S -> error: %s\n" text m)
        [
          "";
          "none,+c,+d";
          "none,+d,+c";
          "none,+c";
          "none,+c,+f,+d";
          "-zcd";
          "-c,-zcd,+zcmp";
          "-zcd,-c";
          "-c,-zcd";
          "-c,-zcd,+zcmp,-zca";
          "none,+zcf";
          "-zcd,+zcd";
        ])
    [ 32; 64 ];
  [%expect
    {|
    xlen 32 default: c,d,f,zca,zcd,zcf
      ""                     -> c,d,f,zca,zcd,zcf
      "none,+c,+d"           -> c,d,zca,zcd
      "none,+d,+c"           -> c,d,zca,zcd
      "none,+c"              -> c,zca
      "none,+c,+f,+d"        -> c,d,f,zca,zcd,zcf
      "-zcd"                 -> error: feature zcd is implied by c, which is enabled
      "-c,-zcd,+zcmp"        -> d,f,zca,zcf,zcmp
      "-zcd,-c"              -> d,f,zca,zcf
      "-c,-zcd"              -> d,f,zca,zcf
      "-c,-zcd,+zcmp,-zca"   -> error: feature zcmp requires zca, which is disabled
      "none,+zcf"            -> zcf
      "-zcd,+zcd"            -> c,d,f,zca,zcd,zcf
    xlen 64 default: c,d,f,zca,zcd
      ""                     -> c,d,f,zca,zcd
      "none,+c,+d"           -> c,d,zca,zcd
      "none,+d,+c"           -> c,d,zca,zcd
      "none,+c"              -> c,zca
      "none,+c,+f,+d"        -> c,d,f,zca,zcd
      "-zcd"                 -> error: feature zcd is implied by c, which is enabled
      "-c,-zcd,+zcmp"        -> d,f,zca,zcmp
      "-zcd,-c"              -> d,f,zca
      "-c,-zcd"              -> d,f,zca
      "-c,-zcd,+zcmp,-zca"   -> error: feature zcmp requires zca, which is disabled
      "none,+zcf"            -> error: feature zcf is not applicable at XLEN 64
      "-zcd,+zcd"            -> c,d,f,zca,zcd |}]

(* {1 The text path} *)

let%expect_test "M: enabled, partially enabled, disabled" =
  List.iter
    (fun features ->
      show ~features "riscv64" "mul x5, x6, x7";
      show ~features "riscv64" "mulw x5, x6, x7";
      show ~features "riscv64" "remu x5, x6, x7";
      show ~features "riscv64" "add x5, x6, x7")
    [ "none,+m"; "none,+zmmul"; "none" ];
  [%expect
    {|
    riscv64  [none,+m  ] mul x5, x6, x7         b3 02 73 02
    riscv64  [none,+m  ] mulw x5, x6, x7        bb 02 73 02
    riscv64  [none,+m  ] remu x5, x6, x7        b3 72 73 02
    riscv64  [none,+m  ] add x5, x6, x7         b3 02 73 00
    riscv64  [none,+zmmul] mul x5, x6, x7         b3 02 73 02
    riscv64  [none,+zmmul] mulw x5, x6, x7        bb 02 73 02
    riscv64  [none,+zmmul] remu x5, x6, x7        REFUSED riscv64.feature: remu requires feature m, which is not enabled
    riscv64  [none,+zmmul] add x5, x6, x7         b3 02 73 00
    riscv64  [none     ] mul x5, x6, x7         REFUSED riscv64.feature: mul requires feature zmmul, which is not enabled
    riscv64  [none     ] mulw x5, x6, x7        REFUSED riscv64.feature: mulw requires feature zmmul, which is not enabled
    riscv64  [none     ] remu x5, x6, x7        REFUSED riscv64.feature: remu requires feature m, which is not enabled
    riscv64  [none     ] add x5, x6, x7         b3 02 73 00 |}]

let%expect_test "M on RV32, where mulw does not exist regardless" =
  show ~features:"none,+m" "riscv32" "mulw x5, x6, x7";
  show ~features:"none" "riscv32" "mulw x5, x6, x7";
  [%expect
    {|
    riscv32  [none,+m  ] mulw x5, x6, x7        REFUSED riscv32.lower: mulw is available only when XLEN is 64
    riscv32  [none     ] mulw x5, x6, x7        REFUSED riscv32.feature: mulw requires feature zmmul, which is not enabled |}]

let%expect_test "x87: enabled and disabled, with the base unaffected" =
  List.iter
    (fun features ->
      List.iter
        (fun t ->
          show ~features t "fldl 8(%esp)";
          show ~features t "fadd %st(1), %st";
          show ~features t "fucomp";
          show ~features t "fnstsw %ax";
          show ~features t "sahf";
          show ~features t "ret")
        [ "x86_32"; "x86_64" ])
    [ "x87"; "-x87" ];
  [%expect
    {|
    x86_32   [x87      ] fldl 8(%esp)           dd 44 24 08
    x86_32   [x87      ] fadd %st(1), %st       d8 c1
    x86_32   [x87      ] fucomp                 dd e9
    x86_32   [x87      ] fnstsw %ax             df e0
    x86_32   [x87      ] sahf                   9e
    x86_32   [x87      ] ret                    c3
    x86_64   [x87      ] fldl 8(%esp)           67 dd 44 24 08
    x86_64   [x87      ] fadd %st(1), %st       d8 c1
    x86_64   [x87      ] fucomp                 dd e9
    x86_64   [x87      ] fnstsw %ax             df e0
    x86_64   [x87      ] sahf                   9e
    x86_64   [x87      ] ret                    c3
    x86_32   [-x87     ] fldl 8(%esp)           REFUSED x86.feature: fldl requires feature x87, which is not enabled
    x86_32   [-x87     ] fadd %st(1), %st       REFUSED x86.feature: fadd requires feature x87, which is not enabled
    x86_32   [-x87     ] fucomp                 REFUSED x86.feature: fucomp requires feature x87, which is not enabled
    x86_32   [-x87     ] fnstsw %ax             REFUSED x86.feature: fnstsw requires feature x87, which is not enabled
    x86_32   [-x87     ] sahf                   9e
    x86_32   [-x87     ] ret                    c3
    x86_64   [-x87     ] fldl 8(%esp)           REFUSED x86.feature: fldl requires feature x87, which is not enabled
    x86_64   [-x87     ] fadd %st(1), %st       REFUSED x86.feature: fadd requires feature x87, which is not enabled
    x86_64   [-x87     ] fucomp                 REFUSED x86.feature: fucomp requires feature x87, which is not enabled
    x86_64   [-x87     ] fnstsw %ax             REFUSED x86.feature: fnstsw requires feature x87, which is not enabled
    x86_64   [-x87     ] sahf                   9e
    x86_64   [-x87     ] ret                    c3 |}]

let%expect_test "an invalid configuration fails before any source is read" =
  show ~features:"-zmmul" "riscv64" "add x5, x6, x7";
  show ~features:"bogus" "riscv64" "add x5, x6, x7";
  show ~features:"x87" "arm" "nop";
  [%expect
    {|
    riscv64  [-zmmul   ] add x5, x6, x7         REFUSED config.invalid: invalid feature configuration: feature m requires zmmul, which is disabled
    riscv64  [bogus    ] add x5, x6, x7         REFUSED config.invalid: invalid feature configuration: unknown feature bogus (known: zmmul, m, f, d, a, zacas, zabha, q, zfh, zfhmin, zfa, zfbfmin, zba, zbb, zbs, zbc, zbkb, zbkc, zbkx, zknd, zkne, zknh, zksed, zksh, zicond, b, zkn, zks, zk)
    arm      [x87      ] nop                    REFUSED config.invalid: invalid feature configuration: unknown feature x87 (known: (none)) |}]

(* {1 Normalized and lowered instructions handed in directly} *)

let%expect_test "RISC-V: a normalized instruction is refused at lowering, a lowered one at encoding"
    =
  let module E = Riscv64_encode in
  let config text =
    match Target_config.resolve E.components (spec text) with Ok c -> c | Error m -> failwith m
  in
  let origin = Foundation.Origin.synthesized ~pass:"test" () in
  let surface mnemonic ops =
    match E.make_surface_instruction ~mnemonic ~origin ops with
    | Ok s -> s
    | Error _ -> failwith "surface"
  in
  let x n = E.Operand.Reg (E.Reg.X n) in
  let ops = [ x 5; x 6; x 7 ] in
  List.iter
    (fun (label, features) ->
      let state = E.initial_state (config features) in
      (* Normalization is the front door, so build the instruction with the default state and
         hand it to the later stages under the restricted one. *)
      (match E.simplify_instruction E.default_state (surface "mul" ops) with
      | Error _ -> Printf.printf "%s: simplify failed unexpectedly\n" label
      | Ok insn ->
          Printf.printf "%s: lower_instruction -> %s\n" label
            (match E.lower_instruction state insn with
            | Ok _ -> "accepted"
            | Error e ->
                "refused: " ^ Foundation.Diagnostic.message (E.error_diagnostic (Err.Error.kind e))));
      let lowered =
        E.Lowered.R
          { name = "mul"; opcode = 0x33; funct3 = 0; funct7 = 1; rd = 5; rs1 = 6; rs2 = 7 }
      in
      Printf.printf "%s: encode_in -> %s\n" label
        (match E.encode_in state lowered with
        | Ok (`Fixed f) -> hex f.Asm_core.Lowered_ast.bytes
        | Ok (`Relax _) -> "relax"
        | Error e ->
            "refused: " ^ Foundation.Diagnostic.message (E.error_diagnostic (Err.Error.kind e))))
    [ ("zmmul on ", "none,+zmmul"); ("zmmul off", "none") ];
  [%expect
    {|
    zmmul on : lower_instruction -> accepted
    zmmul on : encode_in -> b3 02 73 02
    zmmul off: lower_instruction -> refused: mul requires feature zmmul, which is not enabled
    zmmul off: encode_in -> refused: mul requires feature zmmul, which is not enabled |}]

let%expect_test "x86: a lowered x87 form is refused at encoding, a base one is not" =
  let module E = X86_32_encode in
  let config text =
    match Target_config.resolve E.components (spec text) with Ok c -> c | Error m -> failwith m
  in
  let encode label features lowered =
    let state = E.initial_state (config features) in
    Printf.printf "%-9s %-8s -> %s\n" label features
      (match E.encode_in state lowered with
      | Ok (`Fixed f) -> hex f.Asm_core.Lowered_ast.bytes
      | Ok (`Relax _) -> "relax"
      | Error e ->
          "refused: " ^ Foundation.Diagnostic.message (E.error_diagnostic (Err.Error.kind e)))
  in
  List.iter
    (fun features ->
      encode "fucomp" features E.Lowered.Fucomp;
      encode "fnstsw" features E.Lowered.Fnstsw;
      encode "sahf" features E.Lowered.Sahf)
    [ "x87"; "none" ];
  [%expect
    {|
    fucomp    x87      -> dd e9
    fnstsw    x87      -> df e0
    sahf      x87      -> 9e
    fucomp    none     -> refused: fucomp requires feature x87, which is not enabled
    fnstsw    none     -> refused: fnstsw requires feature x87, which is not enabled
    sahf      none     -> 9e |}]

(* {1 Decoding} *)

let bytes_of target text = match assembled target text with Ok (b, _) -> b | Error m -> failwith m

let disasm ?features ?(inspect = false) target bytes =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  let features = Option.map spec features in
  match D.dump_disasm_diagnostic ?features ~inspect ~address:0x1000L bytes with
  | Ok text -> print_string text
  | Error ds -> Printf.printf "REFUSED %s\n" (render ds)

let%expect_test "decode: strict refuses a disabled component, inspection marks it" =
  let code =
    bytes_of "riscv64" "\t.text\n\tmul x5, x6, x7\n\tremu x5, x6, x7\n\tadd x5, x6, x7\n"
  in
  print_endline "-- default";
  disasm "riscv64" code;
  print_endline "-- strict, m disabled";
  disasm ~features:"-m" "riscv64" code;
  print_endline "-- inspect, m disabled";
  disasm ~features:"-m" ~inspect:true "riscv64" code;
  print_endline "-- inspect, everything disabled";
  disasm ~features:"none" ~inspect:true "riscv64" code;
  let x87 = bytes_of "x86_32" "\t.text\n\tfldl 8(%esp)\n\tret\n" in
  print_endline "-- x86 default";
  disasm "x86_32" x87;
  print_endline "-- x86 strict, x87 disabled";
  disasm ~features:"-x87" "x86_32" x87;
  print_endline "-- x86 inspect, x87 disabled";
  disasm ~features:"-x87" ~inspect:true "x86_32" x87;
  [%expect
    {|
    -- default
    00001000  b3 02 73 02  mul x5, x6, x7   [riscv64.mul]
    00001004  b3 72 73 02  remu x5, x6, x7  [riscv64.remu]
    00001008  b3 02 73 00  add x5, x6, x7   [riscv64.add]
    -- strict, m disabled
    REFUSED riscv64.feature: remu requires feature m, which is not enabled
    -- inspect, m disabled
    00001000  b3 02 73 02  mul x5, x6, x7                                               [riscv64.mul]
    00001004  b3 72 73 02  remu x5, x6, x7  ; requires feature m, which is not enabled  [riscv64.remu]
    00001008  b3 02 73 00  add x5, x6, x7                                               [riscv64.add]
    -- inspect, everything disabled
    00001000  b3 02 73 02  mul x5, x6, x7  ; requires feature zmmul, which is not enabled  [riscv64.mul]
    00001004  b3 72 73 02  remu x5, x6, x7  ; requires feature m, which is not enabled     [riscv64.remu]
    00001008  b3 02 73 00  add x5, x6, x7                                                  [riscv64.add]
    -- x86 default
    00001000  dd 44 24 08  fldl 8(%esp)  [x86_32.fldl.opsz-absent.sib-disp8]
    00001004  c3           ret           [x86_32.ret]
    -- x86 strict, x87 disabled
    REFUSED x86.feature: fldl requires feature x87, which is not enabled
    -- x86 inspect, x87 disabled
    00001000  dd 44 24 08  fldl 8(%esp)  ; requires feature x87, which is not enabled  [x86_32.fldl.opsz-absent.sib-disp8]
    00001004  c3           ret                                                         [x86_32.ret] |}]

let%expect_test "decode: canonical disassembly is strict and still re-assembles" =
  let (module D : Target_intf.Target.DRIVER) = driver "riscv64" in
  let code = bytes_of "riscv64" "\t.text\n\tmul x5, x6, x7\n" in
  (match D.dump_disasm_canonical ~address:0x1000L code with
  | Ok text ->
      (* Canonical lines are tab-indented; print them trimmed so the transcript has no tabs. *)
      List.iter
        (fun l -> if l <> "" then print_endline (String.trim l))
        (String.split_on_char '\n' text);
      Printf.printf "re-assembled: %s\n" (outcome (assembled "riscv64" ("\t.text\n" ^ text)))
  | Error ds -> print_endline (render ds));
  (match D.dump_disasm_canonical ~features:(spec "none") ~address:0x1000L code with
  | Ok text -> print_string text
  | Error ds -> Printf.printf "REFUSED %s\n" (render ds));
  [%expect
    {|
    mul x5, x6, x7
    re-assembled: b3 02 73 02
    REFUSED riscv64.feature: mul requires feature zmmul, which is not enabled |}]

(* {1 Scope, isolation and layout} *)

let%expect_test "RISC-V .option push/pop leaves the configuration alone" =
  let module F = Riscv64 in
  let config =
    match Target_config.resolve F.components (spec "none,+zmmul") with
    | Ok c -> c
    | Error m -> failwith m
  in
  let state = F.initial_state config in
  let step directive state =
    match F.handle_directive ~name:".option" ~argument:directive state with
    | Target_intf.Target.Handled { state; _ } -> state
    | _ -> failwith directive
  in
  let after = state |> step "push" |> step "rvc" |> step "pic" |> step "pop" in
  Printf.printf "before: %s\nafter:  %s\nsame: %b\n"
    (String.concat "," (Target_config.features (F.state_config state)))
    (String.concat "," (Target_config.features (F.state_config after)))
    (Target_config.equal (F.state_config state) (F.state_config after));
  [%expect {|
    before: zmmul
    after:  zmmul
    same: true |}]

let%expect_test "every unit of a multi-unit assembly gets the caller's configuration" =
  let (module D : Target_intf.Target.DRIVER) = driver "riscv64" in
  let unit name text = (name, Foundation.Span.source ~name ~contents:text) in
  let units =
    [
      unit "a" "\t.text\n\t.globl fa\nfa:\n\tmul x5, x6, x7\n";
      unit "b" "\t.text\n\t.globl fb\nfb:\n\tremu x5, x6, x7\n";
    ]
  in
  List.iter
    (fun features ->
      match D.assemble_many ~features:(spec features) units () with
      | Ok _ -> Printf.printf "%-12s accepted\n" features
      | Error ds -> Printf.printf "%-12s REFUSED %s\n" features (render ds))
    [ "none,+m"; "none,+zmmul"; "none" ];
  [%expect
    {|
    none,+m      accepted
    none,+zmmul  REFUSED riscv64.feature: remu requires feature m, which is not enabled
    none         REFUSED riscv64.feature: mul requires feature zmmul, which is not enabled; riscv64.feature: remu requires feature m, which is not enabled |}]

let%expect_test "padding and relaxation do not depend on an optional component" =
  (* A disabled component must not be needed to lay code out: alignment padding is
     target-defined filler, and a branch ladder is base-ISA. *)
  let check target features text =
    match assembled ~features target text with
    | Ok (bytes, _) -> Printf.printf "%-8s [%s] %d bytes\n" target features (String.length bytes)
    | Error m -> Printf.printf "%-8s [%s] REFUSED %s\n" target features m
  in
  check "riscv64" "none" "\t.text\n\tadd x5, x6, x7\n\t.balign 16\n\tadd x5, x6, x7\n";
  check "x86_32" "none" "\t.text\n\tret\n\t.balign 16\n\tret\n";
  check "x86_64" "-x87" "\t.text\n\tjmp far\n\t.balign 256\nfar:\n\tret\n";
  let code = bytes_of "x86_32" "\t.text\n\tret\n\t.balign 16\n\tret\n" in
  print_endline "-- disassembly of padding under -x87";
  disasm ~features:"-x87" "x86_32" code;
  [%expect
    {|
    riscv64  [none] 32 bytes
    x86_32   [none] 17 bytes
    x86_64   [-x87] 257 bytes
    -- disassembly of padding under -x87
    00001000  c3                                            ret         [x86_32.ret]
    00001001  2e 8d b4 26 00 00 00 00 8d b4 26 00 00 00 00  .balign 16  [padding]
    00001010  c3                                            ret         [x86_32.ret] |}]

let%expect_test "pseudo-instructions expand without a disabled component" =
  List.iter
    (fun line -> show ~prelude:"\t.globl f\nf:\n" ~features:"none" "riscv64" line)
    [ "nop"; "ret"; "mv x5, x6"; "li x5, 42"; "call f"; "tail f"; "la x5, f" ];
  List.iter
    (fun line -> show ~prelude:"\t.globl f\nf:\n" ~features:"-x87" "x86_64" line)
    [ "ret"; "call f"; "jmp f" ];
  [%expect
    {|
    riscv64  [none     ] nop                    13 00 00 00
    riscv64  [none     ] ret                    67 80 00 00
    riscv64  [none     ] mv x5, x6              93 02 03 00
    riscv64  [none     ] li x5, 42              93 02 a0 02
    riscv64  [none     ] call f                 97 00 00 00 e7 80 00 00
    riscv64  [none     ] tail f                 17 03 00 00 67 00 03 00
    riscv64  [none     ] la x5, f               b7 12 00 00 93 82 02 00
    x86_64   [-x87     ] ret                    c3
    x86_64   [-x87     ] call f                 e8 fb ff ff ff
    x86_64   [-x87     ] jmp f                  eb fe |}]

(* {1 Defaults} *)

let%expect_test "the default configuration is pinned" =
  (* Compatibility: an unconfigured assembler enables every component it implements, and a
     target with no components has an empty configuration. Naming a feature enabled here does
     not claim the extension is fully implemented; see each component's summary. *)
  List.iter
    (fun (module D : Target_intf.Target.DRIVER) ->
      match D.configure Target_config.default_spec with
      | Ok c ->
          Printf.printf "%-8s enabled=[%s] components=[%s]\n" D.name
            (String.concat "," (Target_config.features c))
            (String.concat "," (List.map (fun (c : Target_component.t) -> c.id) D.components))
      | Error m -> Printf.printf "%-8s error: %s\n" D.name m)
    Driver.Registry.drivers;
  [%expect
    {|
    x86_32   enabled=[x87] components=[x86.x87]
    x86_64   enabled=[x87] components=[x86.x87]
    arm      enabled=[] components=[]
    aarch64  enabled=[] components=[]
    riscv32  enabled=[a,d,f,m,q,zabha,zacas,zba,zbb,zbc,zbkb,zbkc,zbkx,zbs,zfa,zfbfmin,zfh,zfhmin,zicond,zknd,zkne,zknh,zksed,zksh,zmmul] components=[riscv.zmmul,riscv.m,riscv.f,riscv.d,riscv.a,riscv.zacas,riscv.zabha,riscv.q,riscv.zfh,riscv.zfhmin,riscv.zfa,riscv.zfbfmin,riscv.zba,riscv.zbb,riscv.zbs,riscv.zbc,riscv.zbkb,riscv.zbkc,riscv.zbkx,riscv.zknd,riscv.zkne,riscv.zknh,riscv.zksed,riscv.zksh,riscv.zicond,riscv.b,riscv.zkn,riscv.zks,riscv.zk]
    riscv64  enabled=[a,d,f,m,q,zabha,zacas,zba,zbb,zbc,zbkb,zbkc,zbkx,zbs,zfa,zfbfmin,zfh,zfhmin,zicond,zknd,zkne,zknh,zksed,zksh,zmmul] components=[riscv.zmmul,riscv.m,riscv.f,riscv.d,riscv.a,riscv.zacas,riscv.zabha,riscv.q,riscv.zfh,riscv.zfhmin,riscv.zfa,riscv.zfbfmin,riscv.zba,riscv.zbb,riscv.zbs,riscv.zbc,riscv.zbkb,riscv.zbkc,riscv.zbkx,riscv.zknd,riscv.zkne,riscv.zknh,riscv.zksed,riscv.zksh,riscv.zicond,riscv.b,riscv.zkn,riscv.zks,riscv.zk] |}]

(* [add.uw rd, rs, x0] is also [zext.w]; refusing the specialization leaves the general form. *)
let%expect_test "a disabled specialization falls back to the enabled general form" =
  let bytes = "\xbb\x00\x01\x08" in
  let decode legal =
    match
      Riscv64_encode.decode_where ~legal
        { state = Riscv64_encode.default_state; address = 0L }
        bytes ~pos:0
    with
    | Ok (_, name, n) -> Printf.sprintf "%s (%d bytes)" name n
    | Error _ -> "refused"
  in
  List.iter
    (fun (label, legal) -> Printf.printf "%s: %s\n" label (decode legal))
    [
      ("all legal", fun _ -> true);
      ("no zext.w", fun m -> m <> "zext.w");
      ("neither", fun m -> m <> "zext.w" && m <> "add.uw");
    ];
  [%expect
    {|
    all legal: zext.w (4 bytes)
    no zext.w: add.uw (4 bytes)
    neither: refused
    |}]

(* {1 Floating point} *)

let%expect_test "F and D: text path, both XLENs" =
  List.iter
    (fun (target, features) ->
      List.iter (show ~features target)
        [
          "flw f1, 0(x2)";
          "fadd.s f1, f2, f3";
          "fadd.d f1, f2, f3";
          "fneg.d f1, f2";
          "fmv.x.w x5, f1";
          "add x5, x6, x7";
        ])
    [
      ("riscv32", "none,+f,+d"); ("riscv64", "none,+f"); ("riscv64", "none"); ("riscv64", "none,+d");
    ];
  [%expect
    {|
    riscv32  [none,+f,+d] flw f1, 0(x2)          87 20 01 00
    riscv32  [none,+f,+d] fadd.s f1, f2, f3      d3 70 31 00
    riscv32  [none,+f,+d] fadd.d f1, f2, f3      d3 70 31 02
    riscv32  [none,+f,+d] fneg.d f1, f2          d3 10 21 22
    riscv32  [none,+f,+d] fmv.x.w x5, f1         d3 82 00 e0
    riscv32  [none,+f,+d] add x5, x6, x7         b3 02 73 00
    riscv64  [none,+f  ] flw f1, 0(x2)          87 20 01 00
    riscv64  [none,+f  ] fadd.s f1, f2, f3      d3 70 31 00
    riscv64  [none,+f  ] fadd.d f1, f2, f3      REFUSED riscv64.feature: fadd.d requires feature d, which is not enabled
    riscv64  [none,+f  ] fneg.d f1, f2          REFUSED riscv64.feature: fneg.d requires feature d, which is not enabled
    riscv64  [none,+f  ] fmv.x.w x5, f1         d3 82 00 e0
    riscv64  [none,+f  ] add x5, x6, x7         b3 02 73 00
    riscv64  [none     ] flw f1, 0(x2)          REFUSED riscv64.feature: flw requires feature f, which is not enabled
    riscv64  [none     ] fadd.s f1, f2, f3      REFUSED riscv64.feature: fadd.s requires feature f, which is not enabled
    riscv64  [none     ] fadd.d f1, f2, f3      REFUSED riscv64.feature: fadd.d requires feature d, which is not enabled
    riscv64  [none     ] fneg.d f1, f2          REFUSED riscv64.feature: fneg.d requires feature d, which is not enabled
    riscv64  [none     ] fmv.x.w x5, f1         REFUSED riscv64.feature: fmv.x.w requires feature f, which is not enabled
    riscv64  [none     ] add x5, x6, x7         b3 02 73 00
    riscv64  [none,+d  ] flw f1, 0(x2)          87 20 01 00
    riscv64  [none,+d  ] fadd.s f1, f2, f3      d3 70 31 00
    riscv64  [none,+d  ] fadd.d f1, f2, f3      d3 70 31 02
    riscv64  [none,+d  ] fneg.d f1, f2          d3 10 21 22
    riscv64  [none,+d  ] fmv.x.w x5, f1         d3 82 00 e0
    riscv64  [none,+d  ] add x5, x6, x7         b3 02 73 00 |}]

let%expect_test "F and D: lowered and decoded forms" =
  let module E = Riscv64_encode in
  let config text =
    match Target_config.resolve E.components (spec text) with Ok c -> c | Error m -> failwith m
  in
  let lowered =
    E.Lowered.R { name = "fadd.s"; opcode = 0x53; funct3 = 7; funct7 = 0; rd = 1; rs1 = 2; rs2 = 3 }
  in
  List.iter
    (fun features ->
      let state = E.initial_state (config features) in
      Printf.printf "%-10s encode_in -> %s\n" features
        (match E.encode_in state lowered with
        | Ok (`Fixed f) -> hex f.Asm_core.Lowered_ast.bytes
        | Ok (`Relax _) -> "relax"
        | Error e ->
            "refused: " ^ Foundation.Diagnostic.message (E.error_diagnostic (Err.Error.kind e))))
    [ "none,+f"; "none" ];
  let code =
    bytes_of "riscv64" "\t.text\n\tfadd.d f1, f2, f3\n\tfmv.x.w x5, f1\n\taddi x5, x6, 1\n"
  in
  List.iter
    (fun features ->
      Printf.printf "-- %s\n" features;
      disasm ~features "riscv64" code;
      disasm ~features ~inspect:true "riscv64" code)
    [ "none,+f"; "none,+f,+d"; "none" ];
  [%expect
    {|
    none,+f    encode_in -> d3 70 31 00
    none       encode_in -> refused: fadd.s requires feature f, which is not enabled
    -- none,+f
    REFUSED riscv64.feature: fadd.d requires feature d, which is not enabled
    00001000  d3 70 31 02  fadd.d f1, f2, f3  ; requires feature d, which is not enabled  [riscv64.fadd.d]
    00001004  d3 82 00 e0  fmv.x.w x5, f1                                                 [riscv64.fmv.x.w]
    00001008  93 02 13 00  addi x5, x6, 1                                                 [riscv64.addi]
    -- none,+f,+d
    00001000  d3 70 31 02  fadd.d f1, f2, f3  [riscv64.fadd.d]
    00001004  d3 82 00 e0  fmv.x.w x5, f1     [riscv64.fmv.x.w]
    00001008  93 02 13 00  addi x5, x6, 1     [riscv64.addi]
    00001000  d3 70 31 02  fadd.d f1, f2, f3  [riscv64.fadd.d]
    00001004  d3 82 00 e0  fmv.x.w x5, f1     [riscv64.fmv.x.w]
    00001008  93 02 13 00  addi x5, x6, 1     [riscv64.addi]
    -- none
    REFUSED riscv64.feature: fadd.d requires feature d, which is not enabled
    00001000  d3 70 31 02  fadd.d f1, f2, f3  ; requires feature d, which is not enabled  [riscv64.fadd.d]
    00001004  d3 82 00 e0  fmv.x.w x5, f1  ; requires feature f, which is not enabled     [riscv64.fmv.x.w]
    00001008  93 02 13 00  addi x5, x6, 1                                                 [riscv64.addi] |}]

(* {1 Atomics} *)

let%expect_test "A, Zacas and Zabha: text path" =
  List.iter
    (fun features ->
      List.iter (show ~features "riscv64")
        [
          "amoadd.w x5, x6, (x7)";
          "lr.d x5, (x7)";
          "amoadd.b x5, x6, (x7)";
          "amocas.d x6, x8, (x7)";
          "amocas.b x6, x8, (x7)";
        ])
    [ "none,+a"; "none,+zacas"; "none,+zabha"; "none,+zacas,+zabha"; "none" ];
  [%expect
    {|
    riscv64  [none,+a  ] amoadd.w x5, x6, (x7)  af a2 63 00
    riscv64  [none,+a  ] lr.d x5, (x7)          af b2 03 10
    riscv64  [none,+a  ] amoadd.b x5, x6, (x7)  REFUSED riscv64.feature: amoadd.b requires feature zabha, which is not enabled
    riscv64  [none,+a  ] amocas.d x6, x8, (x7)  REFUSED riscv64.feature: amocas.d requires feature zacas, which is not enabled
    riscv64  [none,+a  ] amocas.b x6, x8, (x7)  REFUSED riscv64.feature: amocas.b requires feature zacas and zabha, which is not enabled
    riscv64  [none,+zacas] amoadd.w x5, x6, (x7)  af a2 63 00
    riscv64  [none,+zacas] lr.d x5, (x7)          af b2 03 10
    riscv64  [none,+zacas] amoadd.b x5, x6, (x7)  REFUSED riscv64.feature: amoadd.b requires feature zabha, which is not enabled
    riscv64  [none,+zacas] amocas.d x6, x8, (x7)  2f b3 83 28
    riscv64  [none,+zacas] amocas.b x6, x8, (x7)  REFUSED riscv64.feature: amocas.b requires feature zabha, which is not enabled
    riscv64  [none,+zabha] amoadd.w x5, x6, (x7)  af a2 63 00
    riscv64  [none,+zabha] lr.d x5, (x7)          af b2 03 10
    riscv64  [none,+zabha] amoadd.b x5, x6, (x7)  af 82 63 00
    riscv64  [none,+zabha] amocas.d x6, x8, (x7)  REFUSED riscv64.feature: amocas.d requires feature zacas, which is not enabled
    riscv64  [none,+zabha] amocas.b x6, x8, (x7)  REFUSED riscv64.feature: amocas.b requires feature zacas, which is not enabled
    riscv64  [none,+zacas,+zabha] amoadd.w x5, x6, (x7)  af a2 63 00
    riscv64  [none,+zacas,+zabha] lr.d x5, (x7)          af b2 03 10
    riscv64  [none,+zacas,+zabha] amoadd.b x5, x6, (x7)  af 82 63 00
    riscv64  [none,+zacas,+zabha] amocas.d x6, x8, (x7)  2f b3 83 28
    riscv64  [none,+zacas,+zabha] amocas.b x6, x8, (x7)  2f 83 83 28
    riscv64  [none     ] amoadd.w x5, x6, (x7)  REFUSED riscv64.feature: amoadd.w requires feature a, which is not enabled
    riscv64  [none     ] lr.d x5, (x7)          REFUSED riscv64.feature: lr.d requires feature a, which is not enabled
    riscv64  [none     ] amoadd.b x5, x6, (x7)  REFUSED riscv64.feature: amoadd.b requires feature zabha, which is not enabled
    riscv64  [none     ] amocas.d x6, x8, (x7)  REFUSED riscv64.feature: amocas.d requires feature zacas, which is not enabled
    riscv64  [none     ] amocas.b x6, x8, (x7)  REFUSED riscv64.feature: amocas.b requires feature zacas and zabha, which is not enabled |}]

let%expect_test "A: a disabled atomic is refused when decoding" =
  let code = bytes_of "riscv64" "\t.text\n\tamoadd.w x5, x6, (x7)\n\tadd x5, x6, x7\n" in
  List.iter
    (fun features ->
      Printf.printf "-- %s\n" features;
      disasm ~features "riscv64" code;
      disasm ~features ~inspect:true "riscv64" code)
    [ "none,+a"; "none" ];
  [%expect
    {|
    -- none,+a
    00001000  af a2 63 00  amoadd.w x5, x6, 0(x7)  [riscv64.amoadd.w]
    00001004  b3 02 73 00  add x5, x6, x7          [riscv64.add]
    00001000  af a2 63 00  amoadd.w x5, x6, 0(x7)  [riscv64.amoadd.w]
    00001004  b3 02 73 00  add x5, x6, x7          [riscv64.add]
    -- none
    REFUSED riscv64.feature: amoadd.w requires feature a, which is not enabled
    00001000  af a2 63 00  amoadd.w x5, x6, 0(x7)  ; requires feature a, which is not enabled  [riscv64.amoadd.w]
    00001004  b3 02 73 00  add x5, x6, x7                                                      [riscv64.add] |}]

let%expect_test "Zfa, Zfh, Q: mixed-format forms need both formats" =
  List.iter
    (fun features ->
      List.iter (show ~features "riscv64")
        [
          "fli.s f1, 1.0";
          "fli.d f1, 1.0";
          "fli.h f1, 1.0";
          "fminm.q f1, f2, f3";
          "fcvt.d.h f1, f2";
          "fadd.h f1, f2, f3";
          "flh f1, 0(x2)";
          "fadd.q f1, f2, f3";
        ])
    [ "none,+zfa"; "none,+zfa,+d"; "none,+zfa,+zfh"; "none,+zfh,+d"; "none,+zfhmin"; "none,+q" ];
  [%expect
    {|
    riscv64  [none,+zfa] fli.s f1, 1.0          d3 00 18 f0
    riscv64  [none,+zfa] fli.d f1, 1.0          REFUSED riscv64.feature: fli.d requires feature d, which is not enabled
    riscv64  [none,+zfa] fli.h f1, 1.0          REFUSED riscv64.feature: fli.h requires feature zfh, which is not enabled
    riscv64  [none,+zfa] fminm.q f1, f2, f3     REFUSED riscv64.feature: fminm.q requires feature q, which is not enabled
    riscv64  [none,+zfa] fcvt.d.h f1, f2        REFUSED riscv64.feature: fcvt.d.h requires feature zfhmin and d, which is not enabled
    riscv64  [none,+zfa] fadd.h f1, f2, f3      REFUSED riscv64.feature: fadd.h requires feature zfh, which is not enabled
    riscv64  [none,+zfa] flh f1, 0(x2)          REFUSED riscv64.feature: flh requires feature zfhmin, which is not enabled
    riscv64  [none,+zfa] fadd.q f1, f2, f3      REFUSED riscv64.feature: fadd.q requires feature q, which is not enabled
    riscv64  [none,+zfa,+d] fli.s f1, 1.0          d3 00 18 f0
    riscv64  [none,+zfa,+d] fli.d f1, 1.0          d3 00 18 f2
    riscv64  [none,+zfa,+d] fli.h f1, 1.0          REFUSED riscv64.feature: fli.h requires feature zfh, which is not enabled
    riscv64  [none,+zfa,+d] fminm.q f1, f2, f3     REFUSED riscv64.feature: fminm.q requires feature q, which is not enabled
    riscv64  [none,+zfa,+d] fcvt.d.h f1, f2        REFUSED riscv64.feature: fcvt.d.h requires feature zfhmin, which is not enabled
    riscv64  [none,+zfa,+d] fadd.h f1, f2, f3      REFUSED riscv64.feature: fadd.h requires feature zfh, which is not enabled
    riscv64  [none,+zfa,+d] flh f1, 0(x2)          REFUSED riscv64.feature: flh requires feature zfhmin, which is not enabled
    riscv64  [none,+zfa,+d] fadd.q f1, f2, f3      REFUSED riscv64.feature: fadd.q requires feature q, which is not enabled
    riscv64  [none,+zfa,+zfh] fli.s f1, 1.0          d3 00 18 f0
    riscv64  [none,+zfa,+zfh] fli.d f1, 1.0          REFUSED riscv64.feature: fli.d requires feature d, which is not enabled
    riscv64  [none,+zfa,+zfh] fli.h f1, 1.0          d3 00 18 f4
    riscv64  [none,+zfa,+zfh] fminm.q f1, f2, f3     REFUSED riscv64.feature: fminm.q requires feature q, which is not enabled
    riscv64  [none,+zfa,+zfh] fcvt.d.h f1, f2        REFUSED riscv64.feature: fcvt.d.h requires feature d, which is not enabled
    riscv64  [none,+zfa,+zfh] fadd.h f1, f2, f3      d3 70 31 04
    riscv64  [none,+zfa,+zfh] flh f1, 0(x2)          87 10 01 00
    riscv64  [none,+zfa,+zfh] fadd.q f1, f2, f3      REFUSED riscv64.feature: fadd.q requires feature q, which is not enabled
    riscv64  [none,+zfh,+d] fli.s f1, 1.0          REFUSED riscv64.feature: fli.s requires feature zfa, which is not enabled
    riscv64  [none,+zfh,+d] fli.d f1, 1.0          REFUSED riscv64.feature: fli.d requires feature zfa, which is not enabled
    riscv64  [none,+zfh,+d] fli.h f1, 1.0          REFUSED riscv64.feature: fli.h requires feature zfa, which is not enabled
    riscv64  [none,+zfh,+d] fminm.q f1, f2, f3     REFUSED riscv64.feature: fminm.q requires feature zfa and q, which is not enabled
    riscv64  [none,+zfh,+d] fcvt.d.h f1, f2        d3 00 21 42
    riscv64  [none,+zfh,+d] fadd.h f1, f2, f3      d3 70 31 04
    riscv64  [none,+zfh,+d] flh f1, 0(x2)          87 10 01 00
    riscv64  [none,+zfh,+d] fadd.q f1, f2, f3      REFUSED riscv64.feature: fadd.q requires feature q, which is not enabled
    riscv64  [none,+zfhmin] fli.s f1, 1.0          REFUSED riscv64.feature: fli.s requires feature zfa, which is not enabled
    riscv64  [none,+zfhmin] fli.d f1, 1.0          REFUSED riscv64.feature: fli.d requires feature zfa and d, which is not enabled
    riscv64  [none,+zfhmin] fli.h f1, 1.0          REFUSED riscv64.feature: fli.h requires feature zfa and zfh, which is not enabled
    riscv64  [none,+zfhmin] fminm.q f1, f2, f3     REFUSED riscv64.feature: fminm.q requires feature zfa and q, which is not enabled
    riscv64  [none,+zfhmin] fcvt.d.h f1, f2        REFUSED riscv64.feature: fcvt.d.h requires feature d, which is not enabled
    riscv64  [none,+zfhmin] fadd.h f1, f2, f3      REFUSED riscv64.feature: fadd.h requires feature zfh, which is not enabled
    riscv64  [none,+zfhmin] flh f1, 0(x2)          87 10 01 00
    riscv64  [none,+zfhmin] fadd.q f1, f2, f3      REFUSED riscv64.feature: fadd.q requires feature q, which is not enabled
    riscv64  [none,+q  ] fli.s f1, 1.0          REFUSED riscv64.feature: fli.s requires feature zfa, which is not enabled
    riscv64  [none,+q  ] fli.d f1, 1.0          REFUSED riscv64.feature: fli.d requires feature zfa, which is not enabled
    riscv64  [none,+q  ] fli.h f1, 1.0          REFUSED riscv64.feature: fli.h requires feature zfa and zfh, which is not enabled
    riscv64  [none,+q  ] fminm.q f1, f2, f3     REFUSED riscv64.feature: fminm.q requires feature zfa, which is not enabled
    riscv64  [none,+q  ] fcvt.d.h f1, f2        REFUSED riscv64.feature: fcvt.d.h requires feature zfhmin, which is not enabled
    riscv64  [none,+q  ] fadd.h f1, f2, f3      REFUSED riscv64.feature: fadd.h requires feature zfh, which is not enabled
    riscv64  [none,+q  ] flh f1, 0(x2)          REFUSED riscv64.feature: flh requires feature zfhmin, which is not enabled
    riscv64  [none,+q  ] fadd.q f1, f2, f3      d3 70 31 06 |}]

let%expect_test "Zfh implies Zfhmin" =
  List.iter
    (fun text ->
      match Target_config.resolve Riscv64_encode.components (spec text) with
      | Ok c -> Printf.printf "%-18S -> %s\n" text (String.concat "," (Target_config.features c))
      | Error m -> Printf.printf "%-18S -> error: %s\n" text m)
    [ "none,+zfh"; "none,+zfh,-zfhmin"; "none,+zfhmin"; "none,+q"; "none,+zfa" ];
  [%expect
    {|
    "none,+zfh"        -> f,zfh,zfhmin
    "none,+zfh,-zfhmin" -> error: feature zfhmin is implied by zfh, which is enabled
    "none,+zfhmin"     -> f,zfhmin
    "none,+q"          -> d,f,q
    "none,+zfa"        -> f,zfa |}]

(* {1 Bit manipulation and scalar crypto} *)

let%expect_test "Zbb, Zbkb, Zk: shared forms accept any providing extension" =
  List.iter
    (fun features ->
      List.iter (show ~features "riscv64")
        [
          "andn x5, x6, x7";
          "min x5, x6, x7";
          "pack x5, x6, x7";
          "clmul x5, x6, x7";
          "aes64ks1i x5, x6, 3";
          "sha256sig0 x5, x6";
          "sh1add x5, x6, x7";
          "czero.eqz x5, x6, x7";
        ])
    [ "none,+zbb"; "none,+zbkb"; "none,+zk"; "none,+zks"; "none,+b"; "none" ];
  [%expect
    {|
    riscv64  [none,+zbb] andn x5, x6, x7        b3 72 73 40
    riscv64  [none,+zbb] min x5, x6, x7         b3 42 73 0a
    riscv64  [none,+zbb] pack x5, x6, x7        REFUSED riscv64.feature: pack requires feature zbkb, which is not enabled
    riscv64  [none,+zbb] clmul x5, x6, x7       REFUSED riscv64.feature: clmul requires feature one of zbc, zbkc, which is not enabled
    riscv64  [none,+zbb] aes64ks1i x5, x6, 3    REFUSED riscv64.feature: aes64ks1i requires feature one of zknd, zkne, which is not enabled
    riscv64  [none,+zbb] sha256sig0 x5, x6      REFUSED riscv64.feature: sha256sig0 requires feature zknh, which is not enabled
    riscv64  [none,+zbb] sh1add x5, x6, x7      REFUSED riscv64.feature: sh1add requires feature zba, which is not enabled
    riscv64  [none,+zbb] czero.eqz x5, x6, x7   REFUSED riscv64.feature: czero.eqz requires feature zicond, which is not enabled
    riscv64  [none,+zbkb] andn x5, x6, x7        b3 72 73 40
    riscv64  [none,+zbkb] min x5, x6, x7         REFUSED riscv64.feature: min requires feature zbb, which is not enabled
    riscv64  [none,+zbkb] pack x5, x6, x7        b3 42 73 08
    riscv64  [none,+zbkb] clmul x5, x6, x7       REFUSED riscv64.feature: clmul requires feature one of zbc, zbkc, which is not enabled
    riscv64  [none,+zbkb] aes64ks1i x5, x6, 3    REFUSED riscv64.feature: aes64ks1i requires feature one of zknd, zkne, which is not enabled
    riscv64  [none,+zbkb] sha256sig0 x5, x6      REFUSED riscv64.feature: sha256sig0 requires feature zknh, which is not enabled
    riscv64  [none,+zbkb] sh1add x5, x6, x7      REFUSED riscv64.feature: sh1add requires feature zba, which is not enabled
    riscv64  [none,+zbkb] czero.eqz x5, x6, x7   REFUSED riscv64.feature: czero.eqz requires feature zicond, which is not enabled
    riscv64  [none,+zk ] andn x5, x6, x7        b3 72 73 40
    riscv64  [none,+zk ] min x5, x6, x7         REFUSED riscv64.feature: min requires feature zbb, which is not enabled
    riscv64  [none,+zk ] pack x5, x6, x7        b3 42 73 08
    riscv64  [none,+zk ] clmul x5, x6, x7       b3 12 73 0a
    riscv64  [none,+zk ] aes64ks1i x5, x6, 3    93 12 33 31
    riscv64  [none,+zk ] sha256sig0 x5, x6      93 12 23 10
    riscv64  [none,+zk ] sh1add x5, x6, x7      REFUSED riscv64.feature: sh1add requires feature zba, which is not enabled
    riscv64  [none,+zk ] czero.eqz x5, x6, x7   REFUSED riscv64.feature: czero.eqz requires feature zicond, which is not enabled
    riscv64  [none,+zks] andn x5, x6, x7        b3 72 73 40
    riscv64  [none,+zks] min x5, x6, x7         REFUSED riscv64.feature: min requires feature zbb, which is not enabled
    riscv64  [none,+zks] pack x5, x6, x7        b3 42 73 08
    riscv64  [none,+zks] clmul x5, x6, x7       b3 12 73 0a
    riscv64  [none,+zks] aes64ks1i x5, x6, 3    REFUSED riscv64.feature: aes64ks1i requires feature one of zknd, zkne, which is not enabled
    riscv64  [none,+zks] sha256sig0 x5, x6      REFUSED riscv64.feature: sha256sig0 requires feature zknh, which is not enabled
    riscv64  [none,+zks] sh1add x5, x6, x7      REFUSED riscv64.feature: sh1add requires feature zba, which is not enabled
    riscv64  [none,+zks] czero.eqz x5, x6, x7   REFUSED riscv64.feature: czero.eqz requires feature zicond, which is not enabled
    riscv64  [none,+b  ] andn x5, x6, x7        b3 72 73 40
    riscv64  [none,+b  ] min x5, x6, x7         b3 42 73 0a
    riscv64  [none,+b  ] pack x5, x6, x7        REFUSED riscv64.feature: pack requires feature zbkb, which is not enabled
    riscv64  [none,+b  ] clmul x5, x6, x7       REFUSED riscv64.feature: clmul requires feature one of zbc, zbkc, which is not enabled
    riscv64  [none,+b  ] aes64ks1i x5, x6, 3    REFUSED riscv64.feature: aes64ks1i requires feature one of zknd, zkne, which is not enabled
    riscv64  [none,+b  ] sha256sig0 x5, x6      REFUSED riscv64.feature: sha256sig0 requires feature zknh, which is not enabled
    riscv64  [none,+b  ] sh1add x5, x6, x7      b3 22 73 20
    riscv64  [none,+b  ] czero.eqz x5, x6, x7   REFUSED riscv64.feature: czero.eqz requires feature zicond, which is not enabled
    riscv64  [none     ] andn x5, x6, x7        REFUSED riscv64.feature: andn requires feature one of zbb, zbkb, which is not enabled
    riscv64  [none     ] min x5, x6, x7         REFUSED riscv64.feature: min requires feature zbb, which is not enabled
    riscv64  [none     ] pack x5, x6, x7        REFUSED riscv64.feature: pack requires feature zbkb, which is not enabled
    riscv64  [none     ] clmul x5, x6, x7       REFUSED riscv64.feature: clmul requires feature one of zbc, zbkc, which is not enabled
    riscv64  [none     ] aes64ks1i x5, x6, 3    REFUSED riscv64.feature: aes64ks1i requires feature one of zknd, zkne, which is not enabled
    riscv64  [none     ] sha256sig0 x5, x6      REFUSED riscv64.feature: sha256sig0 requires feature zknh, which is not enabled
    riscv64  [none     ] sh1add x5, x6, x7      REFUSED riscv64.feature: sh1add requires feature zba, which is not enabled
    riscv64  [none     ] czero.eqz x5, x6, x7   REFUSED riscv64.feature: czero.eqz requires feature zicond, which is not enabled |}]

let%expect_test "K groups are sums of their leaf extensions" =
  List.iter
    (fun text ->
      match Target_config.resolve Riscv64_encode.components (spec text) with
      | Ok c -> Printf.printf "%-14S -> %s\n" text (String.concat "," (Target_config.features c))
      | Error m -> Printf.printf "%-14S -> error: %s\n" text m)
    [ "none,+zk"; "none,+zks"; "none,+b"; "none,+zk,-zbkb"; "none,+zbc,-zbkc" ];
  [%expect
    {|
    "none,+zk"     -> zbkb,zbkc,zbkx,zk,zkn,zknd,zkne,zknh
    "none,+zks"    -> zbkb,zbkc,zbkx,zks,zksed,zksh
    "none,+b"      -> b,zba,zbb,zbs
    "none,+zk,-zbkb" -> error: feature zbkb is implied by zkn, which is enabled
    "none,+zbc,-zbkc" -> zbc |}]
