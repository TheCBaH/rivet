(* Vendor extensions composed into the RISC-V encoder. Each composition is validated once by
   [Extensions.make]; two compositions in one program do not see each other's mnemonics, and the
   standard registry is untouched. *)

module Row = Riscv_family_encode.Riscv_table_row
module Ext = Riscv_family_encode.Extensions

let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

(* The list of known features in a configuration error would make every transcript depend on the
   whole registry, so it is cut off. *)
let render ds =
  let marker = " (known:" in
  let cut m =
    let n = String.length m and k = String.length marker in
    let rec find i =
      if i + k > n then m else if String.sub m i k = marker then String.sub m 0 i else find (i + 1)
    in
    find 0
  in
  String.concat "; "
    (List.map
       (fun d ->
         Printf.sprintf "%s: %s" (Foundation.Diagnostic.code d)
           (cut (Foundation.Diagnostic.message d)))
       (Foundation.Diag.diagnostics ds))

(* A custom-0 R-type instruction [mnemonic rd, rs1, rs2] told apart by funct3. *)
let row ?(opcode = 0x0b) ?(feature = "") ~funct3 mnemonic : Row.row =
  let gpr lsb = Row.Gpr { lsb; nonzero = false } in
  {
    mnemonic;
    mask = 0xfe00707fL;
    match_ = Int64.of_int (opcode lor (funct3 lsl 12));
    operands = [ gpr 7; gpr 15; gpr 20 ];
    xlen = 0;
    feature = (if feature = "" then mnemonic else feature);
    source = "vendor/" ^ mnemonic;
  }

let component ?(default_on = false) feature mnemonics : Target_component.t =
  {
    id = "vendor." ^ feature;
    feature;
    requires = [];
    conflicts = [];
    implies = [];
    default_on;
    xlens = [];
    not_applicable = None;
    summary = "";
    forms =
      List.map
        (fun m ->
          {
            Target_component.label = m;
            mnemonics = [ m ];
            sources = [ { upstream = "vendor"; name = m } ];
            legal = None;
          })
        mnemonics;
  }

let vendor mnemonic funct3 : Ext.definition =
  { components = [ component mnemonic [ mnemonic ] ]; rows = [| row ~funct3 mnemonic |] }

let%expect_test "invalid definitions are reported by make, all at once" =
  let try_make label ~xlen (def : Ext.definition) =
    match Ext.make ~xlen def with
    | Ok _ -> Printf.printf "%s: ok\n" label
    | Error ps -> List.iter (Printf.printf "%s: %s\n" label) ps
  in
  try_make "valid" ~xlen:64 (vendor "xfoo" 0);
  try_make "handwritten" ~xlen:64 (vendor "add" 0);
  try_make "standard row" ~xlen:64 (vendor "zext.w" 0);
  try_make "opcode space" ~xlen:64
    { (vendor "xfoo" 0) with rows = [| row ~opcode:0x33 ~funct3:0 "xfoo" |] };
  try_make "on by default" ~xlen:64
    { (vendor "xfoo" 0) with components = [ component ~default_on:true "xfoo" [ "xfoo" ] ] };
  try_make "overlap" ~xlen:64
    {
      components = [ component "xfoo" [ "xfoo"; "xbar" ] ];
      rows = [| row ~funct3:0 ~feature:"xfoo" "xfoo"; row ~funct3:0 ~feature:"xfoo" "xbar" |];
    };
  try_make "no owner" ~xlen:64 { (vendor "xfoo" 0) with components = [] };
  try_make "unknown feature" ~xlen:64
    { (vendor "xfoo" 0) with rows = [| row ~funct3:0 ~feature:"nope" "xfoo" |] };
  [%expect
    {|
    valid: ok
    handwritten: row add: collides with a hand-written form
    standard row: duplicate form label zext.w
    standard row: duplicate mnemonic zext.w
    standard row: row zext.w: collides with a standard generated form
    opcode space: row xfoo: must lie in the custom-0..3 opcode space
    on by default: vendor.xfoo: a vendor component must not be on by default
    overlap: rows xfoo and xbar overlap
    no owner: row xfoo: feature xfoo is not a vendor component
    no owner: row xfoo: no vendor component lists it
    unknown feature: row xfoo: feature nope is not a vendor component |}]

module P64 = struct
  let name = "riscv64"
  let triple = "riscv64-linux-gnu"
  let xlen = 64
end

let validated mnemonic funct3 =
  match Ext.make ~xlen:64 (vendor mnemonic funct3) with
  | Ok v -> v
  | Error ps -> failwith (String.concat "; " ps)

module Foo =
  Riscv_family.Make_with
    (P64)
    (struct
      let definition = validated "xfoo" 0
    end)

module Bar =
  Riscv_family.Make_with
    (P64)
    (struct
      let definition = validated "xbar" 1
    end)

module Foo_driver = Driver.Pipeline.Make (Foo)
module Bar_driver = Driver.Pipeline.Make (Bar)

let assembled (module D : Target_intf.Target.DRIVER) ?features text =
  let features =
    Option.map
      (fun t ->
        match Target_config.parse_spec t with Ok s -> s | Error m -> failwith ("bad spec: " ^ m))
      features
  in
  let source = Foundation.Span.source ~name:"<test>" ~contents:("\t.text\n\t" ^ text ^ "\n") in
  match D.assemble ?features ~unit_name:"t" ~source () with
  | Error ds -> "REFUSED " ^ render ds
  | Ok laid_out -> (
      let plan = Image.plan_of laid_out in
      let addresses =
        List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0x1000L)) plan.Image.segments
      in
      match Image.bind_image laid_out ~addresses with
      | Error ds -> "REFUSED " ^ render ds
      | Ok img -> (
          match img.Image.segments with s :: _ -> hex s.Image.bytes | [] -> "no segments"))

let%expect_test "two compositions coexist and keep the standard instructions" =
  let foo = (module Foo_driver : Target_intf.Target.DRIVER) in
  let bar = (module Bar_driver : Target_intf.Target.DRIVER) in
  let standard =
    match Driver.Registry.find "riscv64" with Some d -> d | None -> failwith "riscv64"
  in
  List.iter
    (fun (label, driver) ->
      List.iter
        (fun (features, text) ->
          Printf.printf "%-9s %-12s %-18s %s\n" label
            (Option.value features ~default:"default")
            text (assembled driver ?features text))
        [
          (None, "xfoo x5, x6, x7");
          (Some "", "xfoo x5, x6, x7");
          (Some "-m", "xfoo x5, x6, x7");
          (Some "none", "xfoo x5, x6, x7");
          (Some "+xfoo", "xfoo x5, x6, x7");
          (Some "none,+xfoo", "xfoo x5, x6, x7");
          (Some "+xfoo", "xbar x5, x6, x7");
          (Some "+xbar", "xbar x5, x6, x7");
          (Some "+xfoo", "add x5, x6, x7");
        ])
    [ ("foo", foo); ("bar", bar); ("standard", standard) ];
  [%expect
    {|
    foo       default      xfoo x5, x6, x7    REFUSED riscv64.feature: xfoo requires feature xfoo, which is not enabled
    foo                    xfoo x5, x6, x7    REFUSED riscv64.feature: xfoo requires feature xfoo, which is not enabled
    foo       -m           xfoo x5, x6, x7    REFUSED riscv64.feature: xfoo requires feature xfoo, which is not enabled
    foo       none         xfoo x5, x6, x7    REFUSED riscv64.feature: xfoo requires feature xfoo, which is not enabled
    foo       +xfoo        xfoo x5, x6, x7    8b 02 73 00
    foo       none,+xfoo   xfoo x5, x6, x7    8b 02 73 00
    foo       +xfoo        xbar x5, x6, x7    REFUSED riscv64.simplify: unknown instruction xbar
    foo       +xbar        xbar x5, x6, x7    REFUSED config.invalid: invalid feature configuration: unknown feature xbar
    foo       +xfoo        add x5, x6, x7     b3 02 73 00
    bar       default      xfoo x5, x6, x7    REFUSED riscv64.simplify: unknown instruction xfoo
    bar                    xfoo x5, x6, x7    REFUSED riscv64.simplify: unknown instruction xfoo
    bar       -m           xfoo x5, x6, x7    REFUSED riscv64.simplify: unknown instruction xfoo
    bar       none         xfoo x5, x6, x7    REFUSED riscv64.simplify: unknown instruction xfoo
    bar       +xfoo        xfoo x5, x6, x7    REFUSED config.invalid: invalid feature configuration: unknown feature xfoo
    bar       none,+xfoo   xfoo x5, x6, x7    REFUSED config.invalid: invalid feature configuration: unknown feature xfoo
    bar       +xfoo        xbar x5, x6, x7    REFUSED config.invalid: invalid feature configuration: unknown feature xfoo
    bar       +xbar        xbar x5, x6, x7    8b 12 73 00
    bar       +xfoo        add x5, x6, x7     REFUSED config.invalid: invalid feature configuration: unknown feature xfoo
    standard  default      xfoo x5, x6, x7    REFUSED riscv64.simplify: unknown instruction xfoo
    standard               xfoo x5, x6, x7    REFUSED riscv64.simplify: unknown instruction xfoo
    standard  -m           xfoo x5, x6, x7    REFUSED riscv64.simplify: unknown instruction xfoo
    standard  none         xfoo x5, x6, x7    REFUSED riscv64.simplify: unknown instruction xfoo
    standard  +xfoo        xfoo x5, x6, x7    REFUSED config.invalid: invalid feature configuration: unknown feature xfoo
    standard  none,+xfoo   xfoo x5, x6, x7    REFUSED config.invalid: invalid feature configuration: unknown feature xfoo
    standard  +xfoo        xbar x5, x6, x7    REFUSED config.invalid: invalid feature configuration: unknown feature xfoo
    standard  +xbar        xbar x5, x6, x7    REFUSED config.invalid: invalid feature configuration: unknown feature xbar
    standard  +xfoo        add x5, x6, x7     REFUSED config.invalid: invalid feature configuration: unknown feature xfoo |}]

let%expect_test "the configured codec and encoder agree on the golden words" =
  let state =
    match
      Target_config.resolve Foo.components (Result.get_ok (Target_config.parse_spec "+xfoo"))
    with
    | Ok c -> Foo.initial_state c
    | Error m -> failwith m
  in
  let lowered = Foo.Lowered.Fixed { name = "xfoo"; word = 0x0073028bL } in
  (match Foo.encode_in state lowered with
  | Ok (`Fixed f) -> Printf.printf "encode_in: %s\n" (hex f.Asm_core.Lowered_ast.bytes)
  | Ok _ -> print_endline "encode_in: relax"
  | Error _ -> print_endline "encode_in: refused");
  (match Foo.encode_in Foo.default_state lowered with
  | Ok _ -> print_endline "default: accepted"
  | Error _ -> print_endline "default: refused");
  let codec = Foo.codec_in state in
  (match Codec.encode codec lowered with
  | Ok e -> (
      Printf.printf "codec_in: %s\n" (Codec.Bits.to_string e.Codec.bits);
      match Codec.decode_bits codec e.bits with
      | Some d -> Printf.printf "decoded: %s\n" (Fmt.to_to_string Foo.Lowered.pp d.Codec.value)
      | None -> print_endline "decoded: none")
  | Error _ -> print_endline "codec_in: refused");
  [%expect
    {|
    encode_in: 8b 02 73 00
    default: refused
    codec_in: 00000000011100110000001010001011
    decoded: xfoo |}]

let%expect_test "inspection names the capability a disabled vendor form needs" =
  let code = "\x8b\x02\x73\x00" in
  List.iter
    (fun inspect ->
      match Foo_driver.dump_disasm_diagnostic ~inspect ~address:0x1000L code with
      | Ok text -> print_string text
      | Error ds -> Printf.printf "REFUSED %s\n" (render ds))
    [ false; true ];
  [%expect
    {|
    REFUSED riscv64.feature: xfoo requires feature xfoo, which is not enabled
    00001000  8b 02 73 00  xfoo x5, x6, x7  ; requires feature xfoo, which is not enabled  [riscv64.xfoo] |}]

let%expect_test "a composition validated for another XLEN is refused" =
  (match Ext.make ~xlen:32 (vendor "xfoo" 0) with
  | Error ps -> List.iter print_endline ps
  | Ok v -> (
      try
        let module _ =
          Riscv_family_encode.Make_with
            (P64)
            (struct
              let definition = v
            end)
        in
        print_endline "applied"
      with Invalid_argument m -> print_endline m));
  [%expect {| riscv64: extensions validated for XLEN 32, profile is XLEN 64 |}]
