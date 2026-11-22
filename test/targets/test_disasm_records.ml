(* [disassemble_records]: the records both disassembly dumps are rendered from. *)

module T = Target_intf.Target

let driver target = match Driver.Registry.find target with Some d -> d | None -> failwith target

let render ds =
  String.concat "; "
    (List.map
       (fun d ->
         Printf.sprintf "%s: %s" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
       (Foundation.Diag.diagnostics ds))

let spec text =
  match Target_config.parse_spec text with Ok s -> s | Error m -> failwith ("bad spec: " ^ m)

let assembled target text =
  let (module D : T.DRIVER) = driver target in
  let source = Foundation.Span.source ~name:"<test>" ~contents:("\t.text\n" ^ text) in
  match D.assemble ~unit_name:"t" ~source () with
  | Error ds -> failwith (render ds)
  | Ok laid_out -> (
      let plan = Image.plan_of laid_out in
      let addresses =
        List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0x1000L)) plan.Image.segments
      in
      match Image.bind_image laid_out ~addresses with
      | Ok { Image.segments = s :: _; _ } -> s.Image.bytes
      | Ok _ -> failwith "no segments"
      | Error ds -> failwith (render ds))

let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let show (r : T.disasm_record) =
  Printf.printf "  %08Lx  %-23s  %-22s  %s%s%s\n" r.address (hex r.bytes) r.text
    (Option.value r.form ~default:"padding")
    (match r.refusal with Some m -> "  refused: " ^ m | None -> "")
    (if r.alternatives = [] then "" else "  or: " ^ String.concat "; " r.alternatives)

let%expect_test "an instruction and a padding run on every target, and the dumps agree" =
  List.iter
    (fun (target, text) ->
      let (module D : T.DRIVER) = driver target in
      let bytes = assembled target text in
      match D.disassemble_records ~address:0x1000L bytes with
      | Error ds -> Printf.printf "%s: %s\n" target (render ds)
      | Ok records ->
          Printf.printf "%s\n" target;
          List.iter show records;
          let canonical =
            String.concat "" (List.map (fun (r : T.disasm_record) -> "\t" ^ r.text ^ "\n") records)
          in
          if D.dump_disasm_canonical ~address:0x1000L bytes <> Ok canonical then
            print_endline "  canonical dump differs")
    [
      ("x86_32", "\tpushl %ebp\n\t.balign 8\n\tret\n");
      ("x86_64", "\tpushq %rbp\n\t.balign 8\n\tret\n");
      ("arm", "\tadd r0, r0, #1\n\t.balign 16\n\tbx lr\n");
      ("aarch64", "\tadd x0, x0, #1\n\t.balign 16\n\tret\n");
      ("riscv32", "\taddi a0, a0, 1\n\t.balign 16\n\tret\n");
      ("riscv64", "\taddi a0, a0, 1\n\t.balign 16\n\tret\n");
    ];
  [%expect
    {|
    x86_32
      00001000  55                       push %ebp               x86_32.push-r.opsz-absent
      00001001  8d b4 26 00 00 00 00     .balign 8               padding
      00001008  c3                       ret                     x86_32.ret
    x86_64
      00001000  55                       push %rbp               x86_64.push-r.asz-absent.opsz-absent.rex-absent
      00001001  0f 1f 80 00 00 00 00     .balign 8               padding
      00001008  c3                       ret                     x86_64.ret
    arm
      00001000  01 00 80 e2              add r0, r0, #1          arm.dp-imm
      00001004  00 f0 20 e3 00 f0 20 e3 00 f0 20 e3  .balign 16              padding
      00001010  1e ff 2f e1              bx lr                   arm.bx
    aarch64
      00001000  00 04 00 91              add x0, x0, #1          aarch64.add-imm
      00001004  1f 20 03 d5 1f 20 03 d5 1f 20 03 d5  .balign 16              padding
      00001010  c0 03 5f d6              ret                     aarch64.ret
    riscv32
      00001000  13 05 15 00              addi x10, x10, 1        riscv32.addi
      00001004  13 00 00 00 13 00 00 00 13 00 00 00  .balign 16              padding
      00001010  67 80 00 00              jalr x0, 0(x1)          riscv32.jalr
      00001014  13 00 00 00 13 00 00 00 13 00 00 00  .balign 16              padding
    riscv64
      00001000  13 05 15 00              addi x10, x10, 1        riscv64.addi
      00001004  13 00 00 00 13 00 00 00 13 00 00 00  .balign 16              padding
      00001010  67 80 00 00              jalr x0, 0(x1)          riscv64.jalr
      00001014  13 00 00 00 13 00 00 00 13 00 00 00  .balign 16              padding |}]

let%expect_test "inspection records why the configuration refuses an instruction" =
  let (module D : T.DRIVER) = driver "riscv32" in
  let bytes = assembled "riscv32" "\tmul a0, a1, a2\n" in
  (match D.disassemble_records ~features:(spec "none") ~address:0L bytes with
  | Error ds -> Printf.printf "strict: %s\n" (render ds)
  | Ok rs -> List.iter show rs);
  (match D.disassemble_records ~features:(spec "none") ~inspect:true ~address:0L bytes with
  | Error ds -> Printf.printf "inspect: %s\n" (render ds)
  | Ok rs -> List.iter show rs);
  [%expect
    {|
    strict: riscv32.feature: mul requires feature zmmul, which is not enabled
      00000000  33 85 c5 02              mul x10, x11, x12       riscv32.mul  refused: requires feature zmmul, which is not enabled |}]

(* The symbol-labelled listing of a bound image: [.text] at 0x1000, [.data] at 0x2000. *)
let bound target text =
  let (module D : T.DRIVER) = driver target in
  let source = Foundation.Span.source ~name:"<test>" ~contents:text in
  match D.assemble ~unit_name:"t" ~source () with
  | Error ds -> failwith (render ds)
  | Ok laid_out -> (
      let plan = Image.plan_of laid_out in
      let addresses =
        List.map
          (fun (s : Image.segment_plan) ->
            (s.Image.seg_name, if s.Image.perms.Asm_core.Perms.execute then 0x1000L else 0x2000L))
          plan.Image.segments
      in
      match Image.bind_image laid_out ~addresses with
      | Ok image -> image
      | Error ds -> failwith (render ds))

let listing target text =
  let (module D : T.DRIVER) = driver target in
  let image = bound target text in
  let text_segment =
    List.find (fun (s : Image.segment) -> s.Image.perms.Asm_core.Perms.execute) image.Image.segments
  in
  Printf.printf "%s\n" target;
  match D.dump_listing image ~segment:text_segment.Image.name with
  | Ok s -> print_string s
  | Error ds -> print_endline (render ds)

let%expect_test "listings name symbols at their definitions and at the fixups that use them" =
  listing "x86_64"
    {|	.text
	.globl _start
_start:	call exp
	movl counter(%rip), %eax
	ret
exp:	ret
	.data
counter:	.long 7
|};
  listing "aarch64"
    {|	.text
	.globl _start
_start:	adrp x0, counter
	add x0, x0, :lo12:counter
	bl exp
	ret
exp:	ret
	.data
counter:	.word 7
|};
  listing "riscv32"
    {|	.text
	.globl _start
_start:	lui a0, %hi(counter + 4)
	addi a0, a0, %lo(counter + 4)
1:	addi a0, a0, -1
	bnez a0, 1b
	call exp
	ret
exp:	ret
	.data
counter:	.word 7, 8
|};
  [%expect
    {|
    x86_64
    _start:
      00001000  e8 07 00 00 00     call 4108              <exp>
      00001005  8b 05 f5 0f 00 00  movl 4085(%rip), %eax  <counter>
      0000100b  c3                 ret
    exp:
      0000100c  c3                 ret
    aarch64
    _start:
      00001000  00 00 00 b0  adrp x0, 8192   <counter>
      00001004  00 00 00 91  add x0, x0, #0  <counter>
      00001008  02 00 00 94  bl 4112         <exp>
      0000100c  c0 03 5f d6  ret
    exp:
      00001010  c0 03 5f d6  ret
    riscv32
    _start:
      00001000  37 25 00 00  lui x10, 2         <counter+0x4>
      00001004  13 05 45 00  addi x10, x10, 4   <counter+0x4>
    .Lrivet.1.0:
      00001008  13 05 f5 ff  addi x10, x10, -1
      0000100c  e3 1e 05 fe  bne x10, x0, 4104  <.Lrivet.1.0>
      00001010  97 00 00 00  auipc x1, 0        <exp>
      00001014  e7 80 c0 00  jalr x1, 12(x1)    <exp>
      00001018  67 80 00 00  jalr x0, 0(x1)
    exp:
      0000101c  67 80 00 00  jalr x0, 0(x1) |}]

let%expect_test "a listing of a segment the image does not have is an error" =
  let (module D : T.DRIVER) = driver "riscv32" in
  let image = bound "riscv32" "\t.text\n\tret\n" in
  (match D.dump_listing image ~segment:".nope" with
  | Ok s -> print_string s
  | Error ds -> print_endline (render ds));
  [%expect {| listing.segment: the image has no segment .nope |}]
