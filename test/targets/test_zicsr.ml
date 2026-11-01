(* Zicsr words read back as text: each line assembles, decodes to the same text, and that text
   assembles to the same word. The aliases are the ones the assembler takes. *)

let driver target = match Driver.Registry.find target with Some d -> d | None -> failwith target

let render ds =
  String.concat "; "
    (List.map
       (fun d ->
         Printf.sprintf "%s: %s" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
       (Foundation.Diag.diagnostics ds))

let spec text =
  match Target_config.parse_spec text with Ok s -> s | Error m -> failwith ("bad spec: " ^ m)

let assembled ?features target text =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  let source = Foundation.Span.source ~name:"<test>" ~contents:("\t.text\n\t" ^ text ^ "\n") in
  match D.assemble ?features:(Option.map spec features) ~unit_name:"t" ~source () with
  | Error ds -> Error (render ds)
  | Ok laid_out -> (
      let plan = Image.plan_of laid_out in
      let addresses =
        List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0x1000L)) plan.Image.segments
      in
      match Image.bind_image laid_out ~addresses with
      | Error ds -> Error (render ds)
      | Ok { Image.segments = s :: _; _ } -> Ok s.Image.bytes
      | Ok _ -> Error "no segments")

let word_bytes w = String.init 4 (fun i -> Char.chr ((w lsr (8 * i)) land 0xff))

let word_of_bytes b =
  let v = ref 0 in
  String.iteri (fun i c -> v := !v lor (Char.code c lsl (8 * i))) b;
  !v

let decoded ?features target bytes =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  match D.dump_disasm_canonical ?features:(Option.map spec features) ~address:0L bytes with
  | Ok t -> Ok (String.trim t)
  | Error ds -> Error (render ds)

let round_trip target text =
  match assembled target text with
  | Error m -> Printf.printf "%-28s REFUSED %s\n" text m
  | Ok bytes -> (
      let w = word_of_bytes bytes in
      match decoded target bytes with
      | Error m -> Printf.printf "%-28s %08x REFUSED %s\n" text w m
      | Ok back ->
          let same =
            match assembled target back with
            | Ok b -> if String.equal b bytes then "same" else "DIFFERENT"
            | Error m -> "REFUSED " ^ m
          in
          Printf.printf "%-28s %08x %-28s %s\n" text w back same)

let%expect_test "every Zicsr form decodes to the text the assembler takes back" =
  List.iter (round_trip "riscv32")
    [
      "csrrw x1, 0x340, x2";
      "csrrs x1, 0x340, x2";
      "csrrc x1, 0x340, x2";
      "csrrwi x1, 0x340, 3";
      "csrrsi x1, 0x340, 3";
      "csrrci x1, 0x340, 3";
      "csrrw x0, 0x340, x2";
      "csrrs x0, 0x340, x2";
      "csrrc x0, 0x340, x2";
      "csrrwi x0, 0x340, 3";
      "csrrsi x0, 0x340, 3";
      "csrrci x0, 0x340, 3";
      "csrrs x5, 0xb00, x0";
      "csrrs x0, 0xb00, x0";
      "csrrw x1, 0x340, x0";
      "csrrc x1, 0x340, x0";
      "csrrwi x1, 0x340, 0";
      "csrrsi x0, 0x340, 0";
      "csrr x10, 0xf14";
      "csrw 0x340, x5";
      "csrwi 0xb02, 0";
      "csrrs x10, 0xfff, x31";
      "csrrwi x31, 0x000, 31";
    ];
  [%expect
    {|
    csrrw x1, 0x340, x2          340110f3 csrrw x1, mscratch, x2       same
    csrrs x1, 0x340, x2          340120f3 csrrs x1, mscratch, x2       same
    csrrc x1, 0x340, x2          340130f3 csrrc x1, mscratch, x2       same
    csrrwi x1, 0x340, 3          3401d0f3 csrrwi x1, mscratch, 3       same
    csrrsi x1, 0x340, 3          3401e0f3 csrrsi x1, mscratch, 3       same
    csrrci x1, 0x340, 3          3401f0f3 csrrci x1, mscratch, 3       same
    csrrw x0, 0x340, x2          34011073 csrw mscratch, x2            same
    csrrs x0, 0x340, x2          34012073 csrs mscratch, x2            same
    csrrc x0, 0x340, x2          34013073 csrc mscratch, x2            same
    csrrwi x0, 0x340, 3          3401d073 csrwi mscratch, 3            same
    csrrsi x0, 0x340, 3          3401e073 csrsi mscratch, 3            same
    csrrci x0, 0x340, 3          3401f073 csrci mscratch, 3            same
    csrrs x5, 0xb00, x0          b00022f3 csrr x5, mcycle              same
    csrrs x0, 0xb00, x0          b0002073 csrrs x0, mcycle, x0         same
    csrrw x1, 0x340, x0          340010f3 csrrw x1, mscratch, x0       same
    csrrc x1, 0x340, x0          340030f3 csrrc x1, mscratch, x0       same
    csrrwi x1, 0x340, 0          340050f3 csrrwi x1, mscratch, 0       same
    csrrsi x0, 0x340, 0          34006073 csrsi mscratch, 0            same
    csrr x10, 0xf14              f1402573 csrr x10, mhartid            same
    csrw 0x340, x5               34029073 csrw mscratch, x5            same
    csrwi 0xb02, 0               b0205073 csrwi minstret, 0            same
    csrrs x10, 0xfff, x31        ffffa573 csrrs x10, 4095, x31         same
    csrrwi x31, 0x000, 31        000fdff3 csrrwi x31, 0, 31            same |}]

let%expect_test "words the assembler does not produce from a CSR mnemonic" =
  List.iter
    (fun w ->
      match decoded "riscv32" (word_bytes w) with
      | Ok t -> Printf.printf "%08x %s\n" w t
      | Error m -> Printf.printf "%08x REFUSED %s\n" w m)
    [ 0xc0001073; 0x00000073; 0x00100073; 0x30200073; 0x10500073; 0x00004073; 0x08000073 ];
  [%expect
    {|
    c0001073 unimp
    00000073 ecall
    00100073 ebreak
    30200073 mret
    10500073 wfi
    00004073 REFUSED riscv32.decode: no form matches this word
    08000073 REFUSED riscv32.decode: no form matches this word |}]

let%expect_test "with Zicsr disabled the word is refused, not printed" =
  List.iter
    (fun features ->
      match decoded ~features "riscv64" (word_bytes 0xb00022f3) with
      | Ok t -> Printf.printf "%-12s %s\n" features t
      | Error m -> Printf.printf "%-12s REFUSED %s\n" features m)
    [ "none"; "none,+zicsr"; "none,+m" ];
  [%expect
    {|
    none         REFUSED riscv64.feature: csrr requires feature zicsr, which is not enabled
    none,+zicsr  csrr x5, mcycle
    none,+m      REFUSED riscv64.feature: csrr requires feature zicsr, which is not enabled |}]

let%expect_test "CSR names: each assembles to its number and decodes back to its name" =
  let module Csr = Riscv_family_encode.Riscv_csr in
  let bad = ref 0 in
  List.iter
    (fun (name, n) ->
      let expected = 0x2573 lor (n lsl 20) in
      let ok =
        match assembled "riscv64" ("csrr x10, " ^ name) with
        | Ok b when word_of_bytes b = expected -> (
            match decoded "riscv64" b with
            | Ok t -> String.equal t ("csrr x10, " ^ name)
            | _ -> false)
        | _ -> false
      in
      if not ok then (
        incr bad;
        Printf.printf "%s %03x\n" name n))
    Csr.all;
  Printf.printf "%d names, %d bad, names unique: %b\n" (List.length Csr.all) !bad
    (List.length (List.sort_uniq compare (List.map snd Csr.all)) = List.length Csr.all);
  [%expect {| 295 names, 0 bad, names unique: true |}]

let%expect_test "CSR operands: named, numeric, unknown" =
  List.iter (round_trip "riscv32")
    [
      "csrr x5, mcycle";
      "csrwi minstret, 0";
      "csrrw x1, mscratch, x2";
      "csrr x5, 0x7c0";
      "csrr x5, nonsense";
    ];
  [%expect
    {|
    csrr x5, mcycle              b00022f3 csrr x5, mcycle              same
    csrwi minstret, 0            b0205073 csrwi minstret, 0            same
    csrrw x1, mscratch, x2       340110f3 csrrw x1, mscratch, x2       same
    csrr x5, 0x7c0               7c0022f3 csrr x5, 1984                same
    csrr x5, nonsense            REFUSED riscv32.lower: no csrr form takes these operands |}]

(* The words GNU as produced for the corpus's CSR cases decode, and the text assembles back to
   the same word. *)
let%expect_test "decoder agrees with the isa-difficult corpus" =
  let field line key =
    let k = Printf.sprintf "\"%s\":\"" key in
    let n = String.length k in
    let rec find i = if String.sub line i n = k then i + n else find (i + 1) in
    let start = find 0 in
    String.sub line start (String.index_from line start '"' - start)
  in
  let lines =
    In_channel.with_open_text "../../fixtures/isa-difficult/cases.jsonl" In_channel.input_all
  in
  let cases =
    List.filter
      (fun l -> String.length l > 0 && String.starts_with ~prefix:"riscv:csr" (field l "form_id"))
      (String.split_on_char '\n' lines)
  in
  let bad = ref 0 in
  List.iter
    (fun l ->
      let target = field l "target" in
      let hex = String.concat "" (String.split_on_char ' ' (String.sub (field l "bytes") 0 11)) in
      let bytes =
        String.init 4 (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))
      in
      let ok =
        match decoded target bytes with
        | Ok t -> ( match assembled target t with Ok b -> String.equal b bytes | Error _ -> false)
        | Error _ -> false
      in
      if not ok then (
        incr bad;
        Printf.printf "%s %s\n" (field l "case_id") hex))
    cases;
  Printf.printf "%d cases, %d bad\n" (List.length cases) !bad;
  [%expect {| 26 cases, 0 bad |}]
