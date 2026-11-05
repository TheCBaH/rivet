(* The x86 table: every builder's sample, at every width and length it takes, encodes like its
   text in each mode it exists in, and those bytes read back as text that assembles to them again.
   A target is the label after the instruction, so a branch's displacement is zero; where the text
   reads it as an absolute address, the bytes agree outside that address. *)

open X86_family_encode
module Ops = X86_ops

module type ENC = module type of X86_64_encode

module Check
    (E : ENC)
    (B : sig
      val bits : int
    end) =
struct
  module H = Harness.Make (E)

  let gpr8 = if B.bits = 32 then [| 2; 3; 0; 1 |] else [| 2; 3; 6; 7 |]

  let gpr (cls : Rclass.t) i =
    { Reg.cls; num = (match cls with Gpr8 -> gpr8.(i) | _ -> [| 2; 3; 6; 7 |].(i)) }

  let base = { Reg.cls = (if B.bits = 64 then Rclass.Gpr64 else Gpr32); num = 1 }
  let rs = { Ops.Sample.gpr; base }

  let here (m : Ops.mode) =
    match m with Both -> true | Only32 -> B.bits = 32 | Only64 -> B.bits = 64

  let suffix = function 8 -> "b" | 16 -> "w" | 32 -> "l" | 64 -> "q" | _ -> ""

  let text (b : Ops.builder) (i : Instruction.t) ~width ~length =
    let vec l = match l with 256 -> "ymm" | 512 -> "zmm" | _ -> "xmm" in
    let reg r = "%" ^ Reg.name r in
    let wcls () = Option.get (Rclass.of_width width) in
    let operand n : Ops.Kind.t -> string = function
      | R -> reg (gpr (wcls ()) n)
      | R8 -> reg (gpr Gpr8 n)
      | R16 -> reg (gpr Gpr16 n)
      | R32 -> reg (gpr Gpr32 n)
      | R64 -> reg (gpr Gpr64 n)
      | V -> Printf.sprintf "%%%s%d" (vec length) (1 + n)
      | X -> Printf.sprintf "%%xmm%d" (1 + n)
      | Y -> Printf.sprintf "%%ymm%d" (1 + n)
      | Z -> Printf.sprintf "%%zmm%d" (1 + n)
      | Mm -> Printf.sprintf "%%mm%d" (1 + n)
      | K -> Printf.sprintf "%%k%d" (1 + n)
      | St0 -> "%st"
      | Sti -> Printf.sprintf "%%st(%d)" (1 + n)
      | M -> "(" ^ reg base ^ ")"
      | I | I32 | I64 -> "$1"
      | S -> "l"
      | Cl -> "%cl"
      | Ax -> "%ax"
      | Dx -> "%dx"
    in
    let m =
      match String.index_opt b.spelling '%' with
      | Some k -> String.sub b.spelling 0 k ^ suffix i.width
      | None -> b.spelling
    in
    let ops = List.mapi operand b.kinds in
    if ops = [] then m else m ^ " " ^ String.concat ", " ops

  let encode i = match H.encode i with Ok b -> Some b | Error _ -> None

  let debug fmt =
    Printf.ksprintf
      (fun m -> if Sys.getenv_opt "X86_DEBUG" <> None then print_endline ("DBG " ^ m))
      fmt

  (* The bytes outside an absolute address: the last four. *)
  let outside_address b = String.sub b 0 (max 0 (String.length b - 4))

  let round_trip t bytes ~label =
    match H.decode bytes with
    | Error _ -> `Undecoded
    | Ok back -> (
        let text = "\t" ^ String.concat "\n\t" (String.split_on_char '\n' back) in
        match H.assemble (if label then text ^ "\nl:" else text) with
        | Ok b' when String.equal b' bytes -> `Same
        | r ->
            debug "%s | %s | %s | %s" t (Harness.hex bytes) back
              (match r with Ok b' -> Harness.hex b' | Error m -> m);
            `Not_round_trip)

  (* [`Same], or what went wrong *)
  let check (b : Ops.builder) ~width ~length =
    let i = b.sample rs ~address:B.bits ~width ~length in
    let t = text b i ~width ~length in
    match H.encode i with
    | Error m ->
        debug "%s | %s" t m;
        `Unlowered
    | Ok bytes -> (
        let sym = List.mem Ops.Kind.S b.kinds in
        match H.assemble (if sym then "\t" ^ t ^ "\nl:" else "\t" ^ t) with
        | Error m ->
            debug "%s | %s" t m;
            `Unassembled
        | Ok b' when String.equal b' bytes -> `Same (Some (t, bytes, sym))
        | Ok b'
          when sym
               && String.length b' = String.length bytes
               && String.equal (outside_address b') (outside_address bytes) ->
            `Same None
        | Ok b' ->
            debug "%s | %s | %s" t (Harness.hex bytes) (Harness.hex b');
            `Different)

  let cases =
    List.concat_map
      (fun (e : Ops.entry) ->
        List.concat_map
          (fun (b : Ops.builder) ->
            if not (here b.bmode) then []
            else
              let ws = if b.widths = [] then [ 32 ] else b.widths in
              let ls = if b.lengths = [] then [ 128 ] else b.lengths in
              List.concat_map
                (fun width ->
                  if B.bits = 32 && width = 64 then []
                  else List.map (fun length -> (e, b, width, length)) ls)
                ws)
          e.builders)
      (Array.to_list Ops.all)

  let results =
    List.map (fun (e, b, width, length) -> ((e, b, width, length), check b ~width ~length)) cases

  let label ((b : Ops.builder), w, l) =
    b.name
    ^ (if List.length b.widths > 1 then "/" ^ string_of_int w else "")
    ^ if List.length b.lengths > 1 then "/" ^ string_of_int l else ""

  let report name =
    let only k =
      String.concat " "
        (List.filter_map
           (fun ((_, b, w, l), r) -> if r = k then Some (label (b, w, l)) else None)
           results)
    in
    Printf.printf
      "%s: %d samples, same %d\n  unlowered [%s]\n  unassembled [%s]\n  different [%s]\n" name
      (List.length cases)
      (List.length
         (List.filter (fun (_, r) -> match r with `Same _ -> true | _ -> false) results))
      (only `Unlowered) (only `Unassembled) (only `Different)

  (* The opcodes some sample of which does not read back. *)
  let report_round_trip name =
    let fails k =
      List.sort_uniq compare
        (List.filter_map
           (fun (((e : Ops.entry), _, _, _), r) ->
             match r with
             | `Same (Some (t, bytes, label)) when round_trip t bytes ~label = k ->
                 Some (Opcode.name e.op)
             | _ -> None)
           results)
    in
    let undecoded = fails `Undecoded and not_back = fails `Not_round_trip in
    Printf.printf "%s: undecoded %d [%s]\n  not read back %d [%s]\n" name (List.length undecoded)
      (String.concat " " undecoded) (List.length not_back) (String.concat " " not_back)
end

let%expect_test "every builder's sample encodes like its text in both modes" =
  let module C32 =
    Check
      (X86_32_encode)
      (struct
        let bits = 32
      end)
  in
  let module C64 =
    Check
      (X86_64_encode)
      (struct
        let bits = 64
      end)
  in
  C32.report "x86_32";
  C64.report "x86_64";
  [%expect
    {|
    x86_32: 1800 samples, same 1800
      unlowered []
      unassembled []
      different []
    x86_64: 2116 samples, same 2116
      unlowered []
      unassembled []
      different [] |}]

let%expect_test "decoding a sample reads back as text with the same bytes" =
  let module C32 =
    Check
      (X86_32_encode)
      (struct
        let bits = 32
      end)
  in
  let module C64 =
    Check
      (X86_64_encode)
      (struct
        let bits = 64
      end)
  in
  C32.report_round_trip "x86_32";
  C64.report_round_trip "x86_64";
  [%expect
    {|
    x86_32: undecoded 0 []
      not read back 0 []
    x86_64: undecoded 7 [adc add and or sbb sub xor]
      not read back 2 [fadds fsubs] |}]

(* What the encoder reads from the table: each table names one opcode per byte, the x87 component
   publishes exactly the opcodes the table gives it, and the gated encoder agrees. *)
let%expect_test "the encoder's views of the table" =
  let module E = X86_64_encode in
  let clashes = ref [] in
  Array.iter
    (fun (e : Ops.entry) ->
      List.iter
        (fun (f, n) ->
          match f with
          | Ops.Form.Xmm_shift_ext | Xmm_shift_opcode | Vex_shift_ext | Vex_shift_opcode -> ()
          | _ ->
              if Ops.of_code f n <> Some e.op then
                clashes := (Ops.Form.name f ^ " " ^ Opcode.name e.op) :: !clashes)
        e.forms;
      match Ops.shift_ext_opcode e.op with
      | Some (x, o) ->
          let back =
            if Ops.code Ops.Form.Vex_shift_ext e.op <> None then Ops.of_vex_shift ~opcode:o x
            else Ops.of_xmm_shift ~opcode:o x
          in
          if back <> Some e.op then clashes := ("shift " ^ Opcode.name e.op) :: !clashes
      | None -> ())
    Ops.all;
  let x87_feature = (List.hd E.components).Target_component.feature in
  let x87 = Ops.of_feature x87_feature in
  Printf.printf "%d entries, %d opcodes; clashes [%s]; x87 %d, gated %b\n" (Array.length Ops.all)
    (Array.length Opcode.all)
    (String.concat ", " (List.rev !clashes))
    (List.length x87)
    (List.for_all (fun op -> E.op_feature op = Some x87_feature) x87);
  [%expect {| 463 entries, 463 opcodes; clashes []; x87 13, gated true |}]
