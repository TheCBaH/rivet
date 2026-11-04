(* Per target: the bytes text assembles to, the bytes an [Instruction.t] encodes to, the text bytes
   decode to, and whether an instruction and its text agree. *)

let driver target =
  match Driver.Registry.find target with Some d -> d | None -> invalid_arg target

let render ds =
  String.concat "; "
    (List.map
       (fun d ->
         Printf.sprintf "%s: %s" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
       (Foundation.Diag.diagnostics ds))

let hex b =
  String.concat " " (List.init (String.length b) (fun i -> Printf.sprintf "%02x" (Char.code b.[i])))

(* The bytes of one instruction's text, laid out at 0x1000. *)
let spec features =
  Option.map
    (fun f ->
      match Target_config.parse_spec f with Ok s -> s | Error m -> invalid_arg ("features: " ^ m))
    features

let assemble ?features target text =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  let source = Foundation.Span.source ~name:"<asm>" ~contents:("\t.text\n\t" ^ text ^ "\n") in
  match D.assemble ?features:(spec features) ~unit_name:"t" ~source () with
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

let decode ?features target bytes =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  match D.dump_disasm_canonical ?features:(spec features) ~address:0x1000L bytes with
  | Ok t -> Ok (String.trim t)
  | Error ds -> Error (render ds)

(* [E.name] is the target's driver name. *)
module Make (E : Target_encode.ENCODE) = struct
  let message e = Foundation.Diagnostic.message (E.error_diagnostic (Err.Error.kind e))

  (* Every lowered form's bytes in order; a relaxable form takes its first rung. *)
  (* [features] as the driver takes them, e.g. ["+zbs,+zicond"]. *)
  let state features =
    match spec features with
    | None -> E.default_state
    | Some s -> (
        match Target_config.resolve E.components s with
        | Ok c -> E.initial_state c
        | Error m -> invalid_arg ("features: " ^ m))

  let encode ?features insn =
    match E.lower_instruction (state features) insn with
    | Error e -> Error (message e)
    | Ok ls ->
        List.fold_left
          (fun acc l ->
            Result.bind acc (fun b ->
                match E.encode l with
                | Ok (`Fixed f) | Ok (`Relax (f :: _)) -> Ok (b ^ f.Asm_core.Lowered_ast.bytes)
                | Ok (`Relax []) -> Error "no rung"
                | Error e -> Error (message e)))
          (Ok "") ls

  let assemble ?features = assemble ?features E.name
  let decode ?features = decode ?features E.name

  let same_bytes ?features insn text =
    match (encode ?features insn, assemble ?features text) with
    | Ok a, Ok b -> String.equal a b
    | _ -> false

  (* One line: the text, its bytes, and whether the instruction encodes to them. *)
  let check ?features insn text =
    let verdict, bytes =
      match (encode ?features insn, assemble ?features text) with
      | Ok a, Ok b when String.equal a b -> ("same", hex b)
      | Ok a, Ok b -> ("DIFFERENT " ^ hex a, hex b)
      | Error m, _ -> ("ENCODE " ^ m, "")
      | _, Error m -> ("ASSEMBLE " ^ m, "")
    in
    Printf.printf "%-8s %-28s %-24s %s\n" E.name text bytes verdict
end
