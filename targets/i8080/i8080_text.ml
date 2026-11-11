(* Intel-syntax text in, bytes at an origin out.

   The shared pipeline does the work; this module supplies the two things the
   8080 dialect needs that the core deliberately leaves to the host: a section
   (Intel sources have no [.text]) and an address ([org]). Both are decided from
   the pipeline's own public stages: the source AST gets an implicit [.text]
   when it does not start with a section directive, and the origin is read from
   the target's [org] pseudo-instruction in the normalized module, then handed
   to [Image.bind_image] as the section's address. *)

open Foundation
open Asm_core
module P = Pipeline.Make (I8080)

type image = { origin : int; bytes : string }

let with_text_section (m : I8080.Surface.t Asm_syntax.Source_ast.module_) =
  let open Asm_syntax.Source_ast in
  let starts_section = function
    | Directive { name; _ } -> List.mem name [ ".text"; ".data"; ".section" ]
    | _ -> false
  in
  match m.items with
  | first :: _ when starts_section first -> m
  | items ->
      {
        m with
        items =
          Directive { name = ".text"; arguments = []; origin = Origin.synthesized ~pass:"i8080" () }
          :: items;
      }

(* [org] must come first (nothing before it may occupy an address) and only
   once; absent means 0. *)
let origin_of (m : I8080.Instruction.t Normalized_ast.module_) =
  let fail origin =
    Diag.fail ~pos:__POS__ [ I8080.error_diagnostic (Target_error.make ~origin `Org_misplaced) ]
  in
  let rec go seen_content found = function
    | [] -> Ok (Option.value found ~default:0)
    | Normalized_ast.Label { origin; _ } :: rest ->
        ignore origin;
        go true found rest
    | Normalized_ast.Directive _ :: rest -> go seen_content found rest
    | Normalized_ast.Instruction { insn = I8080.Instruction.Org n; origin } :: rest ->
        if seen_content || found <> None then fail origin else go seen_content (Some n) rest
    | Normalized_ast.Instruction _ :: rest -> go true found rest
  in
  go false None m.Normalized_ast.items

let assemble ?(unit_name = "i8080") text =
  let open Err.Syntax in
  let stage r = Diag.stage ~pos:__POS__ Err.Action.Map r in
  let source = Span.source ~name:unit_name ~contents:text in
  let* state = P.initial_state None in
  let* src = stage (P.parse ~unit_name ~source) in
  let* norm, _ = stage (P.simplify ~state (with_text_section src)) in
  let* origin = origin_of norm in
  let* low = stage (P.lower ~state norm) in
  let* laid = stage (P.plan low) in
  let* img = I8080_asm.Program.bind ~origin laid in
  Ok { origin; bytes = img.I8080_asm.Program.bytes }

(* The canonical text of [bytes] loaded at [origin]: an [org] line, then one
   instruction per line. Feeding it back to {!assemble} yields the same image. *)
let canonical ~origin bytes =
  let open Err.Syntax in
  let* body = P.dump_disasm_canonical ~address:(Int64.of_int origin) bytes in
  Ok (Printf.sprintf "\torg 0x%04x\n%s" origin body)

let check_codec = P.check_codec
let dump_codec = P.dump_codec
