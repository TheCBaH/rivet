let ( let* ) = Result.bind

let rec result_map f = function
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = result_map f xs in
      Ok (y :: ys)

let render_line (s : Isa_norm_model.syntax_recipe) ~operands =
  let lookup name =
    match List.assoc_opt name operands with
    | Some v -> Ok v
    | None -> Error (Printf.sprintf "no operand assignment for %S" name)
  in
  let rec render_token = function
    | Isa_norm_model.Syn_operand name -> lookup name
    | Isa_norm_model.Syn_literal text -> Ok text
    | Isa_norm_model.Syn_group toks ->
        let* rendered = result_map render_token toks in
        Ok (String.concat "" rendered)
    | Isa_norm_model.Syn_decorated (prefix, tok) ->
        let* rendered = render_token tok in
        Ok (prefix ^ rendered)
  in
  match s.operands with
  | [] -> Ok s.mnemonic
  | toks ->
      let* rendered = result_map render_token toks in
      Ok (s.mnemonic ^ " " ^ String.concat ", " rendered)

let render_source_lines lines = ".text\n" ^ String.concat "\n" lines ^ "\n"
let render_source line = render_source_lines [ line ]
