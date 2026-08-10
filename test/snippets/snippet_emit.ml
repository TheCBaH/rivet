(* Write each dogfoodable snippet as a .s file, for tools/asm-gas-xref.sh.

   The text comes from the same canonical printer whose output test_snippets
   pins, so the file GNU as assembles and the bytes this project produces come
   from one AST rather than from two hand-maintained descriptions of it. That is
   the whole point of the exercise: a disagreement is then attributable to the
   encoders, because the input is not in question.

   Only [Assembled] entries are written. An entry this assembler cannot build
   has no canonical text to emit, and inventing some would mean hand-writing the
   assembly - which is exactly the second description this avoids.

   Usage: snippet_emit <output-dir>
   Writes <output-dir>/<target>/<case>/input.s and prints each path. *)

let write path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let rec mkdir_p path =
  if not (Sys.file_exists path) then (
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let () =
  match Sys.argv with
  | [| _; root |] ->
      List.iter
        (fun (t : Snippet_corpus.Snippet_ast.target) ->
          List.iter
            (fun (e : Snippet_corpus.Snippet_ast.entry) ->
              match e.built with
              | Snippet_corpus.Snippet_ast.Assembled { canonical; _ } ->
                  let dir = Filename.concat (Filename.concat root t.target) e.name in
                  mkdir_p dir;
                  let path = Filename.concat dir "input.s" in
                  write path (Snippet_corpus.Snippet_ast.source_of t canonical);
                  print_endline path
              | Refused _ | Needs _ -> ())
            t.entries)
        Snippet_corpus.Snippet_ast.all
  | _ ->
      prerr_endline "usage: snippet_emit <output-dir>";
      exit 2
