(* Emit_guard: the generated-code hazards it refuses and the shapes it lets through, then the
   committed generated files given as argv. *)

open Rivet_tools

let failures = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let refused text = Result.is_error (Emit_guard.check text)
let list n elt = "let l = [ " ^ String.concat "; " (List.init n elt) ^ " ]\n"
let int i = string_of_int i

let test_lists () =
  check "a 200-element list is accepted" (not (refused (list 200 int)));
  check "a 201-element list is refused" (refused (list 201 int));
  check "a 1034-element array is accepted"
    (not (refused ("let a = [| " ^ String.concat "; " (List.init 1034 int) ^ " |]\n")));
  check "lists inside an array count separately"
    (not
       (refused ("let a = [| " ^ String.concat "; " (List.init 300 (fun _ -> list 3 int)) ^ " |]")));
  check "semicolons in strings, chars and comments do not count"
    (not (refused (list 150 (fun i -> Printf.sprintf "(* a;b;c *) \"x;y;%d\" ';'" i))));
  check "semicolons in a record or tuple inside a list do not count"
    (not (refused (list 150 (fun i -> Printf.sprintf "{ a = %d; b = (1; 2) }" i))));
  check "the error names the line"
    (match Emit_guard.check ("\n\n" ^ list 201 int) with
    | Error m -> String.starts_with ~prefix:"line 3:" m
    | Ok () -> false)

let test_functors () =
  check "a functor definition is refused" (refused "module Make (X : S) = struct end\n");
  check "the functor keyword is refused" (refused "module F = functor (X : S) -> struct end\n");
  check "a module type and a plain module are accepted"
    (not (refused "module type S = sig val x : int end\nmodule M = struct let x = 1 end\n"));
  check "a functor in a comment or string is accepted"
    (not (refused "(* module Make (X : S) functor *)\nlet s = \"module M (X)\"\n"))

let test_committed files =
  List.iter
    (fun path ->
      let text = In_channel.with_open_bin path In_channel.input_all in
      check
        ("committed " ^ Filename.basename path ^ " passes")
        (match Emit_guard.check text with
        | Ok () -> true
        | Error m ->
            print_endline ("    " ^ m);
            false))
    files

let () =
  print_endline "emit_guard:";
  test_lists ();
  test_functors ();
  test_committed (List.tl (Array.to_list Sys.argv));
  if !failures > 0 then (
    Printf.printf "emit_guard: %d failures\n" !failures;
    exit 1)
  else print_endline "emit_guard: all checks passed"
