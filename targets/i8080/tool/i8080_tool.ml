let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let die fmt =
  Printf.ksprintf
    (fun m ->
      prerr_endline m;
      exit 1)
    fmt

(* asm <source> -o <image> [-l <listing>] [-s <symbols>] [-r LO-HI] *)
let asm args =
  let range = ref None in
  let rec parse (src, out, lst, sym) = function
    | [] -> (src, out, lst, sym)
    | "-o" :: o :: r -> parse (src, Some o, lst, sym) r
    | "-l" :: l :: r -> parse (src, out, Some l, sym) r
    | "-s" :: s :: r -> parse (src, out, lst, Some s) r
    | "-r" :: spec :: r ->
        (match String.split_on_char '-' spec with
        | [ lo; hi ] -> range := Some (int_of_string lo, int_of_string hi)
        | _ -> die "-r wants LO-HI");
        parse (src, out, lst, sym) r
    | f :: r when src = None -> parse (Some f, out, lst, sym) r
    | _ -> die "usage: i8080_tool asm <source> -o <image> [-l <listing>] [-s <symbols>] [-r LO-HI]"
  in
  match parse (None, None, None, None) args with
  | Some src, out, lst, sym -> (
      match I8080_as.assemble ~file:(Filename.basename src) (read_file src) with
      | Error ds -> die "%s" (I8080_as.render ds)
      | Ok img ->
          let origin, bytes = I8080_as.flatten ?range:!range img in
          Option.iter (fun o -> write_file o bytes) out;
          Option.iter (fun l -> write_file l (I8080_as.listing_to_string img ^ "\n")) lst;
          Option.iter
            (fun s ->
              write_file s
                (String.concat ""
                   (List.map (fun (n, v) -> Printf.sprintf "%s %04x\n" n v) img.I8080_as.symbols)))
            sym;
          Printf.printf "%04x %d\n" origin (String.length bytes))
  | _ -> die "usage: i8080_tool asm <source> -o <image>"

let () =
  match List.tl (Array.to_list Sys.argv) with
  | "asm" :: args -> asm args
  | _ -> die "usage: i8080_tool asm|run ..."
