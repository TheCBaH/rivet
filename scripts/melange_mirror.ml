(* Drift check between the native library stanzas and their Melange mirror under melange/.

   Run as `ocaml scripts/melange_mirror.ml <rivet-dir>` by melange-mirror-check.sh. It needs no
   Melange: dune files are read as s-expressions and each library is reduced to its name, the
   modules it contains, its [wrapped] field and its [private_modules]. A library present in one
   tree and not the other, or differing in any of those, fails the check. Dependencies are not
   compared: they legitimately differ (for example err_trace and its Melange build). *)

type sexp = Atom of string | List of sexp list

let read_file p = In_channel.with_open_bin p In_channel.input_all

let parse s =
  let n = String.length s in
  let rec skip i =
    if i >= n then i
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' -> skip (i + 1)
      | ';' ->
          let rec eol j = if j < n && s.[j] <> '\n' then eol (j + 1) else j in
          skip (eol i)
      | _ -> i
  in
  let rec one i =
    let i = skip i in
    if i >= n then None
    else
      match s.[i] with
      | '(' ->
          let items, j = many (i + 1) [] in
          Some (List items, j)
      | ')' -> None
      | '"' ->
          let rec close j =
            if j < n && s.[j] <> '"' then close (if s.[j] = '\\' then j + 2 else j + 1) else j
          in
          let j = close (i + 1) in
          Some (Atom (String.sub s (i + 1) (j - i - 1)), j + 1)
      | _ ->
          let rec stop j =
            if j < n && not (List.mem s.[j] [ ' '; '\t'; '\n'; '\r'; '('; ')' ]) then stop (j + 1)
            else j
          in
          let j = stop i in
          Some (Atom (String.sub s i (j - i)), j)
  and many i acc =
    let i = skip i in
    if i >= n then (List.rev acc, i)
    else if s.[i] = ')' then (List.rev acc, i + 1)
    else match one i with Some (v, j) -> many j (v :: acc) | None -> (List.rev acc, i + 1)
  in
  let rec top i acc = match one i with None -> List.rev acc | Some (v, j) -> top j (v :: acc) in
  top 0 []

let field name fields =
  List.find_map (function List (Atom k :: rest) when k = name -> Some rest | _ -> None) fields

let rec walk dir f =
  if Sys.file_exists dir && Sys.is_directory dir then
    Array.iter
      (fun e ->
        let p = Filename.concat dir e in
        if Sys.is_directory p then walk p f else f p)
      (let a = Sys.readdir dir in
       Array.sort compare a;
       a)

let ml_files dir =
  if Sys.file_exists dir && Sys.is_directory dir then
    Sys.readdir dir |> Array.to_list
    |> List.filter_map (fun f ->
        if Filename.check_suffix f ".ml" then Some (Filename.remove_extension f) else None)
  else []

let normalize m = String.uncapitalize_ascii m
let rec show = function Atom a -> a | List l -> "(" ^ String.concat " " (List.map show l) ^ ")"

type stanza = {
  modules : string list;
  backends : sexp list option;
  wrapped : string;
  private_modules : string list;
}

(* The directories a mirror dune file copies sources from. *)
let copied_dirs dune_dir stanzas =
  List.filter_map
    (function
      | List [ Atom "copy_files#"; List [ Atom "files"; Atom glob ] ] ->
          let d = Filename.dirname glob in
          let p = Filename.concat dune_dir d in
          (* normalise ../ segments *)
          let parts = String.split_on_char '/' p in
          let stack =
            List.fold_left
              (fun acc seg ->
                match (seg, acc) with "..", _ :: rest -> rest | ".", _ -> acc | _ -> seg :: acc)
              [] parts
          in
          Some (String.concat "/" (List.rev stack))
      | _ -> None)
    stanzas

(* name -> sorted module list, for the libraries under [prefix]/<roots>. *)
let libraries root ~prefix ~mirror =
  let table = Hashtbl.create 64 in
  List.iter
    (fun r ->
      walk
        (Filename.concat root (Filename.concat prefix r))
        (fun p ->
          if Filename.basename p = "dune" then
            let dir = Filename.dirname p in
            let stanzas = parse (read_file p) in
            List.iter
              (function
                | List (Atom "library" :: fields) -> (
                    match field "name" fields with
                    | Some [ Atom name ] ->
                        let modules =
                          match field "modules" fields with
                          | Some ms
                            when not
                                   (List.exists
                                      (function
                                        | Atom a -> a = ":standard" || a = "\\" | _ -> false)
                                      ms) ->
                              List.filter_map
                                (function Atom a -> Some (normalize a) | _ -> None)
                                ms
                          | _ ->
                              let dirs = if mirror then copied_dirs dir stanzas else [ dir ] in
                              List.concat_map ml_files dirs
                        in
                        let wrapped =
                          match field "wrapped" fields with
                          | None -> "true"
                          | Some v -> String.concat " " (List.map show v)
                        in
                        let private_modules =
                          match field "private_modules" fields with
                          | None -> []
                          | Some ms ->
                              List.sort_uniq compare
                                (List.map
                                   (function Atom a -> normalize a | List _ as l -> show l)
                                   ms)
                        in
                        Hashtbl.replace table name
                          {
                            modules = List.sort_uniq compare modules;
                            backends = field "modes" fields;
                            wrapped;
                            private_modules;
                          }
                    | _ -> ())
                | _ -> ())
              stanzas))
    [ "lib"; "targets"; "driver"; "driver_direct"; "text_pipeline"; "browser" ];
  table

let () =
  let root = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  let native = libraries root ~prefix:"" ~mirror:false in
  let mirror = libraries root ~prefix:"melange" ~mirror:true in
  let failures = ref 0 in
  let fail fmt =
    incr failures;
    Printf.ksprintf prerr_endline fmt
  in
  let only a b = String.concat " " (List.filter (fun m -> not (List.mem m b)) a) in
  Hashtbl.iter
    (fun name n ->
      (* A native library with no [modes] is a tool's helper (i8080_corpus), not production. *)
      if n.backends <> None then
        match Hashtbl.find_opt mirror name with
        | None -> fail "melange mirror: library %s has no stanza under melange/" name
        | Some m ->
            if n.modules <> m.modules then
              fail "melange mirror: library %s differs; native only [%s], melange only [%s]" name
                (only n.modules m.modules) (only m.modules n.modules);
            if n.wrapped <> m.wrapped then
              fail
                "melange mirror: library %s is (wrapped %s) natively but (wrapped %s) under \
                 melange/"
                name n.wrapped m.wrapped;
            if n.private_modules <> m.private_modules then
              fail
                "melange mirror: library %s private_modules differ; native only [%s], melange only \
                 [%s]"
                name
                (only n.private_modules m.private_modules)
                (only m.private_modules n.private_modules);
            if not (match m.backends with Some l -> List.mem (Atom "melange") l | None -> false)
            then fail "melange mirror: library %s is not built in melange mode" name)
    native;
  Hashtbl.iter
    (fun name _ ->
      if not (Hashtbl.mem native name) then
        fail "melange mirror: library %s exists only under melange/" name)
    mirror;
  let native =
    Hashtbl.filter_map_inplace (fun _ v -> if v.backends = None then None else Some v) native;
    native
  in
  Printf.printf "melange mirror: %d native libraries, %d mirrored, %s\n" (Hashtbl.length native)
    (Hashtbl.length mirror)
    (if !failures = 0 then "no drift" else string_of_int !failures ^ " problems");
  if !failures > 0 then exit 1
