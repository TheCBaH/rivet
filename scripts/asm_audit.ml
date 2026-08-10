(* Purity and layer audit for asm/ (.ai/asm_plan.md §1, §2.2, §3.7, §5.1).

   Run as `ocaml tools/asm_audit.ml <mode> <describe.sexp> <asm-dir>` by the two
   wrapper scripts beside it. Deliberately a standalone toplevel script rather
   than part of the asm/ dune tree: an auditor that had to be built by the thing
   it audits could not report on a tree that fails to build, and it would appear
   in its own closure.

   Not OCaml-in-a-shell-script by preference either - `dune describe` emits
   s-expressions, and python3, bc and dc are all absent from the container, so
   the alternative was sed against nested parentheses.

   What `dune describe` can and cannot answer
   ------------------------------------------
   Measured against dune 3.23.1: `dune describe --lang 0.1` exposes, per
   library, exactly (name uid local requires source_dir modules include_dirs).
   It carries *no* foreign-object information - no foreign_stubs, no
   foreign_archives, no c_names, no extra_objects. So it settles the resolved
   transitive closure and nothing else, which is why the no-C property is
   established separately below by scanning each closure member's installed
   directory for stub archives and by reading our own dune stanzas. Each check
   claims only what it actually establishes; see the plan's mechanism list. *)

(* {1 A minimal s-expression reader} *)

type sexp = Atom of string | List of sexp list

let parse_sexps (s : string) : sexp list =
  let n = String.length s in
  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let is_delim c = is_space c || c = '(' || c = ')' in
  (* Returns the parsed value and the index just past it. Recursive rather than
     stateful so there is no reader position to get out of step. *)
  let rec skip i = if i < n && is_space s.[i] then skip (i + 1) else i in
  let rec atom i j = if j < n && not (is_delim s.[j]) then atom i (j + 1) else (String.sub s i (j - i), j) in
  let rec quoted i buf =
    if i >= n then (Buffer.contents buf, i)
    else
      match s.[i] with
      | '"' -> (Buffer.contents buf, i + 1)
      | '\\' when i + 1 < n ->
          Buffer.add_char buf s.[i + 1];
          quoted (i + 2) buf
      | c ->
          Buffer.add_char buf c;
          quoted (i + 1) buf
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
          let a, j = quoted (i + 1) (Buffer.create 16) in
          Some (Atom a, j)
      | _ ->
          let a, j = atom i i in
          Some (Atom a, j)
  and many i acc =
    let i = skip i in
    if i >= n then (List.rev acc, i)
    else if s.[i] = ')' then (List.rev acc, i + 1)
    else match one i with Some (v, j) -> many j (v :: acc) | None -> (List.rev acc, i + 1)
  in
  let rec top i acc = match one i with None -> List.rev acc | Some (v, j) -> top j (v :: acc) in
  top 0 []

(* {1 The library graph} *)

type lib = {
  name : string;
  uid : string;
  local : bool;
  requires : string list;
  source_dir : string;
}

let field key = function
  | List items ->
      List.find_map (function List (Atom k :: rest) when k = key -> Some rest | _ -> None) items
  | _ -> None

let atom_of = function Some [ Atom a ] -> Some a | _ -> None
let atoms_of = function
  | Some [ List l ] -> List.filter_map (function Atom a -> Some a | _ -> None) l
  | _ -> []

let lib_of_sexp body =
  match (atom_of (field "name" body), atom_of (field "uid" body)) with
  | Some name, Some uid ->
      Some
        {
          name;
          uid;
          local = atom_of (field "local" body) = Some "true";
          requires = atoms_of (field "requires" body);
          source_dir = Option.value ~default:"" (atom_of (field "source_dir" body));
        }
  | _ -> None

let libraries sexps =
  List.filter_map
    (function List [ Atom "library"; body ] -> lib_of_sexp body | _ -> None)
    (List.concat_map (function List items -> items | a -> [ a ]) sexps)

(* {1 Directory classification}

   The production closure is defined by *where a library lives*, not by an
   enumerated list of roots, so adding a package under lib/ or targets/ puts it
   under audit automatically instead of silently escaping one. melange/ mirrors
   the same tree (see asm/melange/dune) and is classified identically. *)

let strip_build d =
  let p = "_build/default/" in
  if String.length d > String.length p && String.sub d 0 (String.length p) = p then
    String.sub d (String.length p) (String.length d - String.length p)
  else d

let starts_with pre s =
  String.length s >= String.length pre && String.sub s 0 (String.length pre) = pre

let production_dirs = [ "lib/"; "targets/"; "driver/"; "browser/"; "vendor/" ]
let non_production_dirs = [ "test/"; "tool/" ]

(* [under "driver/" "driver"] must hold: a package can sit directly at a root
   directory rather than in a subdirectory of it, and a prefix test alone would
   classify that as Unknown and quietly drop it out of the audited root set. *)
let under pre d = starts_with pre d || d = String.sub pre 0 (String.length pre - 1)

let classify lib =
  let d = strip_build lib.source_dir in
  let d = if starts_with "melange/" d then String.sub d 8 (String.length d - 8) else d in
  if List.exists (fun p -> under p d) production_dirs then `Production
  else if List.exists (fun p -> under p d) non_production_dirs then `Not_production
  else `Unknown

(* {1 Denylists and allowlists} *)

(* Named explicitly so a mistake reports the reason rather than the generic
   "unexpected external package". zarith is installed in this switch and §5.3
   bans it, so the audit must reject it actively rather than rely on its
   absence. base/stdio/time_now/ppx_* come in with ppx_expect, which is
   test-only; time_now genuinely has C stubs. *)
let denied =
  [
    ("zarith", "§5.3 bans Zarith/GMP: C-backed");
    ("unix", "§3.7 bans Unix from the production closure");
    ("threads", "§3.7: not portable to js_of_ocaml or Melange");
    ("bigarray", "§3.7 bans bigarrays");
    ("dynlink", "§3.7: not portable");
    ("str", "C-backed");
    ("base", "test-only, and C-backed via base_internalhash_types");
    ("stdio", "test-only (ppx_expect)");
    ("time_now", "test-only (ppx_inline_test), C stubs");
    ("ppx_expect", "test-only, native only");
    ("ppx_inline_test", "test-only, native only");
  ]

let denied_prefixes = [ ("ctypes", "C-backed FFI"); ("core", "C-backed") ]

(* External packages permitted inside the production closure. Empty on purpose:
   Fmt is vendored precisely so that the closure needs nothing from opam, and
   any addition here is a reviewed decision, not an accident. *)
let external_allowlist : string list = []

(* {1 Filesystem helpers} *)

let rec walk dir f =
  if Sys.file_exists dir && Sys.is_directory dir then
    Array.iter
      (fun e ->
        let p = Filename.concat dir e in
        if e = "_build" || e = ".git" then ()
        else if Sys.is_directory p then walk p f
        else f p)
      (Sys.readdir dir)

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  nn > 0 && go 0

(* {1 Reporting} *)

let failures = ref []
let fail fmt = Printf.ksprintf (fun s -> failures := s :: !failures) fmt
let note fmt = Printf.ksprintf (fun s -> print_endline ("  . " ^ s)) fmt

(* {1 The purity audit} *)

let closure libs roots =
  let by_uid = Hashtbl.create 64 in
  List.iter (fun l -> Hashtbl.replace by_uid l.uid l) libs;
  let rec go seen = function
    | [] -> seen
    | uid :: rest when List.mem uid seen -> go seen rest
    | uid :: rest -> (
        match Hashtbl.find_opt by_uid uid with
        | None -> go (uid :: seen) rest
        | Some l -> go (uid :: seen) (l.requires @ rest))
  in
  List.filter_map (Hashtbl.find_opt by_uid) (go [] (List.map (fun l -> l.uid) roots))

let audit_purity asm_dir libs =
  (* A local library in an unrecognized directory would otherwise be neither a
     root nor a rejected member: it would simply escape the audit. *)
  List.iter
    (fun l ->
      if l.local && classify l = `Unknown then
        fail "purity: local library %S lives in %s, which is neither a production nor a test tree"
          l.name (strip_build l.source_dir))
    libs;
  let roots = List.filter (fun l -> l.local && classify l = `Production) libs in
  if roots = [] then fail "purity: no production libraries found - is asm/ built?";
  note "production roots: %s" (String.concat " " (List.map (fun l -> l.name) roots));
  let members = closure libs roots in
  note "resolved closure: %d libraries" (List.length members);

  (* Mechanism 1 - the resolved dependency closure. This is all `dune describe`
     can establish; it says nothing about foreign objects. *)
  List.iter
    (fun l ->
      (match List.assoc_opt l.name denied with
      | Some why -> fail "purity: production closure reaches %S (%s)" l.name why
      | None -> ());
      List.iter
        (fun (p, why) ->
          if starts_with p l.name then fail "purity: production closure reaches %S (%s)" l.name why)
        denied_prefixes;
      if (not l.local) && not (List.mem l.name external_allowlist) then
        fail "purity: production closure reaches external package %S (allowlist is empty by design)"
          l.name;
      if l.local && classify l = `Not_production then
        fail "purity: production closure reaches non-production library %S in %s" l.name
          (strip_build l.source_dir))
    members;

  (* Mechanism 2 - foreign-build inspection, which carries the whole no-C claim
     because the schema above cannot. Local sources are checked for C in the
     tree; external members are checked for installed stub archives. *)
  List.iter
    (fun l ->
      let dir = if l.local then Filename.concat asm_dir (strip_build l.source_dir) else l.source_dir in
      if Sys.file_exists dir && Sys.is_directory dir then
        Array.iter
          (fun e ->
            let bad_ext = List.exists (fun x -> Filename.check_suffix e x) [ ".c"; ".h"; ".o"; ".a" ] in
            let bad_so = starts_with "dll" e && Filename.check_suffix e ".so" in
            if bad_ext || bad_so then
              fail "purity: %S ships a foreign object %s (in %s)" l.name e dir)
          (Sys.readdir dir))
    members;

  (* Our own stanzas, including any generated by a rule: the closure above is
     built from what dune resolved, so a foreign_stubs clause that failed to
     resolve would not appear there at all. *)
  let forbidden_stanzas =
    [ "foreign_stubs"; "foreign_archives"; "(c_names"; "extra_objects"; "(language c)" ]
  in
  walk asm_dir (fun p ->
      if Filename.basename p = "dune" then
        let s = read_file p in
        List.iter
          (fun k -> if contains s k then fail "purity: %s uses %s" p k)
          forbidden_stanzas)

(* {1 The layer audit} *)

let audit_layers asm_dir libs =
  let dir_of l = strip_build l.source_dir in
  let generic l =
    let d = dir_of l in
    starts_with "lib/" d || starts_with "melange/lib/" d
  in
  let target l =
    let d = dir_of l in
    starts_with "targets/" d || starts_with "melange/targets/" d
  in
  (* §5.1: dependencies run one way, from generic machinery to target
     descriptions. A generic package reaching a target package would make the
     registry a cycle and the "no target vocabulary in lib/" rule unenforceable. *)
  List.iter
    (fun l ->
      if l.local && generic l then
        List.iter
          (fun m -> if target m then fail "layers: generic %S depends on target %S" l.name m.name)
          (closure libs [ l ]))
    libs;

  (* §3.7 again, from the other direction: no generic source may branch on a
     target's identity. Checked textually because it is a property of the source,
     not of the resolved graph - a `match` on a target name compiles fine. *)
  let target_names = [ "x86_32"; "x86_64"; "aarch64"; "x86_family" ] in
  let lib_src = Filename.concat asm_dir "lib" in
  walk lib_src (fun p ->
      if Filename.check_suffix p ".ml" || Filename.check_suffix p ".mli" then
        let s = read_file p in
        List.iter
          (fun t ->
            if contains s ("\"" ^ t ^ "\"") then
              fail "layers: generic source %s mentions target name %S" p t)
          target_names)

(* {1 Entry point} *)

let () =
  let mode = Sys.argv.(1) and desc = Sys.argv.(2) and asm_dir = Sys.argv.(3) in
  let libs = libraries (parse_sexps (read_file desc)) in
  if libs = [] then fail "audit: `dune describe` reported no libraries";
  (match mode with
  | "purity" -> audit_purity asm_dir libs
  | "layers" -> audit_layers asm_dir libs
  | m ->
      prerr_endline ("unknown mode: " ^ m);
      exit 2);
  match List.rev !failures with
  | [] ->
      Printf.printf "%s: ok\n" mode;
      exit 0
  | fs ->
      List.iter (fun f -> print_endline ("FAIL " ^ f)) fs;
      exit 1
