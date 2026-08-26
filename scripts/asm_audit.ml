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

(* An immediate abort, for malformed graph metadata. This is deliberately not
   the accumulating `fail` defined further down: if the build context cannot be
   determined, every later check would be measuring the wrong paths, so
   collecting more findings would only produce confident nonsense. *)
let fail_now fmt =
  Printf.ksprintf
    (fun s ->
      print_endline ("FAIL " ^ s);
      exit 1)
    fmt

let libraries sexps =
  List.filter_map
    (function List [ Atom "library"; body ] -> lib_of_sexp body | _ -> None)
    (List.concat_map (function List items -> items | a -> [ a ]) sexps)

(* {1 Executables as graph roots}

   `dune describe` emits executables as (executables ((names (...)) (requires
   (...)) (modules (...)) ...)) with NO `local` and NO `source_dir`. The library
   filter above drops them entirely, so cmdliner - which bin/dune names and
   lib/dune does not - is invisible to any closure taken over libraries alone.
   Since an executable is where a CLI's own dependencies are declared, a
   dependency audit that could not see them would miss exactly the edge it
   exists to check.

   An executable is identified by where its modules LIVE, not by its name: a
   name is not a location, and two projects may use the same one. *)

type exe = { exe_names : string list; exe_requires : string list; exe_impls : string list }

let rec impl_paths = function
  | List [ Atom "impl"; List [ Atom p ] ] -> [ p ]
  | List items -> List.concat_map impl_paths items
  | Atom _ -> []

let executables sexps =
  List.filter_map
    (function
      | List [ Atom "executables"; body ] ->
          Some
            {
              exe_names = atoms_of (field "names" body);
              exe_requires = atoms_of (field "requires" body);
              exe_impls = impl_paths body;
            }
      | _ -> None)
    (List.concat_map (function List items -> items | a -> [ a ]) sexps)

(* {2 Build-context normalization}

   strip_build below removes the literal "_build/default/", which is right for
   an ordinary build and WRONG for the focused boundary check: that one uses a
   private --build-dir, and dune then reports an absolute build_context such as
   /tmp/xxx/default with local source_dir and impl paths absolute beneath it.
   Measured, not assumed. A literal-prefix strip would find no executable root
   and classify every local library Unknown - the check would fail for the wrong
   reason, or pass vacuously.

   So paths are normalized against the graph's own declared context, and a local
   path that cannot be normalized FAILS CLOSED rather than falling back to
   substring replacement. *)

let build_context sexps =
  let ctxs =
    List.filter_map
      (function List [ Atom "build_context"; Atom c ] -> Some c | _ -> None)
      (List.concat_map (function List items -> items | a -> [ a ]) sexps)
  in
  match ctxs with
  | [ c ] when c <> "" -> c
  | [] -> fail_now "audit: `dune describe` reported no build_context"
  | _ -> fail_now "audit: `dune describe` reported several build_context entries"

(* Component-wise containment, not a raw string prefix: a context of
   /tmp/build/default must not accept /tmp/build/default-other/x. *)
let relative_to_context ~context path =
  if path = context then Some ""
  else
    let c = if String.length context > 0 && context.[String.length context - 1] = '/' then context
            else context ^ "/" in
    let lc = String.length c in
    if String.length path > lc && String.sub path 0 lc = c then
      Some (String.sub path lc (String.length path - lc))
    else None

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
(* "tools/" is the nested OCaml tool project (tool.md §5). It is additive, not a
   widening: `under "tool/" "tools/lib"` is false, so the existing entry never
   covered it. One entry classifies BOTH tool libraries, because production_dirs
   is tested first and `under "vendor/" "tools/vendor/err_trace"` is false -
   the test is a prefix test on the whole path, not a component search. *)
let non_production_dirs = [ "test/"; "tool/"; "tools/" ]

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

(* {1 The tool dependency manifest}

   asm/tools/dependencies.sexp declares, per opam package, its version floor,
   its scope, and the dune library names it supplies. There is no committed
   .opam file: an *.opam inside asm/ risks dune treating it as a project package,
   which affects @install and package inference in a project whose libraries
   deliberately have no public names. The manifest is an audited declaration,
   not an installable package, so it does not pretend to be one.

   A DEDICATED strict parser, not parse_sexps above. That reader is deliberately
   permissive and cannot deliver a fail-before-rules contract: it has no comment
   syntax (so the ";" lines in the manifest would parse as atoms), it accepts an
   unterminated quoted string at end of input, it accepts a missing closing
   paren at end of input, and a stray ")" silently terminates parsing. *)

type dep = { pkg : string; floor : string; scope : string; libs : string list }

let parse_manifest path =
  let s = read_file path in
  let n = String.length s in
  let line_of i =
    let l = ref 1 in
    String.iteri (fun j c -> if j < i && c = '\n' then incr l) s;
    !l
  in
  let bad i fmt =
    Printf.ksprintf (fun m -> fail_now "tool-dependencies: %s:%d: %s" path (line_of i) m) fmt
  in
  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  (* Comments are part of the grammar here, because the checked-in file uses
     them to record which exclusions are decisions rather than omissions. *)
  let rec skip i =
    if i >= n then i
    else if is_space s.[i] then skip (i + 1)
    else if s.[i] = ';' then
      let rec eol j = if j >= n || s.[j] = '\n' then j else eol (j + 1) in
      skip (eol i)
    else i
  in
  let token i =
    let i = skip i in
    if i >= n then None
    else if s.[i] = '(' then Some (`Open, i + 1)
    else if s.[i] = ')' then Some (`Close, i + 1)
    else if s.[i] = '"' then bad i "quoted strings are not part of this grammar"
    else
      let rec fin j =
        if j < n && (not (is_space s.[j])) && s.[j] <> '(' && s.[j] <> ')' && s.[j] <> ';' then fin (j + 1)
        else j
      in
      let j = fin i in
      Some (`Atom (String.sub s i (j - i)), j)
  in
  (* One row: (pkg (>= VERSION) scope (lib ...)). Exactly one constraint shape
     is accepted, because the installed-set query renders precisely
     PACKAGE>=FLOOR and cannot evaluate an arbitrary opam formula. The same
     value is rendered into both the generated opam file and the query, so the
     two can never diverge. *)
  let rec rows acc i =
    match token i with
    | None -> List.rev acc
    | Some (`Close, i) -> bad i "stray ')'"
    | Some (`Atom a, i) -> bad i "unexpected atom %S at top level" a
    | Some (`Open, i) ->
        let pkg, i =
          match token i with
          | Some (`Atom a, i) -> (a, i)
          | Some (_, j) -> bad j "expected a package name"
          | None -> bad i "expected a package name"
        in
        let i =
          match token i with Some (`Open, i) -> i | _ -> bad i "expected a version constraint"
        in
        let i =
          match token i with
          | Some (`Atom ">=", i) -> i
          | Some (`Atom op, i) -> bad i "constraint operator %S: only >= is accepted" op
          | _ -> bad i "expected a constraint operator"
        in
        let floor, i =
          match token i with Some (`Atom a, i) -> (a, i) | _ -> bad i "expected a version"
        in
        let i =
          match token i with
          | Some (`Close, i) -> i
          | Some (`Atom a, i) -> bad i "trailing item %S in a constraint: only (>= V) is accepted" a
          | _ -> bad i "unterminated constraint"
        in
        let scope, i =
          match token i with
          | Some (`Atom (("runtime" | "test") as a), i) -> (a, i)
          | Some (`Atom a, i) -> bad i "unknown scope %S: expected runtime or test" a
          | _ -> bad i "expected a scope"
        in
        let i = match token i with Some (`Open, i) -> i | _ -> bad i "expected a library list" in
        let rec libs acc i =
          match token i with
          | Some (`Atom a, i) -> libs (a :: acc) i
          | Some (`Close, i) -> (List.rev acc, i)
          | _ -> bad i "unterminated library list"
        in
        let ls, i = libs [] i in
        if ls = [] then bad i "package %s declares no libraries" pkg;
        if List.exists (fun l -> l = "digestif") ls then
          bad i "bare `digestif` is not a backend; name digestif.c or digestif.ocaml";
        let i = match token i with Some (`Close, i) -> i | _ -> bad i "unterminated row for %s" pkg in
        rows ({ pkg; floor; scope; libs = ls } :: acc) i
  in
  let ds = rows [] 0 in
  if ds = [] then fail_now "tool-dependencies: %s declares nothing" path;
  let names = List.map (fun d -> d.pkg) ds in
  let sorted = List.sort String.compare names in
  let rec dup = function a :: (b :: _ as r) -> if a = b then Some a else dup r | _ -> None in
  (match dup sorted with Some p -> fail_now "tool-dependencies: duplicate package %s" p | None -> ());
  let all_libs = List.concat_map (fun d -> d.libs) ds in
  (match dup (List.sort String.compare all_libs) with
  | Some l -> fail_now "tool-dependencies: library %s is mapped by two packages" l
  | None -> ());
  ds

(* {2 opam, which owns version comparison}

   Never compare version strings by hand: opam's ordering is not lexicographic.
   Exit statuses are classified EXACTLY rather than as "nonzero", so an
   operational failure - a lock, a bad configuration, a solver crash - is never
   reported as a dependency problem. Per opam 2.3's documented table: with
   --check, 1 is False; 20 is "no solution to the user request"; everything else
   nonzero is operational. --cli=2.3 pins the contract so a newer opam binary
   still means what this code was written against. *)

type opam_answer = Yes | No | No_solution | Operational of int * string

let tmp_file suffix = Filename.concat (Filename.get_temp_dir_name ()) ("asm_audit_" ^ suffix)

(* Sys.command rather than Unix.open_process_in: this script is run by `ocaml`,
   not linked, so unix.cma is not available. The status Sys.command returns is
   the command's, which is all the classification below needs. *)
let opam_query args =
  let errf = tmp_file "opam_err" in
  let cmd = Printf.sprintf "opam --cli=2.3 %s >/dev/null 2>%s" args (Filename.quote errf) in
  let status = Sys.command cmd in
  let stderr_text = try read_file errf with _ -> "" in
  (try Sys.remove errf with _ -> ());
  match status with
  | 0 -> Yes
  | 1 -> No
  | 20 -> No_solution
  | c -> Operational (c, stderr_text)

let check_floor d =
  (* Two stages, so "not installed at all" is distinguishable from "installed
     but below the floor" - they have different fixes. *)
  match opam_query (Printf.sprintf "list --installed --check %s" (Filename.quote d.pkg)) with
  | No -> fail "tool-dependencies: package %s is declared but not installed" d.pkg
  | Operational (c, e) ->
      fail "tool-dependencies: opam failed operationally (status %d) checking %s: %s" c d.pkg
        (String.trim e)
  | No_solution ->
      fail "tool-dependencies: opam reported no solution while checking whether %s is installed" d.pkg
  | Yes -> (
      match
        opam_query
          (Printf.sprintf "list --installed --resolve=%s --check"
             (Filename.quote (d.pkg ^ ">=" ^ d.floor)))
      with
      | Yes -> ()
      | No | No_solution ->
          fail "tool-dependencies: %s is installed but below the declared floor %s" d.pkg d.floor
      | Operational (c, e) ->
          fail "tool-dependencies: opam failed operationally (status %d) checking %s >= %s: %s" c
            d.pkg d.floor (String.trim e))

(* {3 Rule 4's containment boundary}

   A resolved-library allowlist, NOT a dependency declaration: stale entries are
   accepted by design, because the question it answers is "did anything
   unexpected enter the closure", not "is every entry still used". Measured on
   OCaml 4.14.3 from the final stanzas; compiler-supplied unix is excluded. *)
let tool_transitive_allowlist =
  [ "astring"; "bos"; "cmdliner"; "digestif"; "digestif.ocaml"; "eqaf"; "fmt"; "fpath"; "logs";
    "mtime"; "mtime.clock"; "mtime.clock.os"; "rresult"; "topkg"; "seq"; "stdlib-shims" ]

(* {4 The two tool modes} *)

let ocaml_lib_dir () =
  let out = tmp_file "ocamlwhere" in
  let rc = Sys.command (Printf.sprintf "ocamlc -where >%s 2>/dev/null" (Filename.quote out)) in
  let d = if rc = 0 then (try String.trim (read_file out) with _ -> "") else "" in
  (try Sys.remove out with _ -> ());
  d

let tool_roots ~context libs exes =
  let under_tools p =
    match relative_to_context ~context p with
    | Some rel -> starts_with "tools/" rel
    | None -> false
  in
  let lib_roots = List.filter (fun l -> l.local && under_tools l.source_dir) libs in
  let exe_roots = List.filter (fun e -> List.exists under_tools e.exe_impls) exes in
  (lib_roots, exe_roots)

let audit_tool_dependencies asm_dir libs exes context =
  let manifest = Filename.concat asm_dir "tools/dependencies.sexp" in
  if not (Sys.file_exists manifest) then fail_now "tool-dependencies: %s is missing" manifest;
  let deps = parse_manifest manifest in
  let lib_roots, exe_roots = tool_roots ~context libs exes in
  if lib_roots = [] then fail_now "tool-dependencies: no tool libraries found - is asm/tools built?";
  if exe_roots = [] then fail_now "tool-dependencies: no tool executables found - is asm/tools built?";
  let by_uid = Hashtbl.create 64 in
  List.iter (fun l -> Hashtbl.replace by_uid l.uid l) libs;
  let resolve uid = Hashtbl.find_opt by_uid uid in
  (* Local paths that cannot be normalized fail closed rather than being
     silently classified. *)
  List.iter
    (fun l ->
      if l.local && relative_to_context ~context l.source_dir = None then
        fail "tool-dependencies: local library %S has source_dir %s outside the build context %s"
          l.name l.source_dir context)
    libs;
  let switch_lib = ocaml_lib_dir () in
  (* unix is recognized as compiler-supplied BY ITS DIRECTORY, not by name: a
     name-only rule would let any external library called unix through. *)
  let compiler_supplied l = (not l.local) && switch_lib <> "" && starts_with switch_lib l.source_dir in
  let direct_uids =
    List.concat_map (fun l -> l.requires) lib_roots @ List.concat_map (fun e -> e.exe_requires) exe_roots
  in
  let direct =
    List.filter_map resolve direct_uids
    |> List.filter (fun l -> (not l.local) && not (compiler_supplied l))
  in
  let declared_libs = List.concat_map (fun d -> d.libs) deps in
  (* Rule 1: every direct external library maps to exactly one declared package. *)
  List.iter
    (fun l ->
      if not (List.mem l.name declared_libs) then
        fail "tool-dependencies: rule 1: tool stanzas use external library %S, which no package in %s declares"
          l.name manifest)
    direct;
  (* Rule 2: every declared package supplies at least one direct dependency.
     This is what catches a row left behind after its dune edge was deleted. *)
  List.iter
    (fun d ->
      if not (List.exists (fun l -> List.mem l.name d.libs) direct) then
        fail "tool-dependencies: rule 2: package %s is declared but none of its libraries (%s) is used"
          d.pkg (String.concat " " d.libs))
    deps;
  (* Rule 3: floors, evaluated by opam. *)
  List.iter check_floor deps;
  (* Rule 4: the transitive external closure is contained. *)
  let members = closure libs (lib_roots @ List.filter_map resolve (List.concat_map (fun e -> e.exe_requires) exe_roots)) in
  List.iter
    (fun l ->
      if (not l.local) && (not (compiler_supplied l)) && not (List.mem l.name tool_transitive_allowlist)
      then
        fail "tool-dependencies: rule 4: unexpected resolved library %S (in %s) is outside the transitive allowlist"
          l.name l.source_dir)
    members;
  (* The devcontainer must be able to provide every declared package. *)
  let dc = Filename.concat (Filename.dirname asm_dir) ".devcontainer/devcontainer.json" in
  if Sys.file_exists dc then begin
    let text = read_file dc in
    List.iter
      (fun d ->
        if not (contains text d.pkg) then
          fail "tool-dependencies: package %s is declared but absent from %s" d.pkg dc)
      deps
  end

(* The focused boundary: seeded from EXACTLY the compcert_tools executable, not
   from every tool root. tools-build builds one executable, so auditing the
   test, fake and integration closures would stop proving that target's actual
   boundary. *)
let audit_tool_boundary libs exes context =
  let allowlist = [ "tools/" ] in
  let root =
    List.filter
      (fun e ->
        List.exists
          (fun p ->
            match relative_to_context ~context p with
            | Some rel -> rel = "tools/bin/compcert_tools.ml"
            | None -> false)
          e.exe_impls)
      exes
  in
  (match root with
  | [] -> fail_now "tool-boundary: no executable with implementation tools/bin/compcert_tools.ml"
  | [ _ ] -> ()
  | _ -> fail_now "tool-boundary: several executables claim tools/bin/compcert_tools.ml");
  let by_uid = Hashtbl.create 64 in
  List.iter (fun l -> Hashtbl.replace by_uid l.uid l) libs;
  let seeds = List.filter_map (Hashtbl.find_opt by_uid) (List.concat_map (fun e -> e.exe_requires) root) in
  let members = closure libs seeds in
  let locals = List.filter (fun l -> l.local) members in
  note "focused local closure: %s" (String.concat " " (List.map (fun l -> l.name) locals));
  List.iter
    (fun l ->
      match relative_to_context ~context l.source_dir with
      | None ->
          fail "tool-boundary: local library %S has source_dir %s outside the build context %s" l.name
            l.source_dir context
      | Some rel ->
          if not (List.exists (fun p -> starts_with p rel) allowlist) then
            fail "tool-boundary: the focused build reaches local library %S in %s, outside the allowlist (%s)"
              l.name rel (String.concat " " allowlist))
    locals

(* {1 Entry point} *)

let () =
  let mode = Sys.argv.(1) and desc = Sys.argv.(2) and asm_dir = Sys.argv.(3) in
  let sexps = parse_sexps (read_file desc) in
  let libs = libraries sexps in
  if libs = [] then fail "audit: `dune describe` reported no libraries";
  (match mode with
  | "purity" -> audit_purity asm_dir libs
  | "layers" -> audit_layers asm_dir libs
  | "tool-dependencies" ->
      audit_tool_dependencies asm_dir libs (executables sexps) (build_context sexps)
  | "tool-boundary" -> audit_tool_boundary libs (executables sexps) (build_context sexps)
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
