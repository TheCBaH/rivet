(* Expect tests for Foundation.Err_policy, Legacy_error, and Diagnostic.of_error.

   Three properties, each of which the rest of the migration relies on and none
   of which is obvious from reading the code.

   Nothing here prints an origin, a stack, or an event: asm/docs/errors.md §3
   forbids that in a baseline, and a test that violated the rule it is checking
   would be a poor guard. The observable facts are booleans and rendered
   diagnostics. *)

open Foundation

(* Whether an error's origin carries each of its two independent components.
   [Err.Origin.pp] would print the stack text itself, which is exactly what must
   not reach this file. *)
let origin_shape error =
  match Err.Error.origin error with
  | None -> (false, false)
  | Some origin -> (Err.Origin.source origin <> None, Err.Origin.stack origin <> None)

let report label error =
  let explicit, stack = origin_shape error in
  Fmt.pr "%s: explicit-source=%b automatic-stack=%b events=%d@." label explicit stack
    (List.length (Err.Error.events error))

let fail_here () = Err.fail ~pos:__POS__ ~pp_error:Fmt.string "boom" |> Result.get_error

(* The policy keeps the explicit position and drops the stack. This is the
   property that makes ~pos:__POS__ worth writing at every failure site: under
   Err's own default the stack would be there too, and its text is host- and
   backend-dependent. *)
let%expect_test "deterministic policy: explicit position kept, stack dropped" =
  Err.Config.with_config Err.Config.fast (fun () ->
      Err_policy.apply ();
      report "deterministic" (fail_here ()));
  [%expect {| deterministic: explicit-source=true automatic-stack=false events=0 |}]

(* And the converse, so the first test is a demonstrated difference rather than
   an assertion about a value nobody varied. Err.Config.default is what a caller
   that forgets Err_policy.apply would get. *)
let%expect_test "Err's own default captures a stack" =
  Err.Config.with_config Err_policy.deterministic (fun () ->
      Err.Config.with_config Err.Config.default (fun () ->
          report "Err.Config.default" (fail_here ()));
      report "restored" (fail_here ()));
  [%expect
    {|
    Err.Config.default: explicit-source=true automatic-stack=true events=0
    restored: explicit-source=true automatic-stack=false events=0
    |}]

(* The event trail survives the policy - which Err.Config.fast would have
   discarded, since it selects no actions at all. [boundaries] excludes Detect,
   so a plain failure carries no event and a crossing carries one. *)
let%expect_test "boundary crossings are recorded, detections are not" =
  Err.Config.with_config Err_policy.deterministic (fun () ->
      let crossed =
        Err.fail ~pos:__POS__ ~pp_error:Fmt.string "boom"
        |> Err.map_error ~pos:__POS__ ~pp_error:Fmt.string (fun m -> m ^ " (mapped)")
        |> Result.get_error
      in
      report "after one map" crossed;
      Fmt.pr "payload: %s@." (Err.Error.kind crossed));
  [%expect
    {|
    after one map: explicit-source=true automatic-stack=false events=1
    payload: boom (mapped)
    |}]

(* [of_error] is the only way to build a diagnostic now, so this pins what it
   builds: the domain supplies both strings and the layout around them is
   Diagnostic's. The migration's own guarantee - that routing through a typed
   error reproduced what the string constructor produced - was checked against
   the string constructor while it existed, and is now held by the 63 cram lines
   that never moved. *)
type demo = [ `Unknown_register of string ]

let pp_demo ppf : demo -> unit = function
  | `Unknown_register n -> Fmt.pf ppf "unknown register %%%s" n

let demo_code : demo -> string = function `Unknown_register _ -> "x86.operand"

let%expect_test "of_error renders the domain's own code and message" =
  let origin = Origin.producer ~tool:"test" () in
  let d = Diagnostic.of_error ~origin ~code:demo_code ~pp:pp_demo (`Unknown_register "ah") in
  Fmt.pr "rendered: %s@." (Diagnostic.render d);
  Fmt.pr "code=%S message=%S@." (Diagnostic.code d) (Diagnostic.message d);
  [%expect
    {|
    rendered: <test>: error[x86.operand]: unknown register %ah
    code="x86.operand" message="unknown register %ah"
    |}]
