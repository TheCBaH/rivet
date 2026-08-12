(* The bridge between the two error representations, for the duration of the
   typed-error migration and after it.

   [Diagnostic.t] is a rendering; a typed error domain is the currency (see
   asm/docs/errors.md). Between them sits [Err], whose wrapper carries the
   detection origin and the semantic event trail. This module names the
   combinations the codebase actually needs, so that no caller has to spell out
   [Err.fail ~pp_error:Diagnostic.pp_all] and get the printer wrong.

   The type is deliberately [Diagnostic.t list] rather than a domain: these are
   the *stage* signatures, at and above the point where a pass has already
   turned typed errors into their presentation, and where several independent
   failures are reported together. Below that point a pass returns
   [('a, <its own domain>) Err.t] and names no diagnostic at all. Independent
   checks enter this shape through [validate_all], so each failure is detected
   separately before the batch is collapsed to the reporting payload. *)

type 'a t = ('a, Diagnostic.t list) Err.t
(* A stage result: many diagnostics, wrapped once. *)

let pp_error = Diagnostic.pp_all
let pp_error1 = Diagnostic.pp

(* {1 Construction} *)

(* [?pos] is threaded rather than defaulted because a default would record this
   module's own position at every failure in the project, which is worse than
   recording none: it would look like provenance while pointing at the bridge.
   Callers pass [~pos:__POS__]. *)
let fail ?pos ds = Err.fail ?pos ~pp_error ds
let fail1 ?pos d = Err.fail ?pos ~pp_error:pp_error1 d

(* Run independent checks and retain every failure wrapper until the batch is
   collapsed into the stage's existing diagnostic-list domain. The resulting
   wrapper keeps the first failure's detection origin and trail; monitors have
   already observed every individual failure with its own printer and origin. *)
let validate_all ?pos checks =
  Err.Accum.all ?pos checks
  |> Err.Accum.fold_errors (fun errors -> List.map Err.Error.kind errors)
  |> Result.map (fun _ -> ())

(* {1 Reading the reporting payload} *)

let diagnostics e = Err.Error.kind e

(* {1 Phase boundaries}

   [stage] is what makes the event trail say parse -> simplify -> lower -> bind.
   It changes no payload; it records that a failure crossed a named boundary,
   at the position of the crossing. Under Err_policy.deterministic the [Map]
   action is selected and carries only the explicit [~pos], so this is free of
   host state. *)
let stage ?pos action r = Err.mark_error ?pos ~pp_error action r

(* {1 Rendering}

   The single place that turns a wrapped stage failure back into text. Every
   caller that used to write [Diagnostic.render_all ds] writes this instead, and
   the output is identical - which is what keeps the committed baselines valid
   through the migration. *)
let render e = Diagnostic.render_all (diagnostics e)
