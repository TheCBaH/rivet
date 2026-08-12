(* The process-wide err_trace policy.

   [Err] has two independent nondeterminism sources, and this module turns one
   of them off while keeping the other.

   The one that must go is automatic stack capture. [Err.Config.default], which
   is what [Err] starts with if nobody says otherwise, uses [Origin] backtraces:
   every detected error calls [Printexc.get_callstack]. That text carries host
   paths and frame counts, which .ai/asm_plan.md §3.7 bans from any diagnostic
   string, and it differs between native OCaml, js_of_ocaml and Melange - so a
   rendering that reached an expect or cram baseline could not agree across the
   three backends §11.6 requires. [Never] is therefore not a preference here, it
   is a correctness condition.

   The one that stays is the explicit [~pos:__POS__] origin and the semantic
   event trail. Neither is a nondeterminism source: [__POS__] is a compile-time
   constant, and under [Never] an event's origin holds that constant and no
   stack. Keeping them is the point of the library - the trail is what records
   parse -> simplify -> lower -> bind as data rather than as a convention in the
   diagnostic code prefix.

   This is why the policy is spelled out rather than taken from
   [Err.Config.fast]: [fast] is deterministic, but it selects no actions at all,
   so it would silently discard the trail along with the stacks. [boundaries] is
   every action except [Detect], which is not a loss - a detection is already
   described by the error's own origin, so recording it again as an event would
   duplicate it.

   Even so, no [Err] origin, stack, event or observation text may be rendered
   into a baseline; see asm/docs/errors.md. Determinism here makes that rule
   cheap to hold, not unnecessary. *)

(* Upstream added this preset from this project's adoption feedback. It is the
   exact policy described above: boundary events and explicit positions stay,
   while automatic stack capture is disabled on every backend. *)
let deterministic = Err.Config.deterministic

(* Called by each entry point - the CLI and the two browser adapters - rather
   than at module initialization, so that linking this library never mutates
   process state on its own. *)
let apply () = Err.Config.set deterministic

(* Host-supplied configuration, for the CLI's tracing switch. Exposed from here
   rather than used directly so callers cannot reach past the policy and enable
   backtraces by accident: [of_strings] can return any [backtrace] mode, and a
   caller that wants one has to say so through this name. *)
let of_strings = Err.Config.of_strings
let pp_of_strings_error = Err.Config.pp_of_strings_error
