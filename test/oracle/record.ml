(* The result-record recovery validator (asm/docs/exec-abi-v1.md §15.2).

   Four fixed steps, in this order, and the order is the whole design:

     1. unconditional framing        - true of every recoverable record
     2. raw pending-error patterns   - returns immediately
     3. canonical committed state    - state-dependent
     4. the result-staging matrix

   Step 2 has to precede step 3 because it recognizes records the step-3 rules
   deliberately reject: the undefined-field-zero rule rejects a staged
   error_subcode, and the state-combination rule rejects pre_run_error with
   runner_error = 0. Placing them after would make both patterns unreachable -
   they would be checks that exist and can never fire, which is exactly the
   defect class §16.1's reachability requirement exists to catch.

   Because step 2 returns immediately, each pattern is written as a *complete*
   wire predicate: it must itself enforce every step-3 and step-4 invariant it
   bypasses. A pattern that only looked at three fields would accept a record
   with a corrupt status, case_id, expected[], n_values, actuals or diagnostics,
   since the validator stops before the checks that would have rejected them.

   The validator is pure and reads a plain string, so the synthetic cases §16.4
   requires - the terminal fault schema no user-mode helper can publish, and
   records interrupted on both sides of the step-14 n_values store - are ordinary
   unit tests rather than something only QEMU can produce. *)

type t = {
  record_state : Abi.record_state;
  status : Abi.status;
  case_id : int64;
  n_expected : int;
  n_values : int;
  expected : int64 array;
  values : int64 array;
  diag : string;
  runner_error : Abi.error_class option;
  error_subcode : int;
  staging_ignored : bool;
      (** true in the two [running] cells of the §15.2 step-4 matrix, where step 14 is interruptible
          and [values]/[diag] carry no information *)
  staged_subcode_ignored : bool;
      (** true for raw pattern A: a bootstrap record with a staged subcode *)
}

type verdict =
  | Valid of t
  | Pending_pre_run_error
      (** raw pattern B. The class never committed and cannot be inferred, so the host reports
          [result = unavailable] rather than guessing one. *)
  | Invalid of string

(* {1 Field readers} *)

let is_zero_range s off len =
  let rec go i = i >= len || (String.get_uint8 s (off + i) = 0 && go (i + 1)) in
  go 0

let u64_array s off n = Array.init n (fun i -> Abi.get_u64 s (off + (8 * i)))

(* {1 Step 1 - unconditional framing}

   These hold in every recoverable record whatever its state, which is what
   makes them safe to run before pattern recognition. The zero page tail is not
   decoration: the guest can see the result page through the second alias, so a
   stray guest store lands here and must not be mistaken for a valid record. *)

let check_framing ~abi_version s =
  let result_magic = Abi_v3.result_magic_for_version abi_version in
  if String.length s <> Abi.result_page_size then
    Some
      (Printf.sprintf "framing: result file is %d bytes, expected %d" (String.length s)
         Abi.result_page_size)
  else if String.sub s Abi.Result_off.magic (String.length result_magic) <> result_magic then
    Some "framing: bad result magic"
  else if Abi.get_u16 s Abi.Result_off.abi_version <> abi_version then
    Some
      (Printf.sprintf "framing: abi_version %d, expected %d"
         (Abi.get_u16 s Abi.Result_off.abi_version)
         abi_version)
  else if not (is_zero_range s Abi.Result_off.reserved Abi.Result_off.reserved_len) then
    Some "framing: reserved bytes nonzero"
  else if
    not (is_zero_range s Abi.result_record_size (Abi.result_page_size - Abi.result_record_size))
  then Some "framing: page tail nonzero"
  else None

(* {1 Step 2 - raw pending-error patterns}

   "Every other bootstrap and result-staging field canonical zero" spelled out:
   with runner_error = 0 the only nonzero value such a record may carry is the
   staged error_subcode itself.

   "Nonzero" means exactly a nonzero u16 and nothing more. With runner_error = 0
   the class commit has not happened, and pattern B says outright that the class
   cannot be inferred - so this cannot consult a class-scoped subcode table, and
   ABI v1 therefore does not need a globally unique subcode namespace. An
   unknown nonzero subcode is a required test case for exactly this reason. *)

let is_pending_shape s =
  Abi.get_u16 s Abi.Result_off.status = Abi.status_code Abi.Not_started
  && Abi.get_u16 s Abi.Result_off.runner_error = 0
  && Abi.get_u16 s Abi.Result_off.error_subcode <> 0
  && Abi.get_u32 s Abi.Result_off.case_id = 0L
  && Abi.get_u16 s Abi.Result_off.n_expected = 0
  && Abi.get_u16 s Abi.Result_off.n_values = 0
  && Abi.get_u32 s Abi.Result_off.diag_len = 0L
  && is_zero_range s Abi.Result_off.expected (8 * Abi.n_expected_max)
  && is_zero_range s Abi.Result_off.values (8 * Abi.n_values_max)
  && is_zero_range s Abi.Result_off.diag Abi.diag_max

let bootstrap_record_of s =
  {
    record_state = Abi.Bootstrap;
    status = Abi.Not_started;
    case_id = 0L;
    n_expected = 0;
    n_values = 0;
    expected = [||];
    values = [||];
    diag = "";
    runner_error = None;
    error_subcode = 0;
    staging_ignored = false;
    staged_subcode_ignored = Abi.get_u16 s Abi.Result_off.error_subcode <> 0;
  }

(* {1 Steps 3 and 4} *)

(* §15.2 step 4, verbatim. Not a "terminal versus nonterminal" rule: three
   informal statements of that kind give different answers for a committed
   validated/not_started record, which is precisely the state §15.1 uses to
   classify a helper failure between the validated and running commits. *)
type staging = Zero | Ignored | Fault_terminal | Value_terminal

let staging_rule state status =
  match (state, status) with
  | Abi.Bootstrap, Abi.Not_started -> Some Zero
  | Abi.Pre_run_error, Abi.Not_started -> Some Zero
  | Abi.Validated, Abi.Not_started -> Some Zero
  | Abi.Validated, Abi.Running -> Some Ignored
  | Abi.Returned, Abi.Running -> Some Ignored
  | Abi.Validated, Abi.Fault -> Some Fault_terminal
  | Abi.Returned, (Abi.Passed | Abi.Failed) -> Some Value_terminal
  | _ -> None

let validate ?(abi_version = Abi.abi_version) ~expected_case_id s =
  match check_framing ~abi_version s with
  | Some why -> Invalid why
  | None -> (
      let raw_state = Abi.get_u32 s Abi.Result_off.record_state in
      let raw_status = Abi.get_u16 s Abi.Result_off.status in
      let runner_error = Abi.get_u16 s Abi.Result_off.runner_error in
      let error_subcode = Abi.get_u16 s Abi.Result_off.error_subcode in
      let case_id = Abi.get_u32 s Abi.Result_off.case_id in
      let n_expected = Abi.get_u16 s Abi.Result_off.n_expected in
      let n_values = Abi.get_u16 s Abi.Result_off.n_values in
      let diag_len = Abi.get_u32 s Abi.Result_off.diag_len in
      (* Step 2. *)
      if raw_state = 0L && is_pending_shape s then Valid (bootstrap_record_of s)
      else if raw_state = 1L && is_pending_shape s then Pending_pre_run_error
      else
        (* Step 3. *)
        let state =
          if Int64.unsigned_compare raw_state 4L < 0 then
            Abi.record_state_of_code (Int64.to_int raw_state)
          else None
        in
        match (state, Abi.status_of_code raw_status) with
        | None, _ -> Invalid (Printf.sprintf "state: unknown record_state %Lu" raw_state)
        | _, None -> Invalid (Printf.sprintf "state: unknown status %d" raw_status)
        | Some state, Some status -> (
            let err =
              (* One committed-error schema, applied whatever the state: the
                 class must be known, a committed class must carry a subcode
                 that exists in that class (this is where class/subcode
                 compatibility is validated - after the commit, never before),
                 and a subcode without a committed class is staging that only
                 the step-2 patterns may carry. *)
              if runner_error = 0 then
                if error_subcode <> 0 then
                  Some
                    (Printf.sprintf "state: error_subcode %d staged with runner_error = 0"
                       error_subcode)
                else None
              else
                match Abi.class_of_code runner_error with
                | None -> Some (Printf.sprintf "state: unknown runner_error class %d" runner_error)
                | Some cls -> (
                    if error_subcode = 0 then
                      Some
                        (Printf.sprintf "state: class %s committed with subcode 0"
                           (Abi.class_name cls))
                    else
                      match Abi_v3.subcode_of_wire_for_version ~abi_version cls error_subcode with
                      | None ->
                          Some
                            (Printf.sprintf "state: subcode %d is not defined for class %s"
                               error_subcode (Abi.class_name cls))
                      | Some _ -> None)
            in
            let combination () =
              if not (Abi.status_admitted state status) then
                Some
                  (Printf.sprintf "state: status %s is not admitted by record_state %s"
                     (Abi.status_name status) (Abi.record_state_name state))
              else if
                runner_error <> 0
                && match status with Abi.Passed | Abi.Failed | Abi.Fault -> true | _ -> false
              then Some "state: runner_error committed with a terminal status"
              else if state = Abi.Pre_run_error && runner_error = 0 then
                (* Not a pending pattern (step 2 already declined it) and not a
                   committed error either: the zero-subcode row of §15.2. *)
                Some "state: pre_run_error without a committed runner_error"
              else None
            in
            let trusted_fields () =
              match state with
              | Abi.Bootstrap | Abi.Pre_run_error ->
                  if case_id <> 0L then Some "state: case_id set before the manifest was trusted"
                  else if n_expected <> 0 then
                    Some "state: n_expected set before the manifest was trusted"
                  else if not (is_zero_range s Abi.Result_off.expected (8 * Abi.n_expected_max))
                  then Some "state: expected[] set before the manifest was trusted"
                  else None
              | Abi.Validated | Abi.Returned ->
                  if case_id <> Int64.of_int expected_case_id then
                    Some (Printf.sprintf "state: case_id %Lu, expected %d" case_id expected_case_id)
                  else
                    (* v3 (exec-abi-v3.md §4): n_expected is 1..8 rather than
                       forced to 1; v1/v2 keep their exact-1 rule unchanged. *)
                    let n_expected_valid, n_expected_err =
                      if abi_version = Abi_v3.abi_version then
                        ( n_expected >= 1 && n_expected <= Abi.n_expected_max,
                          Printf.sprintf "state: n_expected %d, expected 1..%d" n_expected
                            Abi.n_expected_max )
                      else
                        ( n_expected = 1,
                          Printf.sprintf "state: n_expected %d, expected 1" n_expected )
                    in
                    if not n_expected_valid then Some n_expected_err
                    else if
                      not
                        (is_zero_range s
                           (Abi.Result_off.expected + (8 * n_expected))
                           (8 * (Abi.n_expected_max - n_expected)))
                    then Some "state: expected[] tail beyond n_expected nonzero"
                    else None
            in
            (* Step 4. *)
            let rule = staging_rule state status in
            let staging () =
              match rule with
              | None ->
                  Some
                    (Printf.sprintf "staging: (%s, %s) is not a cell of the matrix"
                       (Abi.record_state_name state) (Abi.status_name status))
              | Some Ignored -> None
              | Some Zero ->
                  if n_values <> 0 then
                    Some (Printf.sprintf "staging: n_values %d, expected 0" n_values)
                  else if diag_len <> 0L then Some "staging: diag_len nonzero before staging began"
                  else if not (is_zero_range s Abi.Result_off.values (8 * Abi.n_values_max)) then
                    Some "staging: actuals nonzero before staging began"
                  else if not (is_zero_range s Abi.Result_off.diag Abi.diag_max) then
                    Some "staging: diagnostic bytes nonzero before staging began"
                  else None
              | Some ((Fault_terminal | Value_terminal) as terminal) ->
                  (* Value completeness applies only to passed/failed. A fault
                     records no observation, so requiring n_values = n_expected
                     there would let the initialized zeros in values[] masquerade
                     as real observations from a guest that never returned any.

                     [n_expected] rather than a literal 1: by the time [staging]
                     runs, [trusted_fields] has already forced it to exactly 1
                     for every v1/v2 record (exec-abi-v3.md §4's "n_values must
                     equal n_expected" generalizes cleanly to v1/v2 because their
                     own n_expected is already pinned upstream). *)
                  let want_values = if terminal = Value_terminal then n_expected else 0 in
                  if n_values <> want_values then
                    Some (Printf.sprintf "staging: n_values %d, expected %d" n_values want_values)
                  else if
                    not
                      (is_zero_range s
                         (Abi.Result_off.values + (8 * want_values))
                         (8 * (Abi.n_values_max - want_values)))
                  then Some "staging: actual slots beyond n_values nonzero"
                  else if Int64.unsigned_compare diag_len (Int64.of_int Abi.diag_max) > 0 then
                    Some (Printf.sprintf "staging: diag_len %Lu exceeds %d" diag_len Abi.diag_max)
                  else
                    let d = Int64.to_int diag_len in
                    if not (is_zero_range s (Abi.Result_off.diag + d) (Abi.diag_max - d)) then
                      Some "staging: diagnostic tail nonzero"
                    else None
            in
            (* Precedence: the committed-error schema, then the state
               combination, then the state's trusted fields, then staging. A
               record that violates several rules reports the earliest, so a
               conformance expectation is a single string rather than "one of". *)
            let ( ||> ) a f = match a with Some _ -> a | None -> f () in
            match err ||> combination ||> trusted_fields ||> staging with
            | Some why -> Invalid why
            | None ->
                let ignored = staging_rule state status = Some Ignored in
                Valid
                  {
                    record_state = state;
                    status;
                    case_id;
                    n_expected;
                    n_values = (if ignored then 0 else n_values);
                    expected = u64_array s Abi.Result_off.expected n_expected;
                    values = (if ignored then [||] else u64_array s Abi.Result_off.values n_values);
                    diag =
                      (if ignored then ""
                       else String.sub s Abi.Result_off.diag (Int64.to_int diag_len));
                    runner_error = Abi.class_of_code runner_error;
                    error_subcode;
                    staging_ignored = ignored;
                    staged_subcode_ignored = false;
                  }))

(* {1 Building records}

   The writer side of the same layout. Conformance needs to fabricate records
   the user-mode helper cannot produce - the terminal fault schema, and
   interruptions on both sides of a commit store - and it needs them built from
   the same offsets the validator reads, so that a transcription slip fails
   loudly instead of making a test agree with itself. *)

let build ?(abi_version = Abi.abi_version) ?(record_state = Abi.Bootstrap)
    ?(status = Abi.Not_started) ?(case_id = 0) ?(expected = [||]) ?(values = [||]) ?(n_values = -1)
    ?(n_expected = -1) ?(diag = "") ?(runner_error = 0) ?(error_subcode = 0) () =
  let b = Bytes.make Abi.result_page_size '\000' in
  Abi.set_string b Abi.Result_off.magic (Abi_v3.result_magic_for_version abi_version);
  Abi.set_u16 b Abi.Result_off.abi_version abi_version;
  Abi.set_u16 b Abi.Result_off.status (Abi.status_code status);
  Abi.set_u32 b Abi.Result_off.case_id case_id;
  Abi.set_u16 b Abi.Result_off.n_expected
    (if n_expected >= 0 then n_expected else Array.length expected);
  Abi.set_u16 b Abi.Result_off.n_values (if n_values >= 0 then n_values else Array.length values);
  Abi.set_u32 b Abi.Result_off.diag_len (String.length diag);
  Array.iteri (fun i v -> Abi.set_u64 b (Abi.Result_off.expected + (8 * i)) v) expected;
  Array.iteri (fun i v -> Abi.set_u64 b (Abi.Result_off.values + (8 * i)) v) values;
  Abi.set_string b Abi.Result_off.diag diag;
  Abi.set_u16 b Abi.Result_off.runner_error runner_error;
  Abi.set_u16 b Abi.Result_off.error_subcode error_subcode;
  Abi.set_u32 b Abi.Result_off.record_state (Abi.record_state_code record_state);
  Bytes.to_string b

(* {1 Rendering} *)

let pp_u64_array ppf a =
  Fmt.pf ppf "[%s]"
    (String.concat " " (Array.to_list (Array.map (fun v -> Printf.sprintf "%Lu" v) a)))

let pp ppf t =
  Fmt.pf ppf "%s/%s case_id=%Lu n_expected=%d expected=%a"
    (Abi.record_state_name t.record_state)
    (Abi.status_name t.status) t.case_id t.n_expected pp_u64_array t.expected;
  if t.staging_ignored then Fmt.pf ppf " staging=ignored"
  else Fmt.pf ppf " n_values=%d values=%a diag=%S" t.n_values pp_u64_array t.values t.diag;
  (match t.runner_error with
  | None -> ()
  | Some c -> (
      Fmt.pf ppf " runner_error=%s" (Abi.class_name c);
      (* [t] does not retain the wire ABI version.  Use the richest known
         namespace when rendering so a v2- or v3-only error keeps its
         symbolic name; validation has already rejected those subcodes for
         records at an earlier ABI version. *)
      match Abi_v3.subcode_of_wire c t.error_subcode with
      | Some sc -> Fmt.pf ppf ".%s" sc.Abi.name
      | None -> Fmt.pf ppf ".%d" t.error_subcode));
  if t.staged_subcode_ignored then Fmt.pf ppf " staged_subcode=ignored"

let pp_verdict ppf = function
  | Valid t -> pp ppf t
  | Pending_pre_run_error -> Fmt.pf ppf "pending_pre_run_error (result unavailable)"
  | Invalid why -> Fmt.pf ppf "invalid: %s" why
