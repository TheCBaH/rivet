module S = Set.Make (String)

type t = S.t
type item = Enable of string | Disable of string | None_
type spec = item list

let default_spec = []

let parse_item raw =
  let s = String.trim raw in
  if String.equal s "" then Error "empty feature item"
  else if String.equal s "none" then Ok None_
  else
    let body = String.sub s 1 (String.length s - 1) in
    match s.[0] with
    | '+' -> if body = "" then Error "'+' needs a feature name" else Ok (Enable (String.trim body))
    | '-' -> if body = "" then Error "'-' needs a feature name" else Ok (Disable (String.trim body))
    | _ -> Ok (Enable s)

let parse_spec text =
  if String.equal (String.trim text) "" then Ok default_spec
  else
    List.fold_left
      (fun acc raw ->
        match (acc, parse_item raw) with
        | Ok items, Ok item -> Ok (item :: items)
        | (Error _ as e), _ -> e
        | _, (Error _ as e) -> e)
      (Ok []) (String.split_on_char ',' text)
    |> Result.map List.rev

let spec_to_string spec =
  String.concat ","
    (List.map (function Enable f -> "+" ^ f | Disable f -> "-" ^ f | None_ -> "none") spec)

let usable (c : Target_component.t) = Option.is_none c.not_applicable
let all_features components = List.map (fun (c : Target_component.t) -> c.feature) components

let find components f =
  List.find_opt (fun (c : Target_component.t) -> String.equal c.feature f) components

let known components =
  match all_features components with [] -> "(none)" | fs -> String.concat ", " fs

(* Enabling a feature enables what it requires, transitively. The seen set keeps a requirement
   cycle from looping. *)
let rec enable_closure components set seen f =
  if S.mem f seen then set
  else
    match find components f with
    | None -> set
    | Some c ->
        List.fold_left
          (fun set r -> enable_closure components set (S.add f seen) r)
          (S.add f set) c.requires

let rec holds set : Target_component.cond -> bool = function
  | Target_component.Always -> true
  | Target_component.Has f -> S.mem f set
  | Target_component.Xlen _ -> false
  | Target_component.And cs -> List.for_all (holds set) cs

(* The implications of enabled components whose condition holds, as the features they name. *)
let implied components set =
  List.concat_map
    (fun (c : Target_component.t) ->
      if not (S.mem c.feature set) then []
      else List.filter_map (fun (cond, f) -> if holds set cond then Some f else None) c.implies)
    components

(* Close [set] under requirements and implications, never adding a feature in [denied]: an
   explicit removal is kept, and validation then reports the implication it contradicts. Run
   after every spec item so the result does not depend on the order features were named. *)
let rec close components denied set =
  let next =
    List.fold_left
      (fun set f ->
        if S.mem f denied || S.mem f set then set
        else
          let closure = enable_closure components S.empty S.empty f in
          if S.disjoint closure denied then S.union set closure else set)
      set (implied components set)
  in
  if S.equal next set then set else close components denied next

let default components =
  let seed =
    List.fold_left
      (fun set (c : Target_component.t) ->
        if c.default_on && usable c then enable_closure components set S.empty c.feature else set)
      S.empty components
  in
  close components S.empty seed

let all_enabled components =
  let all =
    List.fold_left
      (fun set (c : Target_component.t) ->
        if usable c then enable_closure components set S.empty c.feature else set)
      S.empty components
  in
  close components S.empty all

let validate components set =
  let problems =
    List.concat_map
      (fun (c : Target_component.t) ->
        if not (S.mem c.feature set) then []
        else
          List.filter_map
            (fun r ->
              if S.mem r set then None
              else Some (Printf.sprintf "feature %s requires %s, which is disabled" c.feature r))
            c.requires
          @ List.filter_map
              (fun x ->
                if S.mem x set then Some (Printf.sprintf "feature %s conflicts with %s" c.feature x)
                else None)
              c.conflicts
          @ List.filter_map
              (fun (cond, f) ->
                if holds set cond && not (S.mem f set) then
                  Some (Printf.sprintf "feature %s is implied by %s, which is enabled" f c.feature)
                else None)
              c.implies)
      components
  in
  match problems with [] -> Ok set | p :: _ -> Error p

(* [denied] holds the features the spec removed explicitly. A later [+f] overrides an earlier
   [-f], and [none] forgets both. *)
let apply components start spec =
  let known_names = S.of_list (all_features components) in
  let step acc item =
    match acc with
    | Error _ as e -> e
    | Ok (set, denied) -> (
        match item with
        | None_ -> Ok (S.empty, S.empty)
        | (Enable f | Disable f) when not (S.mem f known_names) ->
            Error (Printf.sprintf "unknown feature %s (known: %s)" f (known components))
        | Enable f -> (
            match find components f with
            | Some { Target_component.not_applicable = Some why; _ } ->
                Error (Printf.sprintf "feature %s is %s" f why)
            | _ ->
                let enabled = enable_closure components S.empty S.empty f in
                let denied = S.diff denied enabled in
                Ok (close components denied (S.union set enabled), denied))
        | Disable f ->
            let denied = S.add f denied in
            Ok (close components denied (S.remove f set), denied))
  in
  match List.fold_left step (Ok (start, S.empty)) spec with
  | Error _ as e -> e
  | Ok (set, _) -> validate components set

let resolve components spec = apply components (default components) spec
let enabled t f = S.mem f t
let features t = S.elements t
let equal = S.equal
let pp ppf t = Fmt.(list ~sep:(any ",") string) ppf (features t)
