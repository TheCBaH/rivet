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

let all_features components = List.map (fun (c : Target_component.t) -> c.feature) components
let default components = S.of_list (all_features components)

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
              c.conflicts)
      components
  in
  match problems with [] -> Ok set | p :: _ -> Error p

let resolve components spec =
  let known_names = S.of_list (all_features components) in
  let step acc item =
    match acc with
    | Error _ as e -> e
    | Ok set -> (
        match item with
        | None_ -> Ok S.empty
        | (Enable f | Disable f) when not (S.mem f known_names) ->
            Error (Printf.sprintf "unknown feature %s (known: %s)" f (known components))
        | Enable f -> Ok (enable_closure components set S.empty f)
        | Disable f -> Ok (S.remove f set))
  in
  match List.fold_left step (Ok (default components)) spec with
  | Error _ as e -> e
  | Ok set -> validate components set

let enabled t f = S.mem f t
let features t = S.elements t
let equal = S.equal
let pp ppf t = Fmt.(list ~sep:(any ",") string) ppf (features t)
