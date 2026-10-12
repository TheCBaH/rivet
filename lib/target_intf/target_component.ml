type source = { upstream : string; name : string }

module Req = struct
  type t = Feature of string | All of t list | Any of t list

  let rec eval enabled = function
    | Feature f -> enabled f
    | All l -> List.for_all (eval enabled) l
    | Any l -> List.exists (eval enabled) l

  let rec unmet enabled = function
    | Feature f as r -> if enabled f then None else Some r
    | All l -> (
        match List.filter_map (unmet enabled) l with
        | [] -> None
        | [ x ] -> Some x
        | xs -> Some (All xs))
    | Any _ as r -> if eval enabled r then None else Some r

  let rec features = function Feature f -> [ f ] | All l | Any l -> List.concat_map features l

  let rec pp ppf = function
    | Feature f -> Fmt.string ppf f
    | All l -> Fmt.(list ~sep:(any " and ") pp) ppf l
    | Any l -> Fmt.pf ppf "one of %a" Fmt.(list ~sep:(any ", ") pp) l
end

type cond = Always | Has of string | Xlen of int | And of cond list
type form = { label : string; mnemonics : string list; sources : source list; legal : Req.t option }

type t = {
  id : string;
  feature : string;
  requires : string list;
  conflicts : string list;
  implies : (cond * string) list;
  default_on : bool;
  xlens : int list;
  not_applicable : string option;
  summary : string;
  forms : form list;
}

let rec specialize_cond ~xlen = function
  | Always -> Some Always
  | Has _ as c -> Some c
  | Xlen n -> if n = xlen then Some Always else None
  | And cs ->
      List.fold_left
        (fun acc c ->
          match (acc, specialize_cond ~xlen c) with
          | Some l, Some Always -> Some l
          | Some l, Some c -> Some (c :: l)
          | None, _ | _, None -> None)
        (Some []) cs
      |> Option.map (function [] -> Always | l -> And (List.rev l))

let specialize ~xlen c =
  let implies =
    List.filter_map
      (fun (cond, f) -> Option.map (fun cond -> (cond, f)) (specialize_cond ~xlen cond))
      c.implies
  in
  let not_applicable =
    if c.xlens = [] || List.mem xlen c.xlens then c.not_applicable
    else Some (Printf.sprintf "not applicable at XLEN %d" xlen)
  in
  { c with implies; not_applicable }

let legal_of c f = Option.value f.legal ~default:(Req.Feature c.feature)

let legal components m =
  List.find_map
    (fun c ->
      List.find_map (fun f -> if List.mem m f.mnemonics then Some (legal_of c f) else None) c.forms)
    components

let labels c = List.map (fun f -> f.label) c.forms
let mnemonics c = List.concat_map (fun f -> f.mnemonics) c.forms
let owns_mnemonic c m = List.exists (String.equal m) (mnemonics c)

let feature_of_mnemonic components m =
  List.find_map (fun c -> if owns_mnemonic c m then Some c.feature else None) components

let duplicates xs =
  let sorted = List.sort String.compare xs in
  let rec go acc = function
    | a :: (b :: _ as rest) -> go (if String.equal a b then a :: acc else acc) rest
    | _ -> acc
  in
  List.sort_uniq String.compare (go [] sorted)

let rec cond_features = function
  | Always | Xlen _ -> []
  | Has f -> [ f ]
  | And cs -> List.concat_map cond_features cs

let check components =
  let referenced =
    List.concat_map
      (fun c ->
        List.concat_map
          (fun f -> match f.legal with None -> [] | Some r -> Req.features r)
          c.forms)
      components
  in
  let per_component c =
    (if c.forms = [] && c.implies = [] && not (List.mem c.feature referenced) then
       [ Printf.sprintf "%s: no forms" c.id ]
     else [])
    @ List.concat_map
        (fun f ->
          (if f.mnemonics = [] then [ Printf.sprintf "%s: form %s has no mnemonic" c.id f.label ]
           else [])
          @
          if f.sources = [] then [ Printf.sprintf "%s: form %s has no source mapping" c.id f.label ]
          else [])
        c.forms
  in
  let dup what xs = List.map (fun d -> Printf.sprintf "duplicate %s %s" what d) (duplicates xs) in
  let features = List.map (fun c -> c.feature) components in
  let unknown c what fs =
    List.filter_map
      (fun f ->
        if List.mem f features then None
        else Some (Printf.sprintf "%s: %s names unknown feature %s" c.id what f))
      fs
  in
  List.concat_map per_component components
  @ List.concat_map
      (fun c ->
        unknown c "requires" c.requires @ unknown c "conflicts" c.conflicts
        @ unknown c "implies" (List.concat_map (fun (cond, f) -> f :: cond_features cond) c.implies)
        @ unknown c "legality"
            (List.concat_map
               (fun f -> match f.legal with None -> [] | Some r -> Req.features r)
               c.forms))
      components
  @ dup "feature" features
  @ dup "component id" (List.map (fun c -> c.id) components)
  @ dup "form label" (List.concat_map labels components)
  @ dup "mnemonic" (List.concat_map mnemonics components)
