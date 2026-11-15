(* The standard list functions whose stack use grows with the list in OCaml 4.14, the minimum
   supported: shadow [List] with this where the lists are as long as the program. *)

include List

let map f l = rev (rev_map f l)

let map2 f a b =
  let rec go acc a b =
    match (a, b) with
    | [], [] -> rev acc
    | x :: a, y :: b -> go (f x y :: acc) a b
    | _ -> invalid_arg "List.map2"
  in
  go [] a b

let append a b = rev_append (rev a) b
let concat l = rev (fold_left (fun acc x -> rev_append x acc) [] l)
