let max_list = 200

(* Skips a string literal or a comment starting at [i]; [None] when [i] starts neither. *)
let skip s i =
  let n = String.length s in
  if s.[i] = '"' then (
    let j = ref (i + 1) in
    while !j < n && s.[!j] <> '"' do
      if s.[!j] = '\\' then incr j;
      incr j
    done;
    Some (!j + 1))
  else if s.[i] = '\'' && i + 2 < n && s.[i + 2] = '\'' then Some (i + 3)
  else if s.[i] = '\'' && i + 3 < n && s.[i + 1] = '\\' && s.[i + 3] = '\'' then Some (i + 4)
  else if s.[i] = '(' && i + 1 < n && s.[i + 1] = '*' then (
    let depth = ref 1 and j = ref (i + 2) in
    while !j < n && !depth > 0 do
      if s.[!j] = '(' && !j + 1 < n && s.[!j + 1] = '*' then (
        incr depth;
        j := !j + 2)
      else if s.[!j] = '*' && !j + 1 < n && s.[!j + 1] = ')' then (
        decr depth;
        j := !j + 2)
      else incr j
    done;
    Some !j)
  else None

let ident_char c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true | _ -> false

let word_at s i w =
  let n = String.length s and k = String.length w in
  i + k <= n
  && String.sub s i k = w
  && (i = 0 || not (ident_char s.[i - 1]))
  && (i + k = n || not (ident_char s.[i + k]))

(* [module M (] opens a functor definition. *)
let functor_at s i =
  word_at s i "module"
  &&
  let n = String.length s in
  let j = ref (i + 6) in
  while !j < n && s.[!j] = ' ' do
    incr j
  done;
  if !j < n && s.[!j] >= 'A' && s.[!j] <= 'Z' then (
    while !j < n && ident_char s.[!j] do
      incr j
    done;
    while !j < n && (s.[!j] = ' ' || s.[!j] = '\n') do
      incr j
    done;
    !j < n && s.[!j] = '(')
  else false

type frame = List of int | Other

let check text =
  let n = String.length text in
  let line = ref 1 in
  let fail what = Error (Printf.sprintf "line %d: %s" !line what) in
  let rec go i stack =
    if i >= n then Ok ()
    else
      match skip text i with
      | Some j ->
          for k = i to min (j - 1) (n - 1) do
            if text.[k] = '\n' then incr line
          done;
          go j stack
      | None -> (
          let c = text.[i] in
          if c = '\n' then incr line;
          if word_at text i "functor" || functor_at text i then fail "a functor"
          else
            match (c, stack) with
            | '[', _ when i + 1 < n && text.[i + 1] = '|' -> go (i + 2) (Other :: stack)
            | '[', _ -> go (i + 1) (List 0 :: stack)
            | ('(' | '{'), _ -> go (i + 1) (Other :: stack)
            | (']' | ')' | '}'), List k :: rest ->
                if k + 1 > max_list then
                  fail (Printf.sprintf "a list literal of %d elements" (k + 1))
                else go (i + 1) rest
            | (']' | ')' | '}'), _ :: rest -> go (i + 1) rest
            | ';', List k :: rest when not (i + 1 < n && text.[i + 1] = ';') ->
                let k = k + 1 in
                if k + 1 > max_list then
                  fail (Printf.sprintf "a list literal of over %d elements" max_list)
                else go (i + 1) (List k :: rest)
            | _ -> go (i + 1) stack)
  in
  go 0 []

let check_files files =
  List.fold_left
    (fun acc (path, text) ->
      Result.bind acc (fun () ->
          match check text with
          | Ok () -> Ok ()
          | Error what ->
              Error
                (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp
                   (Tool_error.v ~path Tool_error.Validate ("emitted code has " ^ what)))))
    (Ok ()) files
