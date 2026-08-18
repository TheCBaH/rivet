let per_line = 16

let of_bytes s =
  let n = String.length s in
  if n = 0 then ""
  else begin
    let b = Buffer.create ((n * 3) + (n / per_line) + 1) in
    String.iteri
      (fun i c ->
        if i > 0 then Buffer.add_char b (if i mod per_line = 0 then '\n' else ' ');
        Buffer.add_string b (Printf.sprintf "%02x" (Char.code c)))
      s;
    Buffer.add_char b '\n';
    Buffer.contents b
  end

let fail detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v Tool_error.Parse detail)

let digit c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | _ -> None

let parse s =
  if s = "" then Ok ""
  else if s.[String.length s - 1] <> '\n' then fail "hex dump has no final newline"
  else begin
    let b = Buffer.create (String.length s / 3) in
    let lines = String.split_on_char '\n' (String.sub s 0 (String.length s - 1)) in
    let rec do_lines lineno = function
      | [] -> Ok (Buffer.contents b)
      | line :: rest ->
          let tokens = String.split_on_char ' ' line in
          if List.exists (fun t -> t = "") tokens then
            fail (Printf.sprintf "line %d has an empty byte token" lineno)
          else if List.length tokens > per_line then
            fail
              (Printf.sprintf "line %d has %d bytes, more than %d" lineno (List.length tokens)
                 per_line)
          else
            let rec do_tokens = function
              | [] -> do_lines (lineno + 1) rest
              | t :: ts -> (
                  if String.length t <> 2 then
                    fail (Printf.sprintf "line %d: %S is not a two-digit byte" lineno t)
                  else
                    match (digit t.[0], digit t.[1]) with
                    | Some hi, Some lo ->
                        Buffer.add_char b (Char.chr ((hi * 16) + lo));
                        do_tokens ts
                    | _ -> fail (Printf.sprintf "line %d: %S is not lowercase hexadecimal" lineno t)
                  )
            in
            do_tokens tokens
    in
    do_lines 1 lines
  end
