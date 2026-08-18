type stream = Stdout | Stderr
type t = { stream : stream; text : string }

(* Exactly one trailing newline, not all of them: a diff body legitimately ends
   with a blank line, and stripping greedily would silently delete it. *)
let strip_one_newline s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\n' then String.sub s 0 (n - 1) else s

let stdout text = { stream = Stdout; text = strip_one_newline text }
let stderr text = { stream = Stderr; text = strip_one_newline text }

let pp ppf t =
  Format.fprintf ppf "%s[%s]" (match t.stream with Stdout -> "out" | Stderr -> "err") t.text
