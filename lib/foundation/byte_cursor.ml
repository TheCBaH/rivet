(* A cursor over a byte string, and a builder for one.

   Pure OCaml over [string] and [Buffer] (.ai/asm_plan.md §3.7): no [Bigarray],
   no native pointers, nothing that would stop this compiling under js_of_ocaml
   or Melange. That constraint is the reason this module exists at all rather
   than the code reaching for [Bytes.unsafe_get] and an offset.

   The reader is a value, not a mutable position: [read] returns the byte *and*
   the advanced cursor. A decoder that backtracks - which every [Alt] does -
   then does so by keeping the earlier cursor, instead of by saving and
   restoring a mutable field and hoping every path restores it. *)

type t = { data : string; pos : int; limit : int }

(* The one way to build a cursor badly, and therefore this module's whole error
   domain. The payload is the three numbers that decide it, so a caller can say
   which bound it broke; the message is the one [invalid_arg] carried before.
   See asm/docs/errors.md. *)
type error = [ `Range_outside_string of range_outside_string ]

(* [start] rather than [pos]: the cursor record below also has a [pos], and the
   later definition would shadow it for every [t.pos] in this file. *)
and range_outside_string = { start : int; len : int option; length : int }

let pp_error ppf : error -> unit = function
  | `Range_outside_string _ -> Fmt.string ppf "Byte_cursor.of_string: range outside the string"

let error_code : error -> string = function `Range_outside_string _ -> "byte-cursor.range"

let of_string ?(pos = 0) ?len data =
  let limit = match len with None -> String.length data | Some n -> pos + n in
  if pos < 0 || limit > String.length data || limit < pos then
    Err.fail ~pos:__POS__ ~pp_error
      (`Range_outside_string { start = pos; len; length = String.length data })
  else Ok { data; pos; limit }

(* The total companion, for the callers that have already established the range -
   the same arrangement, and the same reasoning, as {!Bigint}'s. *)
let of_string_exn ?pos ?len data = Err.or_raise ~pos:__POS__ ~pp_error (of_string ?pos ?len data)
let position t = t.pos
let remaining t = t.limit - t.pos
let at_end t = t.pos >= t.limit

(* {1 Reading}

   Every reader returns an option rather than raising: running off the end of a
   byte string is an ordinary outcome for a decoder trying an alternative that
   does not fit, not an exceptional one. *)

let read_byte t =
  if at_end t then None else Some (Char.code t.data.[t.pos], { t with pos = t.pos + 1 })

let read_bytes t n =
  if n < 0 || n > remaining t then None
  else Some (String.sub t.data t.pos n, { t with pos = t.pos + n })

let skip t n = if n < 0 || n > remaining t then None else Some { t with pos = t.pos + n }
let peek_byte t = if at_end t then None else Some (Char.code t.data.[t.pos])

(* Multi-byte integers in both orders. The assembler needs both: instruction
   words are little-endian on all four M1 profiles, but a big-endian target is
   an explicit M6 goal, so the byte order is a parameter of the call rather than
   a property of the module. *)

let read_int_le t n =
  match read_bytes t n with
  | None -> None
  | Some (s, t') ->
      let v = ref 0L in
      for i = n - 1 downto 0 do
        v := Int64.logor (Int64.shift_left !v 8) (Int64.of_int (Char.code s.[i]))
      done;
      Some (!v, t')

let read_int_be t n =
  match read_bytes t n with
  | None -> None
  | Some (s, t') ->
      let v = ref 0L in
      for i = 0 to n - 1 do
        v := Int64.logor (Int64.shift_left !v 8) (Int64.of_int (Char.code s.[i]))
      done;
      Some (!v, t')

(* {1 Building} *)

module Builder = struct
  type t = Buffer.t

  let create ?(size = 64) () = Buffer.create size
  let byte b v = Buffer.add_char b (Char.chr (v land 0xff))
  let bytes b s = Buffer.add_string b s
  let length b = Buffer.length b
  let contents b = Buffer.contents b

  let int_le b ~width v =
    for i = 0 to width - 1 do
      byte b (Int64.to_int (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xFFL))
    done

  let int_be b ~width v =
    for i = width - 1 downto 0 do
      byte b (Int64.to_int (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xFFL))
    done
end

(* {1 Rendering}

   Hex in memory order, which is the order every committed artifact uses -
   oracle/text.hex, the manifest payloads, and the differential gate. objdump
   prints ARM and AArch64 instruction *words*, and both are little-endian, so a
   dump in word order would compare cleanly against the oracle and mean nothing.
*)

let to_hex ?(sep = " ") s =
  String.concat sep (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let of_hex s =
  let digits =
    List.filter
      (fun c -> c <> ' ' && c <> '\n' && c <> '\t')
      (List.init (String.length s) (String.get s))
  in
  let rec pair = function
    | [] -> Some []
    | [ _ ] -> None
    | a :: b :: rest -> (
        match int_of_string_opt (Printf.sprintf "0x%c%c" a b) with
        | None -> None
        | Some v -> ( match pair rest with None -> None | Some tl -> Some (Char.chr v :: tl)))
  in
  match pair digits with
  | None -> None
  | Some chars -> Some (String.init (List.length chars) (List.nth chars))
