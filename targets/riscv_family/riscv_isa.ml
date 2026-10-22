type isa_spec = [ `V2_2 | `V20191213 ]
type march = { xlen : int; spec : Target_config.spec; vlen : int option }

(* The features with no ISA-string name: GNU as accepts their instructions under any -march. *)
let unnamed = [ "system"; "s" ]

(* Under 2.2, I includes Zicsr and Zifencei. *)
let in_base_2_2 = [ "zicsr"; "zifencei" ]

(* Every other standard extension is at 1.0. *)
let version_1_0 =
  String.split_on_char ' '
    "b h v zmmul zacas zabha zfh zfhmin zfa zfbfmin zba zbb zbs zbc zbkb zbkc zbkx zk zkn zks zknd \
     zkne zknh zksed zksh zicond zihintntl zawrs zimop zicfiss zicfilp zicbom zicboz zicbop \
     svinval sdext ssctr zve32x zve32f zve64x zve64f zve64d zvbb zvbc zvkb zvkg zvkned zvknha \
     zvknhb zvksed zvksh zvkn zvks zvknc zvkng zvksc zvksg zvfbfmin zvfbfwma zca zcb zcd zcf zcmp \
     zcmt zcmop"

let version ~isa_spec name =
  match (isa_spec, name) with
  | `V2_2, ("i" | "m" | "a" | "f" | "d" | "q") -> Some (2, 0)
  | `V20191213, ("i" | "a") -> Some (2, 1)
  | `V20191213, "m" -> Some (2, 0)
  | `V20191213, ("f" | "d" | "q") -> Some (2, 2)
  | _, "c" -> Some (2, 0)
  | _, ("zicsr" | "zifencei" | "zicntr") -> Some (2, 0)
  | _, name when List.mem name version_1_0 -> Some (1, 0)
  | _, name when String.starts_with ~prefix:"zvl" name -> Some (1, 0)
  | _ -> None

(* Single letters in this order; a Z extension sorts by the category letter after its [z]. *)
let canonical = "imafdqlcbkjtpvh"
let rank c = match String.index_opt canonical c with Some i -> i | None -> String.length canonical

let sort_key name =
  if String.length name = 1 then (0, rank name.[0], name)
  else
    match name.[0] with
    | 'z' -> (1, rank name.[1], name)
    | 's' -> (2, 0, name)
    | 'x' -> (3, 0, name)
    | _ -> (4, 0, name)

let zvl ~vlen config =
  let on = Target_config.enabled config in
  let implied = if on "v" then 128 else if on "zve64x" then 64 else if on "zve32x" then 32 else 0 in
  let top = max implied (Option.value vlen ~default:0) in
  let rec from n = if n > top then [] else Printf.sprintf "zvl%db" n :: from (2 * n) in
  if top < 32 then [] else from 32

let to_string ?(isa_spec = `V20191213) ?vlen ~xlen config =
  let names =
    List.filter
      (fun f -> not (List.mem f unnamed || (isa_spec = `V2_2 && List.mem f in_base_2_2)))
      (Target_config.features config)
    @ zvl ~vlen config
  in
  let names = List.sort (fun a b -> compare (sort_key a) (sort_key b)) ("i" :: names) in
  let rec render acc = function
    | [] -> Ok (Printf.sprintf "rv%d%s" xlen (String.concat "_" (List.rev acc)))
    | name :: rest -> (
        match version ~isa_spec name with
        | Some (major, minor) -> render (Printf.sprintf "%s%dp%d" name major minor :: acc) rest
        | None -> Error (Printf.sprintf "no ISA-string version for feature %s" name))
  in
  render [] names

let is_digit c = c >= '0' && c <= '9'
let is_letter c = c >= 'a' && c <= 'z'

(* The end of a version at [i]: digits, then optionally [p] and digits. *)
let skip_version s i =
  let n = String.length s in
  let rec digits j = if j < n && is_digit s.[j] then digits (j + 1) else j in
  let j = digits i in
  if j > i && j + 1 < n && s.[j] = 'p' && is_digit s.[j + 1] then digits (j + 1) else j

(* A multi-letter name without a trailing [<major>p<minor>]. *)
let strip_version name =
  let n = String.length name in
  let rec back j = if j > 0 && is_digit name.[j - 1] then back (j - 1) else j in
  let minor = back n in
  if minor < n && minor > 1 && name.[minor - 1] = 'p' then
    let major = back (minor - 1) in
    if major < minor - 1 && major > 0 && is_letter name.[major - 1] then String.sub name 0 major
    else name
  else name

let zvl_length name =
  let n = String.length name in
  if n > 4 && String.starts_with ~prefix:"zvl" name && name.[n - 1] = 'b' then
    int_of_string_opt (String.sub name 3 (n - 4))
  else None

let of_march ?(isa_spec = `V20191213) march =
  let s = String.lowercase_ascii march in
  let n = String.length s in
  let xlen =
    if String.starts_with ~prefix:"rv32" s then Some 32
    else if String.starts_with ~prefix:"rv64" s then Some 64
    else None
  in
  match xlen with
  | None -> Error (Printf.sprintf "%s: an ISA string starts with rv32 or rv64" march)
  | Some _ when n = 4 -> Error (Printf.sprintf "%s: no base ISA" march)
  | Some xlen -> (
      let base =
        match s.[4] with
        | 'i' -> Ok []
        | 'g' -> Ok [ "m"; "a"; "f"; "d"; "zicsr"; "zifencei" ]
        | 'e' -> Error (Printf.sprintf "%s: RV%dE is not supported" march xlen)
        | c -> Error (Printf.sprintf "%s: unknown base ISA %c" march c)
      in
      (* The single letters run up to the first [_] or multi-letter prefix. *)
      let rec letters acc i =
        if i >= n then (List.rev acc, i)
        else
          match s.[i] with
          | '_' | 'z' | 's' | 'x' -> (List.rev acc, i)
          | c -> letters (String.make 1 c :: acc) (skip_version s (i + 1))
      in
      let multi rest =
        List.filter (fun w -> w <> "") (String.split_on_char '_' rest) |> List.map strip_version
      in
      match base with
      | Error _ as e -> e
      | Ok base ->
          let single, i = letters [] (skip_version s 5) in
          let words = multi (String.sub s i (n - i)) in
          let vlen =
            List.fold_left
              (fun acc w ->
                match (zvl_length w, acc) with
                | Some l, Some a -> Some (max l a)
                | Some l, None -> Some l
                | None, _ -> acc)
              None words
          in
          let words = List.filter (fun w -> zvl_length w = None) words in
          let implicit = if isa_spec = `V2_2 then in_base_2_2 else [] in
          let enable = List.map (fun f -> Target_config.Enable f) in
          Ok
            {
              xlen;
              spec =
                (Target_config.None_ :: enable unnamed) @ enable (implicit @ base @ single @ words);
              vlen;
            })
