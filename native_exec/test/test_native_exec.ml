(* Native_exec on hand-written aarch64 assembly: no CompCert involved. *)

open Asm_core

let assemble text =
  let source = Foundation.Span.source ~name:"hand.s" ~contents:text in
  match Driver.Registry.Aarch64.assemble ~entry:"entry" ~unit_name:"hand" ~source () with
  | Ok l -> l
  | Error e -> failwith (Foundation.Diag.render e)

let io_of_string ?(size = 64) s =
  let io = Bigarray.Array1.create Bigarray.char Bigarray.c_layout size in
  Bigarray.Array1.fill io '\000';
  String.iteri (fun i c -> io.{i} <- c) s;
  io

let run ?observe ?read_globals ?(io = io_of_string "") laid =
  match Native_exec.run ?observe ?read_globals laid ~io with
  | Ok o -> o
  | Error e -> failwith (Format.asprintf "%a" Native_exec.pp_error e)

let return42 = assemble "\t.text\n\t.globl entry\nentry:\n\tmovz\tx0, #42\n\tret\n"

let%expect_test "mov x0, #42; ret returns 42" =
  Printf.printf "%Ld\n" (run return42).value;
  [%expect {| 42 |}]

(* Reads a u64 from io[0], stores it plus one at io[8], and copies io[16] to
   io[17]; returns the incremented value. *)
let roundtrip =
  assemble
    "\t.text\n\
     \t.globl entry\n\
     entry:\n\
     \tldr\tx1, [x0]\n\
     \tadd\tx1, x1, #1\n\
     \tstr\tx1, [x0, #8]\n\
     \tldrb\tw2, [x0, #16]\n\
     \tstrb\tw2, [x0, #17]\n\
     \tadd\tx0, x1, #0\n\
     \tret\n"

let%expect_test "entry reads and writes the io buffer" =
  let io = io_of_string "\x10\x00\x00\x00\x00\x00\x00\x00" in
  io.{16} <- 'Z';
  let o = run ~io roundtrip in
  let u64_at k =
    let r = ref 0L in
    for i = 7 downto 0 do
      r := Int64.logor (Int64.shift_left !r 8) (Int64.of_int (Char.code io.{k + i}))
    done;
    !r
  in
  Printf.printf "value %Ld, io[8] %Ld, io[17] %c\n" o.value (u64_at 8) io.{17};
  [%expect {| value 17, io[8] 17, io[17] Z |}]

(* Code, read-only data, writable data and BSS in one image; entry sums a
   .rodata word, a .data word and a .bss word it first writes. *)
let segments =
  assemble
    "\t.text\n\
     \t.globl entry\n\
     entry:\n\
     \tadrp\tx1, ro\n\
     \tadd\tx1, x1, :lo12:ro\n\
     \tldr\tw2, [x1]\n\
     \tadrp\tx1, counter\n\
     \tadd\tx1, x1, :lo12:counter\n\
     \tldr\tw3, [x1]\n\
     \tadd\tw3, w3, #1\n\
     \tstr\tw3, [x1]\n\
     \tadrp\tx1, zeroed\n\
     \tadd\tx1, x1, :lo12:zeroed\n\
     \tldr\tw4, [x1]\n\
     \tmovz\tw5, #100\n\
     \tstr\tw5, [x1]\n\
     \tadd\tw0, w2, w3\n\
     \tadd\tw0, w0, w4\n\
     \tret\n\
     \t.section\t.rodata\n\
     \t.balign 4\n\
     ro:\n\
     \t.word\t1000\n\
     \t.data\n\
     \t.balign 4\n\
     \t.globl counter\n\
     counter:\n\
     \t.word\t7\n\
     \t.type counter, @object\n\
     \t.size counter, 4\n\
     \t.bss\n\
     \t.balign 4\n\
     \t.globl zeroed\n\
     zeroed:\n\
     \t.space\t4\n\
     \t.type zeroed, @object\n\
     \t.size zeroed, 4\n"

let%expect_test "rodata, data and bss are placed, bound and readable back" =
  let o = run ~read_globals:[ "counter"; "zeroed" ] segments in
  Printf.printf "value %Ld\n" o.value;
  List.iter
    (fun (n, b) ->
      Printf.printf "%s:" n;
      String.iter (fun c -> Printf.printf " %02x" (Char.code c)) b;
      print_newline ())
    o.globals;
  [%expect {|
    value 1008
    counter: 08 00 00 00
    zeroed: 64 00 00 00 |}]

let maps () =
  In_channel.with_open_bin "/proc/self/maps" In_channel.input_all |> String.split_on_char '\n'

(* The permissions of every /proc/self/maps entry overlapping [lo, hi). An
   entry can extend past the range: the kernel merges adjacent anonymous
   mappings that have the same permissions. *)
let pages_within ~lo ~hi =
  List.filter_map
    (fun l ->
      match String.split_on_char ' ' l with
      | range :: perms :: _ -> (
          match String.split_on_char '-' range with
          | [ a; b ] ->
              let a = Int64.of_string ("0x" ^ a) and b = Int64.of_string ("0x" ^ b) in
              if Int64.unsigned_compare a hi < 0 && Int64.unsigned_compare b lo > 0 then Some perms
              else None
          | _ -> None)
      | _ -> None)
    (maps ())

let%expect_test "no page is ever writable and executable" =
  let events = ref [] in
  let at_call = ref [] in
  let range = ref (0L, 0L) in
  let observe e =
    events := e :: !events;
    match e with
    | Native_exec.Mapped { base; size } -> range := (base, Int64.add base (Int64.of_int size))
    | Calling _ ->
        let lo, hi = !range in
        at_call := pages_within ~lo ~hi
    | _ -> ()
  in
  ignore (run ~observe segments);
  List.iter
    (function
      | Native_exec.Protected { segment; perms; _ } ->
          Printf.printf "protect %s %s\n" segment (Perms.to_string perms)
      | _ -> ())
    (List.rev !events);
  Printf.printf "maps at call: %s\n" (String.concat " " !at_call);
  Printf.printf "any w+x: %b\n"
    (List.exists
       (function Native_exec.Protected { perms; _ } -> perms.write && perms.execute | _ -> false)
       !events
    || List.exists (fun p -> String.length p >= 3 && p.[1] = 'w' && p.[2] = 'x') !at_call);
  [%expect
    {|
    protect .text r-x
    protect .rodata r--
    protect .data rw-
    protect .bss rw-
    maps at call: r-xp r--p rw-p
    any w+x: false |}]

let%expect_test "the mapping is released when the call path raises" =
  let before = List.length (maps ()) in
  let observe = function Native_exec.Calling _ -> raise Exit | _ -> () in
  (match Native_exec.run ~observe return42 ~io:(io_of_string "") with
  | _ -> print_endline "no exception?"
  | exception Exit -> print_endline "raised");
  Printf.printf "maps unchanged: %b\n" (List.length (maps ()) = before);
  [%expect {|
    raised
    maps unchanged: true |}]

let%expect_test "10,000 map/run/unmap cycles leave the mapping count unchanged" =
  let io = io_of_string "" in
  ignore (run ~io segments);
  let before = List.length (maps ()) in
  let bad = ref 0 in
  for _ = 1 to 10_000 do
    if (run ~io segments).value <> 1008L then incr bad
  done;
  let after = List.length (maps ()) in
  Printf.printf "wrong results: %d, maps before %s after\n" !bad
    (if before = after then "=" else Printf.sprintf "%d <> %d" before after);
  [%expect {| wrong results: 0, maps before = after |}]

let%expect_test "an image for another target is refused before anything is mapped" =
  (* arm is never a host ISA: AArch32 code cannot run in a 64-bit process. *)
  let events = ref 0 in
  (match
     Native_exec.run ~target:"arm" ~observe:(fun _ -> incr events) return42 ~io:(io_of_string "")
   with
  | Ok _ -> print_endline "unexpectedly Ok"
  | Error (Native_exec.Foreign_target { target; _ }) ->
      Printf.printf "refused %s, events %d\n" target !events
  | Error e -> Format.printf "other error: %a\n" Native_exec.pp_error e);
  [%expect {| refused arm, events 0 |}]

let%expect_test "the host's own target name is accepted" =
  match Native_exec.host_isa with
  | None -> print_endline "no host ISA"
  | Some isa ->
      Printf.printf "%Ld\n"
        (Result.get_ok (Native_exec.run ~target:isa return42 ~io:(io_of_string ""))).value;
      [%expect {| 42 |}]
