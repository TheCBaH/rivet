(* I8080_run: load-time checks and the interrupt line. *)

let shim = { I8080_run.boot = "\xc3\x00\x01"; bdos = "\xc3\x00\xfe"; handler = "\xd3\xff\x76" }

let%expect_test "a program must not overlap the shim or run off the end of memory" =
  List.iter
    (fun (load, len) ->
      match I8080_run.run ~shim ~load ~pc:load ~max_instructions:1 (String.make len '\x76') with
      | r ->
          Printf.printf "load %04x len %d: ran %d instruction(s)\n" load len
            r.I8080_run.instructions
      | exception Invalid_argument m -> Printf.printf "load %04x len %d: %s\n" load len m)
    [
      (0x100, 16); (0x00, 4); (0x04, 4); (0xfdf0, 0x20); (0xfe00, 1); (0xfff0, 0x20); (0xfff0, 0x10);
    ];
  [%expect
    {|
    load 0100 len 16: ran 1 instruction(s)
    load 0000 len 4: I8080_run.run: the program overlaps the shim
    load 0004 len 4: I8080_run.run: the program overlaps the shim
    load fdf0 len 32: I8080_run.run: the program overlaps the shim
    load fe00 len 1: I8080_run.run: the program overlaps the shim
    load fff0 len 32: I8080_run.run: the program does not fit in 64 KiB
    load fff0 len 16: ran 1 instruction(s) |}]

let%expect_test "an interrupt held high is taken after the instruction following the last EI" =
  let shim = { shim with I8080_run.boot = "\xd3\xff\x76" } in
  List.iter
    (fun (name, program) ->
      let r = I8080_run.run ~shim ~interrupt:[ 0xc7 ] ~max_instructions:20 program in
      Printf.printf "%s:%s\n" name
        (String.concat ""
           (List.map (fun (p, v) -> Printf.sprintf " %02x %02x" p v) r.I8080_run.io_out)))
    [
      ("ei inr", "\x3e\x00\xfb\x3c\x3c\x3c\xd3\x32\xd3\xff");
      ("ei ei inr", "\x3e\x00\xfb\xfb\x3c\x3c\x3c\xd3\x32\xd3\xff");
      ("ei di inr", "\x3e\x00\xfb\xf3\x3c\x3c\x3c\xd3\x32\xd3\xff");
    ];
  [%expect {|
    ei inr: ff 01
    ei ei inr: ff 01
    ei di inr: 32 03 ff 03 |}]
