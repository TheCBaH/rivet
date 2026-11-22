module S = I8080_sem.Model

let run ?(setup = fun _ -> ()) src =
  match I8080_as.assemble src with
  | Error ds -> failwith (I8080_as.render ds)
  | Ok img ->
      let origin, bytes = I8080_as.flatten img in
      let env = { S.input = (fun _ -> 0xff); output = (fun _ _ -> ()) } in
      let st = S.create ~env (S.create_memory ()) in
      S.poke st origin bytes;
      st.S.pc <- origin;
      setup st;
      while not st.S.halted do
        S.step st
      done;
      st

let show st =
  let r = S.regs st in
  Printf.printf "a=%02x f=%02x b=%02x c=%02x h=%02x l=%02x sp=%04x pc=%04x t=%d n=%d\n"
    r.I8080_sem.ra r.flags r.rb r.rc r.rh r.rl r.rsp r.rpc st.S.tstates st.S.instructions

let%expect_test "the MVI/STA/HLT fixture: 27 T-states and one write" =
  let writes = ref [] in
  let st =
    run
      ~setup:(fun st ->
        st.S.log <- Some (function S.Write (a, v) -> writes := (a, v) :: !writes | _ -> ()))
      "\torg 0\n\tmvi a,2ah\n\tsta 2000h\n\thlt\n"
  in
  show st;
  List.iter (fun (a, v) -> Printf.printf "write %04x <- %02x\n" a v) !writes;
  [%expect
    {|
    a=2a f=02 b=00 c=00 h=00 l=00 sp=0000 pc=0006 t=27 n=3
    write 2000 <- 2a
    |}]

let%expect_test "arithmetic flags, DAA and the flag byte" =
  let st =
    run
      {|	org 0
	mvi a,99h
	adi 01h
	daa
	push psw
	pop b
	mvi a,7fh
	adi 1
	sta 100h
	mvi a,0
	sui 1
	hlt
|}
  in
  show st;
  [%expect {| a=ff f=87 b=00 c=57 h=00 l=00 sp=0000 pc=0013 t=87 n=11 |}]

let%expect_test "conditional return and call cost more when taken" =
  let st = run "\torg 0\n\tlxi sp,100h\n\tcall f\n\thlt\nf:\txra a\n\trz\n\thlt\n" in
  show st;
  [%expect {| a=00 f=46 b=00 c=00 h=00 l=00 sp=0100 pc=0007 t=49 n=5 |}]

(* {1 EI takes effect after the next instruction; DI cancels it} *)

let interrupt_after src steps =
  match I8080_as.assemble src with
  | Error ds -> failwith (I8080_as.render ds)
  | Ok img ->
      let origin, bytes = I8080_as.flatten img in
      let env = { S.input = (fun _ -> 0xff); output = (fun _ _ -> ()) } in
      let st = S.create ~env (S.create_memory ()) in
      S.poke st origin bytes;
      st.S.pc <- origin;
      st.S.sp <- 0x100;
      for _ = 1 to steps do
        S.step st
      done;
      let before = st.S.pc in
      S.interrupt st [ 0xcf ];
      Printf.printf "pc %04x -> %04x: %s\n" before st.S.pc
        (if st.S.pc = 8 then "interrupt taken" else "interrupt ignored")

let%expect_test "EI; NOP enables interrupts after the NOP" =
  interrupt_after "\torg 0\n\tei\n\tnop\n\tnop\n\thlt\n" 1;
  interrupt_after "\torg 0\n\tei\n\tnop\n\tnop\n\thlt\n" 2;
  [%expect {|
    pc 0001 -> 0001: interrupt ignored
    pc 0002 -> 0008: interrupt taken |}]

let%expect_test "EI; DI leaves interrupts disabled" =
  interrupt_after "\torg 0\n\tei\n\tdi\n\tnop\n\thlt\n" 2;
  interrupt_after "\torg 0\n\tei\n\tdi\n\tnop\n\thlt\n" 3;
  [%expect {|
    pc 0002 -> 0002: interrupt ignored
    pc 0003 -> 0003: interrupt ignored |}]

let%expect_test "EI; EI keeps the interrupt pending until the instruction after the second" =
  interrupt_after "\torg 0\n\tei\n\tei\n\tnop\n\thlt\n" 2;
  interrupt_after "\torg 0\n\tei\n\tei\n\tnop\n\thlt\n" 3;
  [%expect {|
    pc 0002 -> 0002: interrupt ignored
    pc 0003 -> 0008: interrupt taken |}]
