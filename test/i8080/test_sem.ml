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
      "\torg 0\n\
       \tmvi a,99h\n\
       \tadi 01h\n\
       \tdaa\n\
       \tpush psw\n\
       \tpop b\n\
       \tmvi a,7fh\n\
       \tadi 1\n\
       \tsta 100h\n\
       \tmvi a,0\n\
       \tsui 1\n\
       \thlt\n"
  in
  show st;
  [%expect {| a=ff f=87 b=00 c=57 h=00 l=00 sp=0000 pc=0013 t=87 n=11 |}]

let%expect_test "conditional return and call cost more when taken" =
  let st = run "\torg 0\n\tlxi sp,100h\n\tcall f\n\thlt\nf:\txra a\n\trz\n\thlt\n" in
  show st;
  [%expect {| a=00 f=46 b=00 c=00 h=00 l=00 sp=0100 pc=0007 t=49 n=5 |}]
