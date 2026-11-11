(* Run a CP/M-style program in the concrete model under the K5 shim contract:
   the program is loaded at 0x0100, a BDOS entry at 0x0005 jumps to a handler,
   console output is [OUT 0x01], [OUT 0xFF] ends the run and [OUT 0xFE] reports
   an unsupported BDOS function. The shim itself is machine code in memory (the
   three byte strings of the retrocpu release), executed by the model like any
   other program. *)

type shim = { boot : string; bdos : string; handler : string }
type status = Exited | Halted | Fault of string | Limit

type result = {
  console : string;
  io_out : (int * int) list;
  status : status;
  instructions : int;
  tstates : int;
  regs : I8080_sem.regs;
  memory : Bytes.t;
  seen : Bytes.t;
}

module Make (S : I8080_sem.RUNNABLE) = struct
  (* [interrupt]: the bytes an interrupt-acknowledge cycle supplies, with INT held high
     from the start; it is taken at the first instruction boundary where interrupts are on. *)
  let run ?(max_instructions = max_int) ?(input = fun _ -> 0xff) ?(load = 0x100) ?(pc = 0x100)
      ?(sp = 0xfe00) ?(interrupt : int list option) ?(trace : (I8080_sem.event -> unit) option)
      ?(before : (I8080_sem.regs -> unit) option) ~shim program =
    let overlaps (at, len) =
      load < at + len && at < load + String.length program && String.length program > 0
    in
    if load < 0 || load + String.length program > 0x10000 then
      invalid_arg "I8080_run.run: the program does not fit in 64 KiB";
    if
      List.exists overlaps
        [
          (0, String.length shim.boot);
          (5, String.length shim.bdos);
          (0xfe00, String.length shim.handler);
        ]
    then invalid_arg "I8080_run.run: the program overlaps the shim";
    let console = Buffer.create 256 and io_out = ref [] and stop = ref false and fault = ref None in
    let output port v =
      io_out := (port, v) :: !io_out;
      match port with
      | 0x01 -> Buffer.add_char console (Char.chr v)
      | 0xff -> stop := true
      | 0xfe -> fault := Some "unsupported BDOS function"
      | _ -> ()
    in
    let st = S.create ~input:(fun port -> input port land 0xff) ~output in
    S.poke st 0 shim.boot;
    S.poke st 5 shim.bdos;
    S.poke st 0xfe00 shim.handler;
    S.poke st load program;
    S.start st ~pc ~sp;
    S.trace st trace;
    while
      (not !stop) && !fault = None && (not (S.halted st)) && S.instructions st < max_instructions
    do
      (match before with None -> () | Some f -> f (S.regs st));
      let before_n = S.instructions st in
      Option.iter (S.interrupt st) interrupt;
      if S.instructions st = before_n then S.step st
    done;
    let status =
      match !fault with
      | Some m -> Fault m
      | None -> if !stop then Exited else if S.halted st then Halted else Limit
    in
    {
      console = Buffer.contents console;
      io_out = List.rev !io_out;
      status;
      instructions = S.instructions st;
      tstates = S.tstates st;
      regs = S.regs st;
      memory = S.memory st;
      seen = S.seen st;
    }
end

include Make (I8080_sem.Runnable (Compute.Concrete))
