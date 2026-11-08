(* The Intel 8080's behaviour, once, over the shared value and memory
   signatures (rivet.compute): registers, the five flags and what each of the
   256 opcode bytes does to them, to memory and to the environment.

   The model is a functor so that a symbolic interpretation can reuse the
   semantics; {!Model} is the concrete one. Lengths, no-wait T-states and the
   effects are the descriptor table's ({!I8080_ops}): the same entries run here
   over any value interpretation, and are checked against third-party
   assemblers and emulators (docs/i8080-oracle.md), not against a second table.

   Profile (Radio-86RK contracts): 8080A, not 8085 or Z80. The twelve alias
   bytes do what their documented twin does. The flag byte is
   [S Z 0 AC 0 P 1 CY]. [ANA]/[ANI] set AC from bit 3 of the OR of the operands.
   [EI] takes effect after the next instruction. An interrupt-acknowledge fetch
   does not advance PC. *)

module Desc = I8080_ops

exception Not_concrete

type regs = {
  ra : int;
  rb : int;
  rc : int;
  rd : int;
  re : int;
  rh : int;
  rl : int;
  flags : int;
  rsp : int;
  rpc : int;
}

module Make (V : Compute.VALUE) (M : Compute.MEMORY with type value = V.t) = struct
  module O = Compute.Ops (V)

  type value = V.t

  type access =
    | Fetch of value * value
    | Read of value * value
    | Write of value * value
    | In of value * value
    | Out of value * value

  type env = { input : value -> value; output : value -> value -> unit }

  type t = {
    mutable a : value;
    mutable b : value;
    mutable c : value;
    mutable d : value;
    mutable e : value;
    mutable h : value;
    mutable l : value;
    mutable s : value;
    mutable z : value;
    mutable ac : value;
    mutable p : value;
    mutable cy : value;
    mutable sp : value;
    mutable pc : value;
    mutable inte : bool;
    mutable ei_pending : bool;
    mutable halted : bool;
    mutable mem : M.t;
    mutable tstates : int;
    mutable instructions : int;
    mutable taken : bool;
    seen : Bytes.t;  (** [seen.[op]] is set once opcode [op] has been executed *)
    mutable inta : int list;
    mutable log : (access -> unit) option;
    env : env;
  }

  let c8 = V.const ~w:8
  let c16 = V.const ~w:16

  let create ~env mem =
    let z8 = c8 0 and z1 = O.zero1 in
    {
      a = z8;
      b = z8;
      c = z8;
      d = z8;
      e = z8;
      h = z8;
      l = z8;
      s = z1;
      z = z1;
      ac = z1;
      p = z1;
      cy = z1;
      sp = c16 0;
      pc = c16 0;
      inte = false;
      ei_pending = false;
      halted = false;
      mem;
      tstates = 0;
      instructions = 0;
      taken = false;
      seen = Bytes.make 256 '\000';
      inta = [];
      log = None;
      env;
    }

  let note st f = match st.log with None -> () | Some k -> k (f ())

  let read st addr =
    let v = M.load8 st.mem addr in
    (match st.log with None -> () | Some k -> k (Read (addr, v)));
    v

  let write st addr v =
    st.mem <- M.store8 st.mem addr v;
    match st.log with None -> () | Some k -> k (Write (addr, v))

  let fetch st =
    match st.inta with
    | b :: rest ->
        st.inta <- rest;
        c8 b
    | [] ->
        let addr = st.pc in
        let v = M.load8 st.mem addr in
        (match st.log with None -> () | Some k -> k (Fetch (addr, v)));
        st.pc <- V.add ~w:16 addr (c16 1);
        v

  (* The primitives the table's entries are written against. *)
  module Machine : Desc.MACHINE with module V = V and type t = t = struct
    module V = V

    type nonrec t = t

    let reg st = function
      | 0 -> st.b
      | 1 -> st.c
      | 2 -> st.d
      | 3 -> st.e
      | 4 -> st.h
      | 5 -> st.l
      | 7 -> st.a
      | i -> invalid_arg (Printf.sprintf "I8080_sem: no register %d" i)

    let set_reg st i v =
      match i with
      | 0 -> st.b <- v
      | 1 -> st.c <- v
      | 2 -> st.d <- v
      | 3 -> st.e <- v
      | 4 -> st.h <- v
      | 5 -> st.l <- v
      | 7 -> st.a <- v
      | i -> invalid_arg (Printf.sprintf "I8080_sem: no register %d" i)

    let sp st = st.sp
    let set_sp st v = st.sp <- v
    let pc st = st.pc
    let set_pc st v = st.pc <- v

    let flag st : Desc.flag -> value = function
      | `S -> st.s
      | `Z -> st.z
      | `Ac -> st.ac
      | `P -> st.p
      | `Cy -> st.cy

    let set_flag st (f : Desc.flag) v =
      match f with
      | `S -> st.s <- v
      | `Z -> st.z <- v
      | `Ac -> st.ac <- v
      | `P -> st.p <- v
      | `Cy -> st.cy <- v

    let read = read
    let write = write
    let fetch8 = fetch

    let input st port =
      let v = st.env.input port in
      note st (fun () -> In (port, v));
      v

    let output st port v =
      note st (fun () -> Out (port, v));
      st.env.output port v

    let taken st = st.taken <- true
    let halt st = st.halted <- true
    let ei st = st.ei_pending <- true

    let di st =
      st.inte <- false;
      st.ei_pending <- false
  end

  module Lib = Desc.Lib (Machine)

  (* One closure per opcode byte, made from the table's entries; an alias runs
     its documented twin's. *)
  let program : (t -> unit) array =
    let machine = (module Machine : Desc.MACHINE with type t = t) in
    let rec compile b =
      match Desc.form_of_byte b with
      | Some (Desc.Documented (e, fields)) -> e.Desc.sem.Desc.run machine fields
      | Some (Desc.Alias twin) -> compile twin
      | None -> invalid_arg (Printf.sprintf "I8080_sem: byte %02x has no form" b)
    in
    Array.init 256 compile

  let to_int v = match V.to_int v with Some n -> n | None -> raise Not_concrete

  (* One instruction (or one interrupt-supplied instruction when [inta] is
     set), accounted in [tstates]. *)
  let step st =
    let was_pending = st.ei_pending in
    st.ei_pending <- false;
    st.taken <- false;
    let op = to_int (fetch st) in
    Bytes.unsafe_set st.seen op '\001';
    (Array.unsafe_get program op) st;
    let t = Array.unsafe_get Desc.timings op in
    st.tstates <- (st.tstates + if st.taken then t.Desc.taken else t.Desc.tstates);
    st.instructions <- st.instructions + 1;
    if was_pending && not st.ei_pending then st.inte <- true

  (* Take an interrupt: the supplied bytes are the instruction (an RST, or a
     CALL with its two address bytes). Ignored while interrupts are disabled. *)
  let interrupt st bytes =
    if st.inte then (
      st.inte <- false;
      st.halted <- false;
      st.inta <- bytes;
      step st;
      st.inta <- [])

  let snapshot_note = note
end

(* {1 The concrete model} *)

module Concrete_of (V : Compute.VALUE with type t = int) = struct
  include Make (V) (Compute.Bytes_memory)

  let regs st =
    {
      ra = st.a;
      rb = st.b;
      rc = st.c;
      rd = st.d;
      re = st.e;
      rh = st.h;
      rl = st.l;
      flags = Lib.flags_byte st;
      rsp = st.sp;
      rpc = st.pc;
    }

  let create_memory () = Bytes.make 0x10000 '\000'
  let poke st addr s = Bytes.blit_string s 0 st.mem (addr land 0xffff) (String.length s)
end

module Model = Concrete_of (Compute.Concrete)

(* {1 What a runner needs}

   The runner is written against this signature so that a model built on a
   modified value interpretation (a deliberately planted bug, in the tests of
   the differential runs) can be run by the same code. *)

type event =
  | Ev_fetch of int * int
  | Ev_read of int * int
  | Ev_write of int * int
  | Ev_in of int * int
  | Ev_out of int * int

module type RUNNABLE = sig
  type t

  val create : input:(int -> int) -> output:(int -> int -> unit) -> t
  val poke : t -> int -> string -> unit
  val start : t -> pc:int -> sp:int -> unit
  val trace : t -> (event -> unit) option -> unit
  val step : t -> unit
  val halted : t -> bool
  val instructions : t -> int
  val tstates : t -> int
  val regs : t -> regs
  val memory : t -> Bytes.t
  val seen : t -> Bytes.t
end

module Runnable (V : Compute.VALUE with type t = int) : RUNNABLE = struct
  module C = Concrete_of (V)

  type t = C.t

  let create ~input ~output = C.create ~env:{ C.input; output } (C.create_memory ())
  let poke = C.poke

  let start st ~pc ~sp =
    st.C.pc <- pc;
    st.C.sp <- sp

  let trace st f =
    st.C.log <-
      Option.map
        (fun f -> function
          | C.Fetch (a, v) -> f (Ev_fetch (a, v))
          | Read (a, v) -> f (Ev_read (a, v))
          | Write (a, v) -> f (Ev_write (a, v))
          | In (a, v) -> f (Ev_in (a, v))
          | Out (a, v) -> f (Ev_out (a, v)))
        f

  let step = C.step
  let halted st = st.C.halted
  let instructions st = st.C.instructions
  let tstates st = st.C.tstates
  let regs = C.regs
  let memory st = st.C.mem
  let seen st = st.C.seen
end
