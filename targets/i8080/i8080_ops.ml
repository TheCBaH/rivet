(* The Intel 8080 descriptor table: every documented instruction family, the
   twelve undocumented aliases, and the byte -> form map derived from them.

   The encoder's codec, the operand legality checks, the decoder, the canonical
   printer and the behavioural model all read this table; nothing else lists an
   opcode. An entry is an opcode byte with its register fields zeroed ([base]),
   the operands the source spells, where each register-valued operand sits in
   the byte, its no-wait T-states and what it does ([sem]). *)

(* {1 What an instruction does}

   An instruction's effect is written once, against the primitives of a
   {!MACHINE}, and stored in its table entry. The value interpretation is the
   machine's [V]: instantiated with {!Compute.Concrete} the effect runs, with
   {!Compute.Term} it builds expressions. [run machine fields] is applied once
   per opcode byte when a model is created and returns the closure executed on
   every step, so operand decoding costs nothing per instruction. *)

type flag = [ `S | `Z | `Ac | `P | `Cy ]
type cond = [ `Nz | `Z | `Nc | `C | `Po | `Pe | `P | `M ]
type arith = Add | Adc | Sub | Sbb | Ana | Xra | Ora | Cmp

module type MACHINE = sig
  module V : Compute.VALUE

  type t

  val reg : t -> int -> V.t
  (** B C D E H L A as field values 0-5 and 7; 6 is not a register. *)

  val set_reg : t -> int -> V.t -> unit
  val sp : t -> V.t
  val set_sp : t -> V.t -> unit
  val pc : t -> V.t
  val set_pc : t -> V.t -> unit
  val flag : t -> flag -> V.t
  val set_flag : t -> flag -> V.t -> unit
  val read : t -> V.t -> V.t
  val write : t -> V.t -> V.t -> unit

  val fetch8 : t -> V.t
  (** The byte at PC, advancing it (or the next interrupt-supplied byte). *)

  val input : t -> V.t -> V.t
  val output : t -> V.t -> V.t -> unit

  val taken : t -> unit
  (** The conditional call or return was taken: it costs its [t_taken]. *)

  val halt : t -> unit

  val ei : t -> unit
  (** Interrupts are enabled after the next instruction. *)

  val di : t -> unit
end

(* Derived operations shared by the entries below. *)
module Lib (M : MACHINE) = struct
  module V = M.V
  module O = Compute.Ops (V)

  let c8 = V.const ~w:8
  let c16 = V.const ~w:16
  let hi8 v = V.extract v ~hi:15 ~lo:8
  let lo8 v = V.extract v ~hi:7 ~lo:0
  let inc16 v = V.add ~w:16 v (c16 1)
  let dec16 v = V.sub ~w:16 v (c16 1)
  let pair hi lo = V.concat ~hi ~lo ~lo_w:8
  let a t = M.reg t 7
  let set_a t v = M.set_reg t 7 v
  let hl t = pair (M.reg t 4) (M.reg t 5)
  let de t = pair (M.reg t 2) (M.reg t 3)

  let set_pair t hi lo v =
    M.set_reg t hi (hi8 v);
    M.set_reg t lo (lo8 v)

  let set_hl t v = set_pair t 4 5 v
  let set_de t v = set_pair t 2 3 v

  (* Field 6 of a register operand is the memory byte HL addresses. *)
  let r8 t i = if i = 6 then M.read t (hl t) else M.reg t i
  let set_r8 t i v = if i = 6 then M.write t (hl t) v else M.set_reg t i v
  let rp t = function 0 -> pair (M.reg t 0) (M.reg t 1) | 1 -> de t | 2 -> hl t | _ -> M.sp t

  let set_rp t i v =
    match i with 0 -> set_pair t 0 1 v | 1 -> set_de t v | 2 -> set_hl t v | _ -> M.set_sp t v

  let fetch16 t =
    let lo = M.fetch8 t in
    let hi = M.fetch8 t in
    pair hi lo

  let push t v =
    M.set_sp t (dec16 (M.sp t));
    M.write t (M.sp t) (hi8 v);
    M.set_sp t (dec16 (M.sp t));
    M.write t (M.sp t) (lo8 v)

  let pop t =
    let lo = M.read t (M.sp t) in
    M.set_sp t (inc16 (M.sp t));
    let hi = M.read t (M.sp t) in
    M.set_sp t (inc16 (M.sp t));
    pair hi lo

  (* The flag byte: S Z 0 AC 0 P 1 CY. *)
  let flags_byte t =
    let bit v n = V.shl ~w:8 (V.zext ~w:8 v) n in
    List.fold_left V.logor (c8 0x02)
      [
        bit (M.flag t `S) 7;
        bit (M.flag t `Z) 6;
        bit (M.flag t `Ac) 4;
        bit (M.flag t `P) 2;
        bit (M.flag t `Cy) 0;
      ]

  let set_flags_byte t f =
    M.set_flag t `S (O.bit f 7);
    M.set_flag t `Z (O.bit f 6);
    M.set_flag t `Ac (O.bit f 4);
    M.set_flag t `P (O.bit f 2);
    M.set_flag t `Cy (O.bit f 0)

  let szp t r =
    M.set_flag t `S (O.bit r 7);
    M.set_flag t `Z (O.is_zero ~w:8 r);
    M.set_flag t `P (O.parity_even ~w:8 r)

  (* 8-bit [a + b + cin] and its carry out. *)
  let add8 a b cin =
    let w9 = V.zext ~w:9 in
    let s = V.add ~w:9 (V.add ~w:9 (w9 a) (w9 b)) (w9 cin) in
    (V.extract s ~hi:7 ~lo:0, O.bit s 8)

  (* The carry into bit 4 of [a + b = r]. *)
  let aux a b r = O.bit (V.logxor (V.logxor a b) r) 4

  let alu t op x =
    let acc = a t in
    match op with
    | Add | Adc ->
        let r, c = add8 acc x (if op = Adc then M.flag t `Cy else O.zero1) in
        M.set_flag t `Ac (aux acc x r);
        M.set_flag t `Cy c;
        szp t r;
        set_a t r
    | Sub | Sbb | Cmp ->
        (* a - x - borrow, as the 8080 does it: a + ~x + not borrow *)
        let nx = V.lognot ~w:8 x in
        let cin = if op = Sbb then O.not1 (M.flag t `Cy) else O.one in
        let r, c = add8 acc nx cin in
        M.set_flag t `Ac (aux acc nx r);
        M.set_flag t `Cy (O.not1 c);
        szp t r;
        if op <> Cmp then set_a t r
    | Ana ->
        let r = V.logand acc x in
        M.set_flag t `Ac (O.bit (V.logor acc x) 3);
        M.set_flag t `Cy O.zero1;
        szp t r;
        set_a t r
    | Xra | Ora ->
        let r = if op = Xra then V.logxor acc x else V.logor acc x in
        M.set_flag t `Ac O.zero1;
        M.set_flag t `Cy O.zero1;
        szp t r;
        set_a t r

  let condition t (c : cond) =
    let flag, set =
      match c with
      | `Nz -> (`Z, false)
      | `Z -> (`Z, true)
      | `Nc -> (`Cy, false)
      | `C -> (`Cy, true)
      | `Po -> (`P, false)
      | `Pe -> (`P, true)
      | `P -> (`S, false)
      | `M -> (`S, true)
    in
    V.decide (if set then M.flag t flag else O.not1 (M.flag t flag))
end

type sem = { run : 'm. (module MACHINE with type t = 'm) -> int list -> 'm -> unit }

let bad name = invalid_arg ("i8080_ops: " ^ name ^ ": wrong operand fields")
let s_nop = { run = (fun (type m) (module _ : MACHINE with type t = m) _ (_ : m) -> ()) }
let s_hlt = { run = (fun (type m) (module M : MACHINE with type t = m) _ t -> M.halt t) }

let s_mov =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with [ d; s ] -> fun t -> L.set_r8 t d (L.r8 t s) | _ -> bad "mov");
  }

let s_mvi =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with [ d ] -> fun t -> L.set_r8 t d (M.fetch8 t) | _ -> bad "mvi");
  }

let s_lxi =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with [ i ] -> fun t -> L.set_rp t i (L.fetch16 t) | _ -> bad "lxi");
  }

let s_ldax =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with [ i ] -> fun t -> L.set_a t (M.read t (L.rp t i)) | _ -> bad "ldax");
  }

let s_stax =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with [ i ] -> fun t -> M.write t (L.rp t i) (L.a t) | _ -> bad "stax");
  }

let s_sta =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t -> M.write t (L.fetch16 t) (L.a t));
  }

let s_lda =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t -> L.set_a t (M.read t (L.fetch16 t)));
  }

let s_shld =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t ->
          let addr = L.fetch16 t in
          M.write t addr (M.reg t 5);
          M.write t (L.inc16 addr) (M.reg t 4));
  }

let s_lhld =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t ->
          let addr = L.fetch16 t in
          M.set_reg t 5 (M.read t addr);
          M.set_reg t 4 (M.read t (L.inc16 addr)));
  }

let s_inx =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with
        | [ i ] -> fun t -> L.set_rp t i (M.V.add ~w:16 (L.rp t i) (L.c16 1))
        | _ -> bad "inx");
  }

let s_dcx =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with
        | [ i ] -> fun t -> L.set_rp t i (M.V.sub ~w:16 (L.rp t i) (L.c16 1))
        | _ -> bad "dcx");
  }

let s_inr =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with
        | [ d ] ->
            fun t ->
              let v = L.r8 t d in
              let one = L.c8 1 in
              let r = M.V.add ~w:8 v one in
              M.set_flag t `Ac (L.aux v one r);
              L.szp t r;
              L.set_r8 t d r
        | _ -> bad "inr");
  }

let s_dcr =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with
        | [ d ] ->
            fun t ->
              let v = L.r8 t d in
              let m1 = L.c8 0xff in
              let r = M.V.add ~w:8 v m1 in
              M.set_flag t `Ac (L.aux v m1 r);
              L.szp t r;
              L.set_r8 t d r
        | _ -> bad "dcr");
  }

let s_dad =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with
        | [ i ] ->
            fun t ->
              let w17 = M.V.zext ~w:17 in
              let s = M.V.add ~w:17 (w17 (L.hl t)) (w17 (L.rp t i)) in
              M.set_flag t `Cy (L.O.bit s 16);
              L.set_hl t (M.V.extract s ~hi:15 ~lo:0)
        | _ -> bad "dad");
  }

let s_alu op =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with [ s ] -> fun t -> L.alu t op (L.r8 t s) | _ -> bad "alu");
  }

let s_alu_imm op =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t -> L.alu t op (M.fetch8 t));
  }

(* Decimal adjust: add 6 to the low digit when it is over 9 or AC is set, then
   6 to the high digit when it is over 9 or CY is set. *)
let s_daa =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        let module V = M.V in
        fun t ->
          let acc = L.a t in
          let lo = V.extract acc ~hi:3 ~lo:0 in
          let need6 = V.logor (V.ult (V.const ~w:4 9) lo) (M.flag t `Ac) in
          let w9 = V.zext ~w:9 in
          let t1 = V.add ~w:9 (w9 acc) (V.ite need6 (V.const ~w:9 6) (V.const ~w:9 0)) in
          let r1 = V.extract t1 ~hi:7 ~lo:0 in
          let ac = V.ite need6 (L.aux acc (L.c8 6) r1) L.O.zero1 in
          (* the high digit of a + 6, with its carry: 10 or more means adjust *)
          let hi = V.extract t1 ~hi:8 ~lo:4 in
          let need60 = V.logor (V.ult (V.const ~w:5 9) hi) (M.flag t `Cy) in
          let r = V.add ~w:8 r1 (V.ite need60 (L.c8 0x60) (L.c8 0)) in
          M.set_flag t `Cy need60;
          M.set_flag t `Ac ac;
          L.szp t r;
          L.set_a t r);
  }

let s_cma =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t -> L.set_a t (M.V.lognot ~w:8 (L.a t)));
  }

let s_stc =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t -> M.set_flag t `Cy L.O.one);
  }

let s_cmc =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t -> M.set_flag t `Cy (L.O.not1 (M.flag t `Cy)));
  }

(* The four rotates: [left] or right, through the carry or not. *)
let s_rotate ~left ~through =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        let module V = M.V in
        fun t ->
          let acc = L.a t in
          let cy = M.flag t `Cy in
          let out = L.O.bit acc (if left then 7 else 0) in
          M.set_flag t `Cy out;
          let incoming = if through then cy else out in
          L.set_a t
            (if left then V.logor (V.shl ~w:8 acc 1) (V.zext ~w:8 incoming)
             else V.logor (V.shr acc 1) (V.shl ~w:8 (V.zext ~w:8 incoming) 7)));
  }

let s_jmp =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t -> M.set_pc t (L.fetch16 t));
  }

let s_jcc c =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t ->
          let target = L.fetch16 t in
          if L.condition t c then M.set_pc t target);
  }

let s_call =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t ->
          let target = L.fetch16 t in
          L.push t (M.pc t);
          M.set_pc t target);
  }

let s_ccc c =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t ->
          let target = L.fetch16 t in
          if L.condition t c then (
            M.taken t;
            L.push t (M.pc t);
            M.set_pc t target));
  }

let s_ret =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t -> M.set_pc t (L.pop t));
  }

let s_rcc c =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t ->
          if L.condition t c then (
            M.taken t;
            M.set_pc t (L.pop t)));
  }

let s_rst =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with
        | [ n ] ->
            fun t ->
              L.push t (M.pc t);
              M.set_pc t (L.c16 (n * 8))
        | _ -> bad "rst");
  }

let s_push =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with
        | [ 3 ] -> fun t -> L.push t (L.pair (L.a t) (L.flags_byte t))
        | [ i ] -> fun t -> L.push t (L.rp t i)
        | _ -> bad "push");
  }

let s_pop =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) args ->
        let module L = Lib (M) in
        match args with
        | [ 3 ] ->
            fun t ->
              let v = L.pop t in
              L.set_flags_byte t (L.lo8 v);
              L.set_a t (L.hi8 v)
        | [ i ] -> fun t -> L.set_rp t i (L.pop t)
        | _ -> bad "pop");
  }

let s_pchl =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t -> M.set_pc t (L.hl t));
  }

let s_sphl =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t -> M.set_sp t (L.hl t));
  }

let s_xthl =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t ->
          let lo = M.read t (M.sp t) in
          let hi = M.read t (L.inc16 (M.sp t)) in
          M.write t (M.sp t) (M.reg t 5);
          M.write t (L.inc16 (M.sp t)) (M.reg t 4);
          M.set_reg t 5 lo;
          M.set_reg t 4 hi);
  }

let s_xchg =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t ->
          let d = L.de t and h = L.hl t in
          L.set_de t h;
          L.set_hl t d);
  }

let s_in =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t ->
          let port = M.fetch8 t in
          L.set_a t (M.input t port));
  }

let s_out =
  {
    run =
      (fun (type m) (module M : MACHINE with type t = m) _ ->
        let module L = Lib (M) in
        fun t ->
          let port = M.fetch8 t in
          M.output t port (L.a t));
  }

let s_ei = { run = (fun (type m) (module M : MACHINE with type t = m) _ t -> M.ei t) }
let s_di = { run = (fun (type m) (module M : MACHINE with type t = m) _ t -> M.di t) }

(* What a source operand is. [Rp] is a register pair as BC/DE/HL/SP, [Rp_psw]
   as BC/DE/HL/PSW (push, pop), [Rp_bd] only BC or DE (ldax, stax). *)
type operand = R8 | Rp | Rp_psw | Rp_bd | Rst | D8 | P8 | D16 | A16
type tail = Tail_none | Tail_8 | Tail_16

type entry = {
  mnemonic : string;
  base : int;
  operands : operand list;
  fields : (int * int) list;
      (** [(shift, width)] of each field-valued operand ([R8], [Rp], [Rp_psw], [Rp_bd], [Rst]), in
          operand order *)
  t_base : int;  (** 8080A no-wait T-states; for a conditional call or return, not taken *)
  t_mem : int;  (** the same when an [R8] field is [m] (memory through HL) *)
  t_taken : int;  (** the same when a conditional call or return is taken *)
  sem : sem;  (** the effect, over the field values in operand order *)
}

let in_field = function R8 | Rp | Rp_psw | Rp_bd | Rst -> true | D8 | P8 | D16 | A16 -> false

let tail_of (e : entry) =
  match List.rev e.operands with
  | (D8 | P8) :: _ -> Tail_8
  | (D16 | A16) :: _ -> Tail_16
  | _ -> Tail_none

let length e = 1 + match tail_of e with Tail_none -> 0 | Tail_8 -> 1 | Tail_16 -> 2

(* How many values a field-valued operand can take. *)
let cardinality = function
  | R8 | Rst -> 8
  | Rp | Rp_psw -> 4
  | Rp_bd -> 2
  | D8 | P8 | D16 | A16 -> 1

let e1 ?m ?taken mnemonic base operands fields t sem =
  {
    mnemonic;
    base;
    operands;
    fields;
    t_base = t;
    t_mem = Option.value m ~default:t;
    t_taken = Option.value taken ~default:t;
    sem;
  }

let e0 ?taken mnemonic base t sem = e1 ?taken mnemonic base [] [] t sem
let alu mnemonic base op = e1 ~m:7 mnemonic base [ R8 ] [ (0, 3) ] 4 (s_alu op)
let imm8 mnemonic base op = e1 mnemonic base [ D8 ] [] 7 (s_alu_imm op)
let addr ?taken mnemonic base t sem = e1 ?taken mnemonic base [ A16 ] [] t sem

let entries : entry list =
  [ e0 "nop" 0x00 4 s_nop; e0 "hlt" 0x76 7 s_hlt ]
  @ [
      (* 0x76 is [mov m,m]'s slot; the table lists [hlt] first and the
         enumeration below skips the second claim. *)
      e1 ~m:7 "mov" 0x40 [ R8; R8 ] [ (3, 3); (0, 3) ] 5 s_mov;
      e1 ~m:10 "mvi" 0x06 [ R8; D8 ] [ (3, 3) ] 7 s_mvi;
      e1 ~m:10 "inr" 0x04 [ R8 ] [ (3, 3) ] 5 s_inr;
      e1 ~m:10 "dcr" 0x05 [ R8 ] [ (3, 3) ] 5 s_dcr;
      e1 "lxi" 0x01 [ Rp; D16 ] [ (4, 2) ] 10 s_lxi;
      e1 "inx" 0x03 [ Rp ] [ (4, 2) ] 5 s_inx;
      e1 "dcx" 0x0b [ Rp ] [ (4, 2) ] 5 s_dcx;
      e1 "dad" 0x09 [ Rp ] [ (4, 2) ] 10 s_dad;
      e1 "ldax" 0x0a [ Rp_bd ] [ (4, 1) ] 7 s_ldax;
      e1 "stax" 0x02 [ Rp_bd ] [ (4, 1) ] 7 s_stax;
      e1 "push" 0xc5 [ Rp_psw ] [ (4, 2) ] 11 s_push;
      e1 "pop" 0xc1 [ Rp_psw ] [ (4, 2) ] 10 s_pop;
      alu "add" 0x80 Add;
      alu "adc" 0x88 Adc;
      alu "sub" 0x90 Sub;
      alu "sbb" 0x98 Sbb;
      alu "ana" 0xa0 Ana;
      alu "xra" 0xa8 Xra;
      alu "ora" 0xb0 Ora;
      alu "cmp" 0xb8 Cmp;
      imm8 "adi" 0xc6 Add;
      imm8 "aci" 0xce Adc;
      imm8 "sui" 0xd6 Sub;
      imm8 "sbi" 0xde Sbb;
      imm8 "ani" 0xe6 Ana;
      imm8 "xri" 0xee Xra;
      imm8 "ori" 0xf6 Ora;
      imm8 "cpi" 0xfe Cmp;
      e1 "in" 0xdb [ P8 ] [] 10 s_in;
      e1 "out" 0xd3 [ P8 ] [] 10 s_out;
      e1 "rst" 0xc7 [ Rst ] [ (3, 3) ] 11 s_rst;
      addr "jmp" 0xc3 10 s_jmp;
      addr "jnz" 0xc2 10 (s_jcc `Nz);
      addr "jz" 0xca 10 (s_jcc `Z);
      addr "jnc" 0xd2 10 (s_jcc `Nc);
      addr "jc" 0xda 10 (s_jcc `C);
      addr "jpo" 0xe2 10 (s_jcc `Po);
      addr "jpe" 0xea 10 (s_jcc `Pe);
      addr "jp" 0xf2 10 (s_jcc `P);
      addr "jm" 0xfa 10 (s_jcc `M);
      addr "call" 0xcd 17 s_call;
      addr ~taken:17 "cnz" 0xc4 11 (s_ccc `Nz);
      addr ~taken:17 "cz" 0xcc 11 (s_ccc `Z);
      addr ~taken:17 "cnc" 0xd4 11 (s_ccc `Nc);
      addr ~taken:17 "cc" 0xdc 11 (s_ccc `C);
      addr ~taken:17 "cpo" 0xe4 11 (s_ccc `Po);
      addr ~taken:17 "cpe" 0xec 11 (s_ccc `Pe);
      addr ~taken:17 "cp" 0xf4 11 (s_ccc `P);
      addr ~taken:17 "cm" 0xfc 11 (s_ccc `M);
      addr "sta" 0x32 13 s_sta;
      addr "lda" 0x3a 13 s_lda;
      addr "shld" 0x22 16 s_shld;
      addr "lhld" 0x2a 16 s_lhld;
      e0 "ret" 0xc9 10 s_ret;
      e0 ~taken:11 "rnz" 0xc0 5 (s_rcc `Nz);
      e0 ~taken:11 "rz" 0xc8 5 (s_rcc `Z);
      e0 ~taken:11 "rnc" 0xd0 5 (s_rcc `Nc);
      e0 ~taken:11 "rc" 0xd8 5 (s_rcc `C);
      e0 ~taken:11 "rpo" 0xe0 5 (s_rcc `Po);
      e0 ~taken:11 "rpe" 0xe8 5 (s_rcc `Pe);
      e0 ~taken:11 "rp" 0xf0 5 (s_rcc `P);
      e0 ~taken:11 "rm" 0xf8 5 (s_rcc `M);
      e0 "xchg" 0xeb 4 s_xchg;
      e0 "xthl" 0xe3 18 s_xthl;
      e0 "sphl" 0xf9 5 s_sphl;
      e0 "pchl" 0xe9 5 s_pchl;
      e0 "ei" 0xfb 4 s_ei;
      e0 "di" 0xf3 4 s_di;
      e0 "daa" 0x27 4 s_daa;
      e0 "cma" 0x2f 4 s_cma;
      e0 "stc" 0x37 4 s_stc;
      e0 "cmc" 0x3f 4 s_cmc;
      e0 "rlc" 0x07 4 (s_rotate ~left:true ~through:false);
      e0 "rrc" 0x0f 4 (s_rotate ~left:false ~through:false);
      e0 "ral" 0x17 4 (s_rotate ~left:true ~through:true);
      e0 "rar" 0x1f 4 (s_rotate ~left:false ~through:true);
    ]

let find_entry name = List.find_opt (fun e -> String.equal e.mnemonic name) entries

(* The twelve undocumented bytes: [(byte, documented byte it duplicates)]. They
   execute exactly as their target, including length and timing. *)
let aliases =
  [
    (0x08, 0x00);
    (0x10, 0x00);
    (0x18, 0x00);
    (0x20, 0x00);
    (0x28, 0x00);
    (0x30, 0x00);
    (0x38, 0x00);
    (0xcb, 0xc3);
    (0xd9, 0xc9);
    (0xdd, 0xcd);
    (0xed, 0xcd);
    (0xfd, 0xcd);
  ]

(* Field values for one byte, in operand order of the field-valued operands. *)
type form = Documented of entry * int list | Alias of int  (** the documented byte *)

let byte_of (e : entry) (values : int list) =
  List.fold_left2 (fun acc (shift, _) v -> acc lor (v lsl shift)) e.base e.fields values

let rec combos = function
  | [] -> [ [] ]
  | n :: rest ->
      let tails = combos rest in
      List.concat_map (fun v -> List.map (fun t -> v :: t) tails) (List.init n Fun.id)

(* The byte -> form map. A byte claimed twice by documented entries is a table
   bug and raises at start-up; [mov m,m] is the one intended overlap, resolved
   in favour of [hlt] by listing order. *)
let forms : form option array =
  let a = Array.make 256 None in
  List.iter
    (fun e ->
      let cards =
        List.filter_map (fun o -> if in_field o then Some (cardinality o) else None) e.operands
      in
      List.iter
        (fun values ->
          let b = byte_of e values in
          match a.(b) with
          | None -> a.(b) <- Some (Documented (e, values))
          | Some _ when e.mnemonic = "mov" && b = 0x76 -> ()
          | Some _ -> invalid_arg (Printf.sprintf "i8080_ops: byte %02x claimed twice" b))
        (combos cards))
    entries;
  List.iter
    (fun (b, target) ->
      match a.(b) with
      | None -> a.(b) <- Some (Alias target)
      | Some _ -> invalid_arg (Printf.sprintf "i8080_ops: alias %02x is documented" b))
    aliases;
  a

let form_of_byte b = forms.(b land 0xff)

(* Register spellings, by field value. *)
let r8_names = [| "b"; "c"; "d"; "e"; "h"; "l"; "m"; "a" |]
let rp_names = [| "b"; "d"; "h"; "sp" |]
let rp_psw_names = [| "b"; "d"; "h"; "psw" |]
let rp_bd_names = [| "b"; "d" |]

let names_of = function
  | R8 -> r8_names
  | Rp -> rp_names
  | Rp_psw -> rp_psw_names
  | Rp_bd -> rp_bd_names
  | Rst | D8 | P8 | D16 | A16 -> [||]

(* Length and no-wait T-states of each of the 256 bytes, from the table. An
   alias takes its twin's. *)
type timing = { length : int; tstates : int; taken : int }

let timings : timing array =
  let of_entry e values =
    let mem = List.exists2 (fun o v -> o = R8 && v = 6) (List.filter in_field e.operands) values in
    {
      length = length e;
      tstates = (if mem then e.t_mem else e.t_base);
      taken = (if mem then e.t_mem else e.t_taken);
    }
  in
  let documented b =
    match forms.(b) with
    | Some (Documented (e, values)) -> of_entry e values
    | Some (Alias _) | None -> invalid_arg (Printf.sprintf "i8080_ops: %02x has no timing" b)
  in
  Array.init 256 (fun b ->
      match forms.(b) with
      | Some (Alias twin) -> documented twin
      | Some (Documented _) | None -> documented b)
