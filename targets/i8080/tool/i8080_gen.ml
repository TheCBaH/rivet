(* Seeded random 8080 programs for differential runs.

   A program starts from a random register and flag state, runs a body of
   random units (every documented form is drawn: moves, arithmetic, logic,
   rotates, DAA, stack traffic, memory through BC/DE/HL and absolute
   addresses, IN/OUT, forward jumps, calls, RST) and ends with an epilogue that
   stores A, F, B..L and SP into a result block and writes the data window and
   the block both raw (port 80h) and as hexadecimal text (console, port 1).
   Every memory access stays inside the data window or the stack area, so the
   whole 64 KiB image after the run is a function of the seed. The random
   generator is a 30-bit LCG: the same seed gives the same program on every
   OCaml version and word size. *)

type rng = { mutable x : int }

let make seed = { x = ((seed * 40503) + 12345) land 0x3fffffff }

let next r =
  r.x <- ((r.x * 69069) + 1) land 0x3fffffff;
  r.x lsr 12

let below r n = next r mod n
let pick r l = List.nth l (below r (List.length l))
let chance r pct = below r 100 < pct
let data = 0x2000
let data_size = 0x80
let result = data + data_size
let stack = 0x3000
let hexb n = Printf.sprintf "0%02xh" (n land 0xff)
let hexw n = Printf.sprintf "0%04xh" (n land 0xffff)
let edges = [ 0; 1; 2; 7; 8; 9; 0x0f; 0x10; 0x7f; 0x80; 0x81; 0xaa; 0x55; 0xfe; 0xff ]
let byte r = if chance r 50 then pick r edges else below r 256
let word r = if chance r 40 then (byte r lsl 8) lor byte r else below r 0x10000
let regs8 = [ "b"; "c"; "d"; "e"; "h"; "l"; "m"; "a" ]
let conds = [ "nz"; "z"; "nc"; "c"; "po"; "pe"; "p"; "m" ]
let alu = [ "add"; "adc"; "sub"; "sbb"; "ana"; "xra"; "ora"; "cmp" ]
let alu_imm = [ "adi"; "aci"; "sui"; "sbi"; "ani"; "xri"; "ori"; "cpi" ]
let alias_nops = [ 0x08; 0x10; 0x18; 0x20; 0x28; 0x30; 0x38 ]

type ctx = {
  r : rng;
  alias : bool;  (** the undocumented alias bytes may appear *)
  out : Buffer.t;
  mutable labels : int;
  mutable subs : (string * int) list;  (** called subroutines still to be written *)
}

let line c fmt = Printf.ksprintf (fun s -> Buffer.add_string c.out ("\t" ^ s ^ "\n")) fmt
let label c name = Buffer.add_string c.out (name ^ ":\n")

let fresh c prefix =
  c.labels <- c.labels + 1;
  Printf.sprintf "%s%d" prefix c.labels

let window c = data + below c.r (data_size - 2)
let with_hl c r8 = if r8 = "m" then line c "lxi\th,%s" (hexw (window c))

(* [stack_ok]: the unit may not change SP. [depth]: nesting of control units. *)
let rec unit_ c ~stack_ok ~depth =
  let r = c.r in
  let choices =
    [ `Mov; `Mvi; `Incdec; `Alu; `Alu_imm; `Rp; `Lxi; `Misc; `Ldax; `Abs; `Io; `Alias; `Bcd ]
    @ (if depth < 2 then [ `Skip; `Call; `Pair; `Rst; `Pchl ] else [])
    @ if stack_ok then [ `Stack_excursion; `Spops ] else []
  in
  match pick r choices with
  | `Mov ->
      let d = pick r regs8 and s = pick r regs8 in
      if d = "m" && s = "m" then line c "nop"
      else (
        if d = "m" || s = "m" then line c "lxi\th,%s" (hexw (window c));
        line c "mov\t%s,%s" d s)
  | `Mvi ->
      let d = pick r regs8 in
      with_hl c d;
      line c "mvi\t%s,%s" d (hexb (byte r))
  | `Incdec ->
      let d = pick r regs8 in
      with_hl c d;
      line c "%s\t%s" (pick r [ "inr"; "dcr" ]) d
  | `Alu ->
      let s = pick r regs8 in
      with_hl c s;
      line c "%s\t%s" (pick r alu) s
  | `Alu_imm -> line c "%s\t%s" (pick r alu_imm) (hexb (byte r))
  | `Rp ->
      let op = pick r [ "inx"; "dcx"; "dad" ] in
      let rp =
        pick r
          ([ "b"; "d"; "h" ] @ if stack_ok then [ "sp" ] else if op = "dad" then [ "sp" ] else [])
      in
      line c "%s\t%s" op rp
  | `Lxi ->
      if stack_ok && chance r 15 then line c "lxi\tsp,%s" (hexw (stack - (2 * below r 8)))
      else line c "lxi\t%s,%s" (pick r [ "b"; "d"; "h" ]) (hexw (word r))
  | `Misc ->
      line c "%s" (pick r [ "rlc"; "rrc"; "ral"; "rar"; "daa"; "cma"; "stc"; "cmc"; "xchg"; "nop" ])
  | `Ldax ->
      let rp = pick r [ "b"; "d" ] in
      line c "lxi\t%s,%s" rp (hexw (window c));
      line c "%s\t%s" (pick r [ "ldax"; "stax" ]) rp
  | `Bcd ->
      let digit () = below r 10 in
      let bcd () = (digit () lsl 4) lor digit () in
      line c "mvi\ta,%s" (hexb (bcd ()));
      line c "%s\t%s" (pick r [ "adi"; "aci"; "sui"; "sbi" ]) (hexb (bcd ()));
      line c "daa"
  | `Abs -> (
      let a = hexw (window c) in
      match below r 4 with
      | 0 -> line c "lda\t%s" a
      | 1 -> line c "sta\t%s" a
      | 2 -> line c "lhld\t%s" a
      | _ -> line c "shld\t%s" a)
  | `Io ->
      if chance r 50 then line c "in\t%s" (hexb (0x60 + below r 16))
      else line c "out\t%s" (hexb (0x70 + below r 16))
  | `Alias ->
      if chance r 20 then line c "%s" (pick r [ "ei"; "di" ])
      else if c.alias then line c "db\t%s" (hexb (pick r alias_nops))
      else line c "nop"
  | `Skip ->
      let l = fresh c "skip" in
      (match below r 3 with
      | 0 -> line c "jmp\t%s" l
      | 1 when c.alias -> line c "db\t0cbh\n\tdw\t%s" l
      | _ -> line c "j%s\t%s" (pick r conds) l);
      for _ = 1 to below r 3 do
        unit_ c ~stack_ok ~depth:(depth + 1)
      done;
      label c l
  | `Call ->
      let name = fresh c "sub" in
      c.subs <- (name, depth) :: c.subs;
      if chance r 20 then line c "db\t0cdh\n\tdw\t%s" name
      else if c.alias && chance r 10 then
        line c "db\t%s\n\tdw\t%s" (hexb (pick r [ 0xdd; 0xed; 0xfd ])) name
      else if chance r 50 then line c "call\t%s" name
      else line c "c%s\t%s" (pick r conds) name
  | `Pair ->
      let rp = pick r [ "b"; "d"; "h"; "psw" ] in
      line c "push\t%s" rp;
      for _ = 1 to 1 + below r 3 do
        unit_ c ~stack_ok:false ~depth:(depth + 1)
      done;
      if chance r 25 then line c "xthl";
      line c "pop\t%s" (pick r [ "b"; "d"; "h"; "psw" ])
  | `Rst -> line c "rst\t%d" (1 + below r 7)
  | `Pchl ->
      let l = fresh c "tgt" in
      line c "lxi\th,%s" l;
      line c "pchl";
      for _ = 1 to below r 3 do
        line c "hlt"
      done;
      label c l
  | `Stack_excursion ->
      line c "lxi\th,%s" (hexw (data + data_size - 8));
      line c "sphl";
      for _ = 1 to 1 + below r 2 do
        let rp = pick r [ "b"; "d"; "psw" ] in
        line c "push\t%s" rp;
        if chance r 30 then line c "xthl";
        line c "pop\t%s" (pick r [ "b"; "d"; "h"; "psw" ])
      done;
      line c "lxi\tsp,%s" (hexw stack)
  | `Spops ->
      (* pops from the zeroed stack area below SP are harmless; pushes are paired *)
      line c "push\t%s" (pick r [ "b"; "d"; "h"; "psw" ]);
      line c "pop\t%s" (pick r [ "b"; "d"; "h"; "psw" ])

let write_sub c (name, depth) =
  let r = c.r in
  label c name;
  for _ = 1 to 1 + below r 3 do
    unit_ c ~stack_ok:false ~depth:(depth + 2)
  done;
  if chance r 50 then (
    line c "r%s" (pick r conds);
    for _ = 1 to 1 + below r 2 do
      unit_ c ~stack_ok:false ~depth:(depth + 2)
    done);
  if c.alias && chance r 10 then line c "db\t0d9h" else line c "ret"

(** The source of the program for [seed], [units] body units long. Seeds that
    are multiples of 3 may use the undocumented alias bytes; multiples of 4 end
    with [hlt] instead of the warm boot. *)
let program ~seed ~units =
  let r = make seed in
  let c = { r; alias = seed mod 3 = 0; out = Buffer.create 4096; labels = 0; subs = [] } in
  let add s = Buffer.add_string c.out s in
  add
    (Printf.sprintf "; generated program, seed %d, %d units\n\tcpu\t8080\n\torg\t0100h\n" seed units);
  line c "lxi\tsp,%s" (hexw stack);
  line c "mvi\ta,0c9h";
  List.iter (fun v -> line c "sta\t%s" (hexw v)) [ 8; 0x10; 0x18; 0x20; 0x28; 0x30; 0x38 ];
  line c "lxi\tb,%s" (hexw (word r));
  line c "lxi\td,%s" (hexw (word r));
  line c "lxi\th,%s" (hexw ((byte r lsl 8) lor (byte r land 0xd5 lor 0x02)));
  line c "push\th";
  line c "pop\tpsw";
  line c "lxi\th,%s" (hexw (word r));
  for _ = 1 to units do
    unit_ c ~stack_ok:true ~depth:0
  done;
  line c "jmp\tepilogue";
  let rec drain () =
    match c.subs with
    | [] -> ()
    | pending ->
        c.subs <- [];
        List.iter (write_sub c) (List.rev pending);
        drain ()
  in
  drain ();
  label c "hex";
  line c "ani\t0fh";
  line c "cpi\t10";
  line c "jc\thex1";
  line c "adi\t7";
  label c "hex1";
  line c "adi\t30h";
  line c "out\t01h";
  line c "ret";
  label c "epilogue";
  line c "shld\t%s" (hexw (result + 7));
  line c "sta\t%s" (hexw result);
  line c "push\tpsw";
  line c "pop\th";
  line c "shld\t%s" (hexw (result + 1));
  List.iteri
    (fun i reg ->
      line c "mov\ta,%s" reg;
      line c "sta\t%s" (hexw (result + 3 + i)))
    [ "b"; "c"; "d"; "e" ];
  line c "lxi\th,0";
  line c "dad\tsp";
  line c "shld\t%s" (hexw (result + 9));
  line c "lxi\th,%s" (hexw data);
  line c "mvi\tb,%d" (data_size + 11);
  label c "dump";
  line c "mov\ta,m";
  line c "out\t80h";
  line c "rrc";
  line c "rrc";
  line c "rrc";
  line c "rrc";
  line c "call\thex";
  line c "mov\ta,m";
  line c "call\thex";
  line c "inx\th";
  line c "dcr\tb";
  line c "jnz\tdump";
  line c "mvi\ta,0ah";
  line c "out\t01h";
  line c "di";
  if seed mod 4 = 0 then line c "hlt"
  else if seed mod 5 = 0 then line c "rst\t0"
  else line c "jmp\t0";
  add (Printf.sprintf "\torg\t%s\n" (hexw data));
  add "\tdb\t";
  add (String.concat "," (List.init data_size (fun _ -> hexb (byte r))));
  add "\n\tdb\t11 dup (0)\n";
  Buffer.contents c.out
