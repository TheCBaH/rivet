(* The Intel 8080 descriptor table: every documented instruction family, the
   twelve undocumented aliases, and the byte -> form map derived from them.

   Data only. The encoder's codec, the operand legality checks, the decoder and
   the canonical printer all read this table; nothing else lists an opcode. An
   entry is an opcode byte with its register fields zeroed ([base]), the
   operands the source spells, and where each register-valued operand sits in
   the byte. *)

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

let e0 mnemonic base = { mnemonic; base; operands = []; fields = [] }
let e1 mnemonic base operands fields = { mnemonic; base; operands; fields }
let alu mnemonic base = e1 mnemonic base [ R8 ] [ (0, 3) ]
let imm8 mnemonic base = e1 mnemonic base [ D8 ] []
let addr mnemonic base = e1 mnemonic base [ A16 ] []

let entries : entry list =
  [ e0 "nop" 0x00; e0 "hlt" 0x76 ]
  @ [
      (* 0x76 is [mov m,m]'s slot; the table lists [hlt] first and the
         enumeration below skips the second claim. *)
      e1 "mov" 0x40 [ R8; R8 ] [ (3, 3); (0, 3) ];
      e1 "mvi" 0x06 [ R8; D8 ] [ (3, 3) ];
      e1 "inr" 0x04 [ R8 ] [ (3, 3) ];
      e1 "dcr" 0x05 [ R8 ] [ (3, 3) ];
      e1 "lxi" 0x01 [ Rp; D16 ] [ (4, 2) ];
      e1 "inx" 0x03 [ Rp ] [ (4, 2) ];
      e1 "dcx" 0x0b [ Rp ] [ (4, 2) ];
      e1 "dad" 0x09 [ Rp ] [ (4, 2) ];
      e1 "ldax" 0x0a [ Rp_bd ] [ (4, 1) ];
      e1 "stax" 0x02 [ Rp_bd ] [ (4, 1) ];
      e1 "push" 0xc5 [ Rp_psw ] [ (4, 2) ];
      e1 "pop" 0xc1 [ Rp_psw ] [ (4, 2) ];
      alu "add" 0x80;
      alu "adc" 0x88;
      alu "sub" 0x90;
      alu "sbb" 0x98;
      alu "ana" 0xa0;
      alu "xra" 0xa8;
      alu "ora" 0xb0;
      alu "cmp" 0xb8;
      imm8 "adi" 0xc6;
      imm8 "aci" 0xce;
      imm8 "sui" 0xd6;
      imm8 "sbi" 0xde;
      imm8 "ani" 0xe6;
      imm8 "xri" 0xee;
      imm8 "ori" 0xf6;
      imm8 "cpi" 0xfe;
      e1 "in" 0xdb [ P8 ] [];
      e1 "out" 0xd3 [ P8 ] [];
      e1 "rst" 0xc7 [ Rst ] [ (3, 3) ];
      addr "jmp" 0xc3;
      addr "jnz" 0xc2;
      addr "jz" 0xca;
      addr "jnc" 0xd2;
      addr "jc" 0xda;
      addr "jpo" 0xe2;
      addr "jpe" 0xea;
      addr "jp" 0xf2;
      addr "jm" 0xfa;
      addr "call" 0xcd;
      addr "cnz" 0xc4;
      addr "cz" 0xcc;
      addr "cnc" 0xd4;
      addr "cc" 0xdc;
      addr "cpo" 0xe4;
      addr "cpe" 0xec;
      addr "cp" 0xf4;
      addr "cm" 0xfc;
      addr "sta" 0x32;
      addr "lda" 0x3a;
      addr "shld" 0x22;
      addr "lhld" 0x2a;
      e0 "ret" 0xc9;
      e0 "rnz" 0xc0;
      e0 "rz" 0xc8;
      e0 "rnc" 0xd0;
      e0 "rc" 0xd8;
      e0 "rpo" 0xe0;
      e0 "rpe" 0xe8;
      e0 "rp" 0xf0;
      e0 "rm" 0xf8;
      e0 "xchg" 0xeb;
      e0 "xthl" 0xe3;
      e0 "sphl" 0xf9;
      e0 "pchl" 0xe9;
      e0 "ei" 0xfb;
      e0 "di" 0xf3;
      e0 "daa" 0x27;
      e0 "cma" 0x2f;
      e0 "stc" 0x37;
      e0 "cmc" 0x3f;
      e0 "rlc" 0x07;
      e0 "rrc" 0x0f;
      e0 "ral" 0x17;
      e0 "rar" 0x1f;
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
