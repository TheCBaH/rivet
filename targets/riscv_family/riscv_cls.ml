type t =
  | Alu
  | Fp
  | Load
  | Store
  | Atomic
  | Branch
  | Jump
  | Csr
  | System
  | Vector
  | Vector_load
  | Vector_store
  | Custom

let all =
  [
    Alu;
    Fp;
    Load;
    Store;
    Atomic;
    Branch;
    Jump;
    Csr;
    System;
    Vector;
    Vector_load;
    Vector_store;
    Custom;
  ]

let name = function
  | Alu -> "alu"
  | Fp -> "fp"
  | Load -> "load"
  | Store -> "store"
  | Atomic -> "atomic"
  | Branch -> "branch"
  | Jump -> "jump"
  | Csr -> "csr"
  | System -> "system"
  | Vector -> "vector"
  | Vector_load -> "vector-load"
  | Vector_store -> "vector-store"
  | Custom -> "custom"

let bits w lsb width =
  Int64.to_int (Int64.logand (Int64.shift_right_logical w lsb) (Int64.of_int ((1 lsl width) - 1)))

(* LOAD-FP and STORE-FP widths 0 and 5..7 are vector element widths; 1..4 are scalar. *)
let vector_width funct3 = funct3 = 0 || funct3 >= 5

let of_word32 w =
  let funct3 = bits w 12 3 in
  match bits w 0 7 with
  | 0x03 -> Load
  | 0x07 -> if vector_width funct3 then Vector_load else Load
  | 0x23 -> Store
  | 0x27 -> if vector_width funct3 then Vector_store else Store
  | 0x2f -> Atomic
  | 0x63 -> Branch
  | 0x67 | 0x6f -> Jump
  | 0x73 -> if funct3 <> 0 then Csr else System
  | 0x0f -> System
  | 0x53 | 0x43 | 0x47 | 0x4b | 0x4f -> Fp
  | 0x57 -> Vector
  | 0x0b | 0x2b | 0x5b | 0x7b -> Custom
  | _ -> Alu

let of_half ~xlen w =
  let funct3 = bits w 13 3 in
  match (bits w 0 2, funct3) with
  | 0, 0 -> Alu
  | 0, (1 | 2 | 3) -> Load
  | 0, 4 -> if bits w 11 1 = 0 then Load else Store
  | 0, _ -> Store
  | 1, 1 -> if xlen = 32 then Jump else Alu
  | 1, 5 -> Jump
  | 1, (6 | 7) -> Branch
  | 1, _ -> Alu
  | _, 0 -> Alu
  | _, (1 | 2 | 3) -> Load
  | _, 4 -> (
      let rd = bits w 7 5 and rs2 = bits w 2 5 in
      match (bits w 12 1, rd, rs2) with 1, 0, 0 -> System | _, _, 0 -> Jump | _ -> Alu)
  | _, _ -> Store

let of_word ~xlen w = if Int64.logand w 3L = 3L then of_word32 w else of_half ~xlen w
