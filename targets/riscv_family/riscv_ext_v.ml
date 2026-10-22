(* The vector extension and its crypto and BF16 subsets. The table rows tagged [v] are split over
   the embedded profiles as GNU as gates them: floating-point forms need Zve32f and loads and
   stores of 64-bit elements need Zve64x. Zve64f, Zve64d and V add no encodings, so they are
   implication-only. Zvkb is the common part of Zvbb and the Zvkn and Zvks suites, so the forms
   they share are legal under either leaf. Zvkn and Zvks and their sums are implication-only
   groups, off by default. *)

module Req = Target_component.Req

let zvkb_forms =
  [
    "vandn.vv";
    "vandn.vx";
    "vbrev8.v";
    "vrev8.v";
    "vrol.vv";
    "vrol.vx";
    "vror.vv";
    "vror.vx";
    "vror.vi";
  ]

let legal mnemonic =
  if List.mem mnemonic zvkb_forms then Some (Req.Any [ Feature "zvbb"; Feature "zvkb" ]) else None

let floating (r : Riscv_table_row.row) =
  (String.starts_with ~prefix:"vf" r.mnemonic || String.starts_with ~prefix:"vmf" r.mnemonic)
  && not (String.equal r.mnemonic "vfirst.m")

(* A vector load or store whose width field (bits 14:12) is 7, EEW=64. *)
let eew64 (r : Riscv_table_row.row) =
  let low = Int64.logand r.match_ 0x707fL in
  Int64.equal low 0x7007L || Int64.equal low 0x7027L

let in_v keep (r : Riscv_table_row.row) = String.equal r.feature "v" && keep r

let zve32x =
  Riscv_ext.component ~id:"riscv.zve32x" ~feature:"zve32x" ~requires:[ "zicsr" ]
    ~rows:(in_v (fun r -> not (floating r || eew64 r)))
    ~summary:"embedded vector, 32-bit integer elements" []

let zve32f =
  Riscv_ext.component ~id:"riscv.zve32f" ~feature:"zve32f" ~requires:[ "zve32x"; "f" ]
    ~rows:(in_v floating) ~summary:"embedded vector, single-precision elements" []

let zve64x =
  Riscv_ext.component ~id:"riscv.zve64x" ~feature:"zve64x" ~requires:[ "zve32x" ] ~rows:(in_v eew64)
    ~summary:"embedded vector, 64-bit integer elements" []

let zve64f =
  Riscv_ext.component ~id:"riscv.zve64f" ~feature:"zve64f" ~implies:[ "zve32f"; "zve64x" ]
    ~summary:"embedded vector, 64-bit integer and single-precision elements" []

let zve64d =
  Riscv_ext.component ~id:"riscv.zve64d" ~feature:"zve64d" ~requires:[ "d" ] ~implies:[ "zve64f" ]
    ~summary:"embedded vector, double-precision elements" []

let v =
  Riscv_ext.component ~id:"riscv.v" ~feature:"v" ~implies:[ "zve64d" ]
    ~rows:(fun _ -> false)
    ~summary:"vector: Zve64d" []

(* Every form is a generated table row, owned through its feature. *)
let leaf feature ?(requires = [ "zve32x" ]) ?(implies = []) summary =
  Riscv_ext.component ~id:("riscv." ^ feature) ~feature ~requires ~implies ~legal ~summary []

let zvbb = leaf "zvbb" ~implies:[ "zvkb" ] "vector basic bit manipulation"
let zvkned = leaf "zvkned" "vector AES block cipher"
let zvbc = leaf "zvbc" ~requires:[ "zve64x" ] "vector carry-less multiply"
let zvknha = leaf "zvknha" "vector SHA-2 (SHA-256)"
let zvksed = leaf "zvksed" "vector SM4 block cipher"
let zvkg = leaf "zvkg" "vector GCM/GMAC"
let zvksh = leaf "zvksh" "vector SM3 hash function"
let zvfbfmin = leaf "zvfbfmin" ~requires:[ "zve32f" ] "vector BF16 conversion"

let zvfbfwma =
  leaf "zvfbfwma"
    ~requires:[ "zve32f"; "zfbfmin"; "zvfbfmin" ]
    "vector BF16 widening multiply-accumulate"

let zvkb = leaf "zvkb" "vector cryptography bit manipulation"

let zvknhb =
  leaf "zvknhb" ~requires:[ "zve64x" ] ~implies:[ "zvknha" ] "vector SHA-2 (SHA-256 and SHA-512)"

let group feature summary implies =
  Riscv_ext.component ~id:("riscv." ^ feature) ~feature ~implies ~default_on:false ~summary []

let zvkn = group "zvkn" "vector NIST algorithm suite" [ "zvkned"; "zvknhb"; "zvkb" ]
let zvks = group "zvks" "vector ShangMi algorithm suite" [ "zvksed"; "zvksh"; "zvkb" ]
let zvknc = group "zvknc" "vector NIST suite with carry-less multiply" [ "zvkn"; "zvbc" ]
let zvkng = group "zvkng" "vector NIST suite with GCM" [ "zvkn"; "zvkg" ]
let zvksc = group "zvksc" "vector ShangMi suite with carry-less multiply" [ "zvks"; "zvbc" ]
let zvksg = group "zvksg" "vector ShangMi suite with GCM" [ "zvks"; "zvkg" ]
