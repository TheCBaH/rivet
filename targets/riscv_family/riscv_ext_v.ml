(* The vector extension and its crypto and BF16 subsets. Zvkb is the common part of Zvbb and the
   Zvkn and Zvks suites, so the forms they share are legal under either leaf. Zvkn and Zvks and
   their sums are implication-only groups, off by default. *)

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

let v = Riscv_ext.component ~id:"riscv.v" ~feature:"v" ~requires:[ "d" ] ~summary:"vector" []

(* Every form is a generated table row, owned through its feature. *)
let leaf feature ?(implies = []) summary =
  Riscv_ext.component ~id:("riscv." ^ feature) ~feature ~requires:[ "v" ] ~implies ~legal ~summary
    []

let zvbb = leaf "zvbb" ~implies:[ "zvkb" ] "vector basic bit manipulation"
let zvkned = leaf "zvkned" "vector AES block cipher"
let zvbc = leaf "zvbc" "vector carry-less multiply"
let zvknha = leaf "zvknha" "vector SHA-2 (SHA-256)"
let zvksed = leaf "zvksed" "vector SM4 block cipher"
let zvkg = leaf "zvkg" "vector GCM/GMAC"
let zvksh = leaf "zvksh" "vector SM3 hash function"
let zvfbfmin = leaf "zvfbfmin" "vector BF16 conversion"
let zvfbfwma = leaf "zvfbfwma" "vector BF16 widening multiply-accumulate"
let zvkb = leaf "zvkb" "vector cryptography bit manipulation"
let zvknhb = leaf "zvknhb" ~implies:[ "zvknha" ] "vector SHA-2 (SHA-256 and SHA-512)"

let group feature summary implies =
  Riscv_ext.component ~id:("riscv." ^ feature) ~feature ~implies ~default_on:false ~summary []

let zvkn = group "zvkn" "vector NIST algorithm suite" [ "zvkned"; "zvknhb"; "zvkb" ]
let zvks = group "zvks" "vector ShangMi algorithm suite" [ "zvksed"; "zvksh"; "zvkb" ]
let zvknc = group "zvknc" "vector NIST suite with carry-less multiply" [ "zvkn"; "zvbc" ]
let zvkng = group "zvkng" "vector NIST suite with GCM" [ "zvkn"; "zvkg" ]
let zvksc = group "zvksc" "vector ShangMi suite with carry-less multiply" [ "zvks"; "zvbc" ]
let zvksg = group "zvksg" "vector ShangMi suite with GCM" [ "zvks"; "zvkg" ]
