(* Bit-manipulation, scalar-crypto and conditional-operation extensions. A form shared with
   another extension is owned by its primary one and lists the others as alternatives: the K
   extensions (Zk, Zkn, Zks) are the sums of the leaf extensions below and are modelled as
   implications, so a mnemonic's alternatives name only leaf extensions. *)

module Req = Target_component.Req

let alternatives = Riscv_ops.alternatives

let legal mnemonic =
  match List.assoc_opt mnemonic alternatives with
  | Some fs -> Some (Req.Any (List.map (fun f -> Req.Feature f) fs))
  | None -> None

let zba_forms = Riscv_ops.forms "zba"
let zbb_forms = Riscv_ops.forms "zbb"
let zbs_forms = Riscv_ops.forms "zbs"
let zbc_forms = Riscv_ops.forms "zbc"
let zbkb_forms = Riscv_ops.forms "zbkb"
let zbkx_forms = Riscv_ops.forms "zbkx"
let zknd_forms = Riscv_ops.forms "zknd"
let zkne_forms = Riscv_ops.forms "zkne"
let zknh_forms = Riscv_ops.forms "zknh"
let zksed_forms = Riscv_ops.forms "zksed"
let zksh_forms = Riscv_ops.forms "zksh"
let zicond_forms = Riscv_ops.forms "zicond"

let component ~feature ~summary forms =
  Riscv_ext.component ~id:("riscv." ^ feature) ~feature ~legal ~summary forms

let zba = component ~feature:"zba" ~summary:"address generation" zba_forms
let zbb = component ~feature:"zbb" ~summary:"basic bit manipulation" zbb_forms
let zbs = component ~feature:"zbs" ~summary:"single-bit operations" zbs_forms
let zbc = component ~feature:"zbc" ~summary:"carry-less multiply" zbc_forms
let zbkb = component ~feature:"zbkb" ~summary:"bit manipulation for cryptography" zbkb_forms
let zbkc = component ~feature:"zbkc" ~summary:"carry-less multiply for cryptography" []
let zbkx = component ~feature:"zbkx" ~summary:"crossbar permutations" zbkx_forms
let zknd = component ~feature:"zknd" ~summary:"AES decryption" zknd_forms
let zkne = component ~feature:"zkne" ~summary:"AES encryption" zkne_forms
let zknh = component ~feature:"zknh" ~summary:"SHA-2 hash functions" zknh_forms
let zksed = component ~feature:"zksed" ~summary:"SM4 block cipher" zksed_forms
let zksh = component ~feature:"zksh" ~summary:"SM3 hash function" zksh_forms
let zicond = component ~feature:"zicond" ~summary:"integer conditional operations" zicond_forms

let group ~feature ~summary implies =
  Riscv_ext.component ~id:("riscv." ^ feature) ~feature ~implies ~default_on:false ~summary []

let b = group ~feature:"b" ~summary:"B: Zba, Zbb and Zbs" [ "zba"; "zbb"; "zbs" ]

let zkn =
  group ~feature:"zkn" ~summary:"NIST algorithm suite"
    [ "zbkb"; "zbkc"; "zbkx"; "zkne"; "zknd"; "zknh" ]

let zks =
  group ~feature:"zks" ~summary:"ShangMi algorithm suite"
    [ "zbkb"; "zbkc"; "zbkx"; "zksed"; "zksh" ]

let zk = group ~feature:"zk" ~summary:"standard scalar cryptography" [ "zkn" ]
