(* Bit-manipulation, scalar-crypto and conditional-operation extensions. A form shared with
   another extension is owned by its primary one and lists the others as alternatives: the K
   extensions (Zk, Zkn, Zks) are the sums of the leaf extensions below and are modelled as
   implications, so a mnemonic's alternatives name only leaf extensions. *)

module Req = Target_component.Req
module Opcode = Riscv_types.Opcode

let alternatives =
  [
    ("andn", [ "zbb"; "zbkb" ]);
    ("orn", [ "zbb"; "zbkb" ]);
    ("xnor", [ "zbb"; "zbkb" ]);
    ("rol", [ "zbb"; "zbkb" ]);
    ("ror", [ "zbb"; "zbkb" ]);
    ("rev8", [ "zbb"; "zbkb" ]);
    ("rolw", [ "zbb"; "zbkb" ]);
    ("rorw", [ "zbb"; "zbkb" ]);
    ("rori", [ "zbb"; "zbkb" ]);
    ("roriw", [ "zbb"; "zbkb" ]);
    ("clmul", [ "zbc"; "zbkc" ]);
    ("clmulh", [ "zbc"; "zbkc" ]);
    ("aes64ks2", [ "zknd"; "zkne" ]);
    ("aes64ks1i", [ "zknd"; "zkne" ]);
  ]

let legal mnemonic =
  match List.assoc_opt mnemonic alternatives with
  | Some fs -> Some (Req.Any (List.map (fun f -> Req.Feature f) fs))
  | None -> None

let zba_forms =
  [
    (Opcode.Sh1add, "rv_zba/sh1add");
    (Opcode.Sh2add, "rv_zba/sh2add");
    (Opcode.Sh3add, "rv_zba/sh3add");
    (Opcode.Sh1adduw, "rv64_zba/sh1add.uw");
    (Opcode.Sh2adduw, "rv64_zba/sh2add.uw");
    (Opcode.Sh3adduw, "rv64_zba/sh3add.uw");
  ]

let zbb_forms =
  [
    (Opcode.Min, "rv_zbb/min");
    (Opcode.Minu, "rv_zbb/minu");
    (Opcode.Max, "rv_zbb/max");
    (Opcode.Maxu, "rv_zbb/maxu");
    (Opcode.Andn, "rv_zbb/andn");
    (Opcode.Orn, "rv_zbb/orn");
    (Opcode.Xnor, "rv_zbb/xnor");
    (Opcode.Rol, "rv_zbb/rol");
    (Opcode.Ror, "rv_zbb/ror");
    (Opcode.Clz, "rv_zbb/clz");
    (Opcode.Ctz, "rv_zbb/ctz");
    (Opcode.Cpop, "rv_zbb/cpop");
    (Opcode.Sextb, "rv_zbb/sext.b");
    (Opcode.Sexth, "rv_zbb/sext.h");
    (Opcode.Orcb, "rv_zbb/orc.b");
    (Opcode.Clzw, "rv64_zbb/clzw");
    (Opcode.Ctzw, "rv64_zbb/ctzw");
    (Opcode.Cpopw, "rv64_zbb/cpopw");
    (Opcode.Rev8, "rv32_zbkb/rev8");
    (Opcode.Rolw, "rv64_zbb/rolw");
    (Opcode.Rorw, "rv64_zbb/rorw");
    (Opcode.Rori, "rv64_zbb/rori");
    (Opcode.Roriw, "rv64_zbb/roriw");
    (Opcode.Zext_h, "rv64_zbb/zext.h");
  ]

let zbs_forms =
  [
    (Opcode.Bclr, "rv_zbs/bclr");
    (Opcode.Bext, "rv_zbs/bext");
    (Opcode.Binv, "rv_zbs/binv");
    (Opcode.Bset, "rv_zbs/bset");
    (Opcode.Bclri, "rv64_zbs/bclri");
    (Opcode.Bexti, "rv64_zbs/bexti");
    (Opcode.Binvi, "rv64_zbs/binvi");
    (Opcode.Bseti, "rv64_zbs/bseti");
  ]

let zbc_forms =
  [
    (Opcode.Clmul, "rv_zbc/clmul");
    (Opcode.Clmulh, "rv_zbc/clmulh");
    (Opcode.Clmulr, "rv_zbc/clmulr");
  ]

let zbkb_forms =
  [
    (Opcode.Brev8, "rv_zbkb/brev8");
    (Opcode.Pack, "rv_zbkb/pack");
    (Opcode.Packh, "rv_zbkb/packh");
    (Opcode.Packw, "rv64_zbkb/packw");
    (Opcode.Zip, "rv32_zbkb/zip");
    (Opcode.Unzip, "rv32_zbkb/unzip");
  ]

let zbkx_forms = [ (Opcode.Xperm4, "rv_zbkx/xperm4"); (Opcode.Xperm8, "rv_zbkx/xperm8") ]

let zknd_forms =
  [
    (Opcode.Aes64ds, "rv64_zknd/aes64ds");
    (Opcode.Aes64dsm, "rv64_zknd/aes64dsm");
    (Opcode.Aes64ks2, "rv64_zknd/aes64ks2");
    (Opcode.Aes64im, "rv64_zknd/aes64im");
    (Opcode.Aes64ks1i, "rv64_zknd/aes64ks1i");
    (Opcode.Aes32dsi, "rv32_zknd/aes32dsi");
    (Opcode.Aes32dsmi, "rv32_zknd/aes32dsmi");
  ]

let zkne_forms =
  [
    (Opcode.Aes64es, "rv64_zkne/aes64es");
    (Opcode.Aes64esm, "rv64_zkne/aes64esm");
    (Opcode.Aes32esi, "rv32_zkne/aes32esi");
    (Opcode.Aes32esmi, "rv32_zkne/aes32esmi");
  ]

let zknh_forms =
  [
    (Opcode.Sha256sum0, "rv_zknh/sha256sum0");
    (Opcode.Sha256sum1, "rv_zknh/sha256sum1");
    (Opcode.Sha256sig0, "rv_zknh/sha256sig0");
    (Opcode.Sha256sig1, "rv_zknh/sha256sig1");
    (Opcode.Sha512sum0, "rv64_zknh/sha512sum0");
    (Opcode.Sha512sum1, "rv64_zknh/sha512sum1");
    (Opcode.Sha512sig0, "rv64_zknh/sha512sig0");
    (Opcode.Sha512sig1, "rv64_zknh/sha512sig1");
    (Opcode.Sha512sum0r, "rv32_zknh/sha512sum0r");
    (Opcode.Sha512sum1r, "rv32_zknh/sha512sum1r");
    (Opcode.Sha512sig0l, "rv32_zknh/sha512sig0l");
    (Opcode.Sha512sig1l, "rv32_zknh/sha512sig1l");
    (Opcode.Sha512sig0h, "rv32_zknh/sha512sig0h");
    (Opcode.Sha512sig1h, "rv32_zknh/sha512sig1h");
  ]

let zksed_forms = [ (Opcode.Sm4ed, "rv_zksed/sm4ed"); (Opcode.Sm4ks, "rv_zksed/sm4ks") ]
let zksh_forms = [ (Opcode.Sm3p0, "rv_zksh/sm3p0"); (Opcode.Sm3p1, "rv_zksh/sm3p1") ]

let zicond_forms =
  [ (Opcode.Czero_eqz, "rv_zicond/czero.eqz"); (Opcode.Czero_nez, "rv_zicond/czero.nez") ]

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
