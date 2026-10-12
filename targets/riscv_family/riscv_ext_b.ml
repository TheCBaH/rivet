(* Bit-manipulation, scalar-crypto and conditional-operation extensions. A form shared with
   another extension is owned by its primary one and lists the others as alternatives: the K
   extensions (Zk, Zkn, Zks) are the sums of the leaf extensions below and are modelled as
   implications, so a mnemonic's alternatives name only leaf extensions. *)

module Req = Target_component.Req

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
    ("sh1add", "rv_zba/sh1add");
    ("sh2add", "rv_zba/sh2add");
    ("sh3add", "rv_zba/sh3add");
    ("sh1add.uw", "rv64_zba/sh1add.uw");
    ("sh2add.uw", "rv64_zba/sh2add.uw");
    ("sh3add.uw", "rv64_zba/sh3add.uw");
  ]

let zbb_forms =
  [
    ("min", "rv_zbb/min");
    ("minu", "rv_zbb/minu");
    ("max", "rv_zbb/max");
    ("maxu", "rv_zbb/maxu");
    ("andn", "rv_zbb/andn");
    ("orn", "rv_zbb/orn");
    ("xnor", "rv_zbb/xnor");
    ("rol", "rv_zbb/rol");
    ("ror", "rv_zbb/ror");
    ("clz", "rv_zbb/clz");
    ("ctz", "rv_zbb/ctz");
    ("cpop", "rv_zbb/cpop");
    ("sext.b", "rv_zbb/sext.b");
    ("sext.h", "rv_zbb/sext.h");
    ("orc.b", "rv_zbb/orc.b");
    ("clzw", "rv64_zbb/clzw");
    ("ctzw", "rv64_zbb/ctzw");
    ("cpopw", "rv64_zbb/cpopw");
    ("rev8", "rv32_zbkb/rev8");
    ("rolw", "rv64_zbb/rolw");
    ("rorw", "rv64_zbb/rorw");
    ("rori", "rv64_zbb/rori");
    ("roriw", "rv64_zbb/roriw");
    ("zext.h", "rv64_zbb/zext.h");
  ]

let zbs_forms =
  [
    ("bclr", "rv_zbs/bclr");
    ("bext", "rv_zbs/bext");
    ("binv", "rv_zbs/binv");
    ("bset", "rv_zbs/bset");
    ("bclri", "rv64_zbs/bclri");
    ("bexti", "rv64_zbs/bexti");
    ("binvi", "rv64_zbs/binvi");
    ("bseti", "rv64_zbs/bseti");
  ]

let zbc_forms =
  [ ("clmul", "rv_zbc/clmul"); ("clmulh", "rv_zbc/clmulh"); ("clmulr", "rv_zbc/clmulr") ]

let zbkb_forms =
  [
    ("brev8", "rv_zbkb/brev8");
    ("pack", "rv_zbkb/pack");
    ("packh", "rv_zbkb/packh");
    ("packw", "rv64_zbkb/packw");
    ("zip", "rv32_zbkb/zip");
    ("unzip", "rv32_zbkb/unzip");
  ]

let zbkx_forms = [ ("xperm4", "rv_zbkx/xperm4"); ("xperm8", "rv_zbkx/xperm8") ]

let zknd_forms =
  [
    ("aes64ds", "rv64_zknd/aes64ds");
    ("aes64dsm", "rv64_zknd/aes64dsm");
    ("aes64ks2", "rv64_zknd/aes64ks2");
    ("aes64im", "rv64_zknd/aes64im");
    ("aes64ks1i", "rv64_zknd/aes64ks1i");
    ("aes32dsi", "rv32_zknd/aes32dsi");
    ("aes32dsmi", "rv32_zknd/aes32dsmi");
  ]

let zkne_forms =
  [
    ("aes64es", "rv64_zkne/aes64es");
    ("aes64esm", "rv64_zkne/aes64esm");
    ("aes32esi", "rv32_zkne/aes32esi");
    ("aes32esmi", "rv32_zkne/aes32esmi");
  ]

let zknh_forms =
  [
    ("sha256sum0", "rv_zknh/sha256sum0");
    ("sha256sum1", "rv_zknh/sha256sum1");
    ("sha256sig0", "rv_zknh/sha256sig0");
    ("sha256sig1", "rv_zknh/sha256sig1");
    ("sha512sum0", "rv64_zknh/sha512sum0");
    ("sha512sum1", "rv64_zknh/sha512sum1");
    ("sha512sig0", "rv64_zknh/sha512sig0");
    ("sha512sig1", "rv64_zknh/sha512sig1");
    ("sha512sum0r", "rv32_zknh/sha512sum0r");
    ("sha512sum1r", "rv32_zknh/sha512sum1r");
    ("sha512sig0l", "rv32_zknh/sha512sig0l");
    ("sha512sig1l", "rv32_zknh/sha512sig1l");
    ("sha512sig0h", "rv32_zknh/sha512sig0h");
    ("sha512sig1h", "rv32_zknh/sha512sig1h");
  ]

let zksed_forms = [ ("sm4ed", "rv_zksed/sm4ed"); ("sm4ks", "rv_zksed/sm4ks") ]
let zksh_forms = [ ("sm3p0", "rv_zksh/sm3p0"); ("sm3p1", "rv_zksh/sm3p1") ]
let zicond_forms = [ ("czero.eqz", "rv_zicond/czero.eqz"); ("czero.nez", "rv_zicond/czero.nez") ]

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
