(* Floating-point extensions beyond F and D. They have only generated rows. Zfa and Zfhmin forms
   that mix formats need the other format's component as well: the format is the last suffix of
   the mnemonic, except for the moves that name the wider format first. *)

module Req = Target_component.Req

let format_req ~owner mnemonic =
  let suffix =
    match String.rindex_opt mnemonic '.' with
    | Some i -> String.sub mnemonic (i + 1) (String.length mnemonic - i - 1)
    | None -> ""
  in
  let also =
    match (mnemonic, suffix) with
    | ("fmvh.x.q" | "fmvp.q.x"), _ -> Some "q"
    | ("fmvh.x.d" | "fmvp.d.x" | "fcvtmod.w.d"), _ -> Some "d"
    | _, "d" -> Some "d"
    | _, "q" -> Some "q"
    | _, "h" -> Some "zfh"
    | _ -> None
  in
  match also with
  | Some f when not (String.equal f owner) -> Some (Req.All [ Feature owner; Feature f ])
  | _ -> None

let q =
  Riscv_ext.component ~id:"riscv.q" ~feature:"q" ~requires:[ "d" ]
    ~summary:"quad-precision floating point" []

let zfh =
  Riscv_ext.component ~id:"riscv.zfh" ~feature:"zfh" ~requires:[ "f" ] ~implies:[ "zfhmin" ]
    ~summary:"half-precision floating point" []

let zfhmin =
  let legal m =
    match m with
    | "fcvt.d.h" | "fcvt.h.d" -> Some (Req.All [ Feature "zfhmin"; Feature "d" ])
    | "fcvt.h.q" | "fcvt.q.h" -> Some (Req.All [ Feature "zfhmin"; Feature "q" ])
    | _ -> None
  in
  Riscv_ext.component ~id:"riscv.zfhmin" ~feature:"zfhmin" ~requires:[ "f" ] ~legal
    ~summary:"half-precision conversion and load/store" []

let zfa =
  Riscv_ext.component ~id:"riscv.zfa" ~feature:"zfa" ~requires:[ "f" ]
    ~legal:(format_req ~owner:"zfa") ~summary:"additional floating-point instructions" []

let zfbfmin =
  Riscv_ext.component ~id:"riscv.zfbfmin" ~feature:"zfbfmin" ~requires:[ "f" ]
    ~summary:"BF16 scalar conversion" []
