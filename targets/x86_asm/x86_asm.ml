(* What the x86 builder libraries share: the typed register views and the register classes'
   numbers. {!X86_32_asm} and {!X86_64_asm} name the registers each mode has. *)

open X86_family_encode
module View = X86_ops.View

type w8 = View.w8
type w16 = View.w16
type w32 = View.w32
type w64 = View.w64
type l128 = View.l128
type l256 = View.l256
type l512 = View.l512
type 'w gpr = 'w View.gpr
type 'l vec = 'l View.vec
type xmm = View.xmm
type ymm = View.ymm
type zmm = View.zmm
type mmx = View.mmx
type kmask = View.kmask
type st = View.st

(* A register class of a mode: the numbers [registers] holds for [cls]. *)
module Regs (C : sig
  type t

  val cls : Rclass.t
  val registers : Reg.t list
  val make : Reg.t -> t
  val reg : t -> Reg.t
end) : Target_encode.REGS with type t = C.t = struct
  type t = C.t

  let numbers =
    List.sort_uniq compare
      (List.filter_map (fun (r : Reg.t) -> if r.cls = C.cls then Some r.num else None) C.registers)

  let mem n = List.mem n numbers
  let of_int n = if mem n then Some (C.make { Reg.cls = C.cls; num = n }) else None

  let of_int_exn n =
    match of_int n with
    | Some r -> r
    | None ->
        invalid_arg (Printf.sprintf "%s register %d" (Reg.name { Reg.cls = C.cls; num = 0 }) n)

  let to_int r = (C.reg r).num
  let count = List.length numbers
  let all = List.map of_int_exn numbers

  let range =
    match numbers with
    | [] -> None
    | lo :: _ ->
        let hi = List.nth numbers (count - 1) in
        if hi - lo + 1 = count then Some (lo, hi) else None

  let name r = Reg.name (C.reg r)
end

(* x87's [%st(1)]-[%st(7)] are read from their own token shape, not a mode's register list. *)
let st_registers = List.init 8 (fun num -> { Reg.cls = St; num })

(* The registers a legacy or VEX encoding reaches: numbers 16-31 (EVEX's upper vectors, APX's
   general registers) take their own views. *)
let base_registers = List.filter (fun (r : Reg.t) -> r.num < 16)
