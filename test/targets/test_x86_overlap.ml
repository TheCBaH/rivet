(* Two generated x86 rows overlap when one instruction encoding can match both: the same opcode
   position and prefix, and every fixed field (REX.W, VEX.L, ModR/M digit, mode) equal or
   unconstrained in one of them. Every overlap must be listed here, so a new row cannot shadow
   another by accident. *)
module Row = X86_family_encode.X86_table_row

let modrm_form (r : Row.row) =
  if List.exists (function Row.Mem _ | Row.Vsib _ -> true | _ -> false) r.operands then `Memory
  else if
    r.rm >= 0 || r.digit >= 0
    || List.exists (function Row.Reg { field = Modrm_rm; _ } -> true | _ -> false) r.operands
  then `Register
  else `None

let l_of (r : Row.row) =
  if r.l >= 0 || r.space = Legacy then r.l
  else
    List.fold_left
      (fun acc (o : Row.operand) ->
        match o with
        | Reg { cls = Xmm; _ } -> max acc 0
        | Reg { cls = Ymm; _ } -> max acc 1
        | Reg { cls = Zmm; _ } -> max acc 2
        | _ -> acc)
      (-1) r.operands

let rounding (r : Row.row) = List.exists (function Row.Rounding _ -> true | _ -> false) r.operands
let wild a b = a = b || a = -1 || b = -1
let w_of (r : Row.row) = if r.space = Legacy && r.w = -1 then 0 else r.w

let overlap (a : Row.row) (b : Row.row) =
  a.space = b.space && a.map = b.map && a.opcode = b.opcode && a.prefix = b.prefix && a.osz = b.osz
  && wild (w_of a) (w_of b)
  && wild (l_of a) (l_of b)
  && wild a.digit b.digit && wild a.rm b.rm
  && (a.mode = 0 || b.mode = 0 || a.mode = b.mode)
  && rounding a = rounding b
  && a.evex_p2 = b.evex_p2 && a.bcst = b.bcst && a.mask = b.mask
  && a.pseudo = "" = (b.pseudo = "")
  && (modrm_form a = modrm_form b || modrm_form a = `None || modrm_form b = `None)

(* The 0x90 byte is [nop] as well as [xchg %eax, %eax]; [xchg] with the accumulator is listed in
   both operand orders and encodes the same either way. *)
let approved = [ ("NOP_90", "XCHG_GPRv_OrAX"); ("XCHG_GPRv_OrAX", "XCHG_GPRv_OrAX") ]

let%expect_test "generated x86 rows that overlap are all approved" =
  let rows = X86_family_encode.X86_table_rows.rows in
  let by_position = Hashtbl.create 4096 in
  Array.iteri
    (fun i (r : Row.row) ->
      let k = (r.space, r.map, r.opcode) in
      Hashtbl.replace by_position k (i :: Option.value (Hashtbl.find_opt by_position k) ~default:[]))
    rows;
  let pairs = ref [] in
  Hashtbl.iter
    (fun _ is ->
      List.iter
        (fun i ->
          List.iter
            (fun j -> if i < j && overlap rows.(i) rows.(j) then pairs := (i, j) :: !pairs)
            is)
        is)
    by_position;
  let unapproved =
    List.filter_map
      (fun (i, j) ->
        let a = rows.(i) and b = rows.(j) in
        if List.mem (a.source, b.source) approved || List.mem (b.source, a.source) approved then
          None
        else Some (Printf.sprintf "%s{%s} overlaps %s{%s}" a.mnemonic a.source b.mnemonic b.source))
      (List.sort compare !pairs)
  in
  List.iter print_endline unapproved;
  Printf.printf "%d overlapping pairs, all approved: %b\n" (List.length !pairs) (unapproved = []);
  [%expect {| 5 overlapping pairs, all approved: true |}]
