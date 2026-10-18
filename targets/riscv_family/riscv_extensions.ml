type definition = { components : Target_component.t list; rows : Riscv_table_row.row array }

module Validated = struct
  type t = { xlen : int; components : Target_component.t list; rows : Riscv_table_row.row array }

  let xlen t = t.xlen
  let components t = t.components
  let rows t = t.rows
end

let standard_components =
  [
    Riscv_ext_m.zmmul;
    Riscv_ext_m.m;
    Riscv_ext_fd.f;
    Riscv_ext_fd.d;
    Riscv_ext_a.a;
    Riscv_ext_a.zacas;
    Riscv_ext_a.zabha;
    Riscv_ext_fp.q;
    Riscv_ext_fp.zfh;
    Riscv_ext_fp.zfhmin;
    Riscv_ext_fp.zfa;
    Riscv_ext_fp.zfbfmin;
    Riscv_ext_b.zba;
    Riscv_ext_b.zbb;
    Riscv_ext_b.zbs;
    Riscv_ext_b.zbc;
    Riscv_ext_b.zbkb;
    Riscv_ext_b.zbkc;
    Riscv_ext_b.zbkx;
    Riscv_ext_b.zknd;
    Riscv_ext_b.zkne;
    Riscv_ext_b.zknh;
    Riscv_ext_b.zksed;
    Riscv_ext_b.zksh;
    Riscv_ext_b.zicond;
    Riscv_ext_b.b;
    Riscv_ext_b.zkn;
    Riscv_ext_b.zks;
    Riscv_ext_b.zk;
    Riscv_ext_sys.zicsr;
    Riscv_ext_sys.zifencei;
    Riscv_ext_sys.zicntr;
    Riscv_ext_sys.zihintntl;
    Riscv_ext_sys.zawrs;
    Riscv_ext_sys.zimop;
    Riscv_ext_sys.zicfiss;
    Riscv_ext_sys.zicfilp;
    Riscv_ext_sys.zicbom;
    Riscv_ext_sys.zicboz;
    Riscv_ext_sys.zicbop;
    Riscv_ext_sys.system;
    Riscv_ext_sys.s;
    Riscv_ext_sys.h;
    Riscv_ext_sys.svinval;
    Riscv_ext_sys.sdext;
    Riscv_ext_sys.ssctr;
    Riscv_ext_v.zve32x;
    Riscv_ext_v.zve32f;
    Riscv_ext_v.zve64x;
    Riscv_ext_v.zve64f;
    Riscv_ext_v.zve64d;
    Riscv_ext_v.v;
    Riscv_ext_v.zvbb;
    Riscv_ext_v.zvkned;
    Riscv_ext_v.zvbc;
    Riscv_ext_v.zvknha;
    Riscv_ext_v.zvksed;
    Riscv_ext_v.zvkg;
    Riscv_ext_v.zvksh;
    Riscv_ext_v.zvfbfmin;
    Riscv_ext_v.zvfbfwma;
    Riscv_ext_v.zvkb;
    Riscv_ext_v.zvknhb;
    Riscv_ext_v.zvkn;
    Riscv_ext_v.zvks;
    Riscv_ext_v.zvknc;
    Riscv_ext_v.zvkng;
    Riscv_ext_v.zvksc;
    Riscv_ext_v.zvksg;
    Riscv_ext_c.zca;
    Riscv_ext_c.zcd;
    Riscv_ext_c.zcf;
    Riscv_ext_c.zcb;
    Riscv_ext_c.zcmp;
    Riscv_ext_c.zcmt;
    Riscv_ext_c.zcmop;
    Riscv_ext_c.c;
  ]

let standard ~xlen =
  {
    Validated.xlen;
    components = List.map (Target_component.specialize ~xlen) standard_components;
    rows = Riscv_table_rows.rows;
  }

let overlap (a : Riscv_table_row.row) (b : Riscv_table_row.row) =
  let common = Int64.logand a.mask b.mask in
  Int64.equal (Int64.logand a.match_ common) (Int64.logand b.match_ common)
  && (a.xlen = 0 || b.xlen = 0 || a.xlen = b.xlen)

(* Words of a row's encoding space for the standard decoder to decline: the free bits clear, all
   set, and each set alone. The hand-written forms are code rather than masks, so these witnesses
   stand in for an exact overlap check with them. *)
let witnesses (r : Riscv_table_row.row) =
  let free = Int64.logand (Int64.lognot r.mask) 0xffffffffL in
  let single =
    List.filter_map
      (fun b ->
        let bit = Int64.shift_left 1L b in
        if Int64.equal (Int64.logand free bit) 0L then None else Some (Int64.logor r.match_ bit))
      (List.init 32 Fun.id)
  in
  r.match_ :: Int64.logor r.match_ free :: single

let row_problems ~handwritten ~standard_reads ~standard_rows ~vendor_features
    (r : Riscv_table_row.row) =
  let say fmt = Printf.ksprintf (fun m -> Printf.sprintf "row %s: %s" r.mnemonic m) fmt in
  let wide =
    Int64.logand r.match_ 3L = 3L && Int64.equal r.mask (Int64.logand r.mask 0xffffffffL)
  in
  let within = Int64.equal (Int64.logand r.match_ r.mask) r.match_ in
  (if List.mem r.mnemonic handwritten then [ say "collides with a hand-written form" ] else [])
  @ (if
       Array.exists
         (fun (s : Riscv_table_row.row) -> String.equal s.mnemonic r.mnemonic)
         Riscv_table_rows.rows
     then [ say "collides with a standard generated form" ]
     else [])
  @ (if List.mem r.feature vendor_features then []
     else [ say "feature %s is not a vendor component" r.feature ])
  @ (if wide then [] else [ say "must be a 32-bit instruction" ])
  @ (if within then [] else [ say "match has bits outside the mask" ])
  @
  if not (wide && within) then []
  else
    match
      List.filter_map
        (fun (s : Riscv_table_row.row) ->
          if overlap r s then Some (say "overlaps standard form %s" s.mnemonic) else None)
        (Array.to_list standard_rows)
    with
    | _ :: _ as overlaps -> overlaps
    | [] -> (
        match
          List.find_map
            (fun w -> Option.map (fun name -> (w, name)) (standard_reads w))
            (witnesses r)
        with
        | Some (w, name) -> [ say "the standard decoder reads 0x%08Lx as %s" w name ]
        | None -> [])

let make ~handwritten ~standard_reads ~xlen (def : definition) =
  let vendor = List.map (Target_component.specialize ~xlen) def.components in
  let vendor_features = List.map (fun (c : Target_component.t) -> c.feature) vendor in
  let standard = (standard ~xlen).components in
  let components = standard @ vendor in
  let component_problems =
    List.concat_map
      (fun (c : Target_component.t) ->
        if c.default_on then
          [ Printf.sprintf "%s: a vendor component must not be on by default" c.id ]
        else [])
      vendor
    @ Target_component.check components
  in
  let rows = def.rows in
  let standard_rows =
    Array.of_list
      (List.filter
         (fun (s : Riscv_table_row.row) -> s.xlen = 0 || s.xlen = xlen)
         (Array.to_list Riscv_table_rows.rows))
  in
  let row_problems =
    List.concat_map
      (row_problems ~handwritten ~standard_reads ~standard_rows ~vendor_features)
      (Array.to_list rows)
  in
  let overlaps =
    List.concat
      (List.init (Array.length rows) (fun i ->
           List.filter_map
             (fun j ->
               if j > i && overlap rows.(i) rows.(j) then
                 Some (Printf.sprintf "rows %s and %s overlap" rows.(i).mnemonic rows.(j).mnemonic)
               else None)
             (List.init (Array.length rows) Fun.id)))
  in
  let unowned =
    Array.to_list rows
    |> List.filter_map (fun (r : Riscv_table_row.row) ->
        if List.exists (fun c -> Target_component.owns_mnemonic c r.mnemonic) vendor then None
        else Some (Printf.sprintf "row %s: no vendor component lists it" r.mnemonic))
  in
  match component_problems @ row_problems @ overlaps @ unowned with
  | [] -> Ok { Validated.xlen; components; rows = Array.append Riscv_table_rows.rows rows }
  | problems -> Error problems
