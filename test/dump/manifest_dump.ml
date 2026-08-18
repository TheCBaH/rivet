(* The three-build equality driver for Driver.Portable.manifest (.ai/asm_plan.md
   M4 Phase 8). Same shape as asm_dump.ml - one source, compiled native,
   js_of_ocaml, and Melange, with one committed cram baseline the three builds
   are checked against.

   A dedicated multi-segment source rather than Test_dump_inputs.all: none of
   those six is more than one .text segment, and the property worth proving
   three ways is [manifest]'s own one - a NOBITS segment's [zero_fill]
   materialized into real bytes (driver/portable.ml:190-197) - which needs a
   real code/data/BSS image to exercise at all. *)

let text =
  "\t.text\n\
   \t.globl start\n\
   start:\n\
   \tmovl $1, %eax\n\
   \tret\n\
   \t.data\n\
   g:\n\
   \t.long 42\n\
   \t.bss\n\
   b:\n\
   \t.zero 8\n"

let ok = function
  | Ok v -> v
  | Error e ->
      print_string (Driver.Portable.render_error (Err.Error.kind e));
      exit 1

let () =
  let p = ok (Driver.Portable.assemble ~entry:"start" "x86_64" ~unit_name:"m" ~text) in
  let b = ok (Driver.Portable.bind_sequential p ~base:0x400000L ~gap:0) in
  let m = Driver.Portable.manifest b in
  Printf.printf "target=%s entry=%s\n" m.Driver.Portable.target
    (match m.Driver.Portable.entry with Some e -> Printf.sprintf "0x%Lx" e | None -> "none");
  Printf.printf "exports: %s\n"
    (match m.Driver.Portable.exports with
    | [] -> "none"
    | l -> String.concat ", " (List.map (fun (n, a) -> Printf.sprintf "%s=0x%Lx" n a) l));
  List.iter
    (fun (s : Driver.Portable.manifest_segment) ->
      Printf.printf "%-6s address=0x%-10Lx bytes=%d %s\n" s.Driver.Portable.name
        s.Driver.Portable.address
        (String.length s.Driver.Portable.bytes)
        (Asm_core.Perms.to_string s.Driver.Portable.perms))
    m.Driver.Portable.segments
