(* Shared, pure multi-segment fixture and rendering logic (.ai/asm_plan.md
   M5's real-browser harness). Extracted out of manifest_dump.ml, which used
   to hold this as a private module-level binding and print it directly, so
   that manifest_dump.ml and the browser smoke harness
   (asm/test/browser/smoke.ml) both consume the exact same source and
   rendering rather than one copying the other and silently drifting from
   it. manifest_dump.ml's own committed cram baseline is the regression
   check that this extraction changed nothing about its output. *)

let multi_segment_source =
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

let summarize (m : Driver.Portable.manifest) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf
    (Printf.sprintf "target=%s entry=%s\n" m.Driver.Portable.target
       (match m.Driver.Portable.entry with Some e -> Printf.sprintf "0x%Lx" e | None -> "none"));
  Buffer.add_string buf
    (Printf.sprintf "exports: %s\n"
       (match m.Driver.Portable.exports with
       | [] -> "none"
       | l -> String.concat ", " (List.map (fun (n, a) -> Printf.sprintf "%s=0x%Lx" n a) l)));
  List.iter
    (fun (s : Driver.Portable.manifest_segment) ->
      Buffer.add_string buf
        (Printf.sprintf "%-6s address=0x%-10Lx bytes=%d %s\n" s.Driver.Portable.name
           s.Driver.Portable.address
           (String.length s.Driver.Portable.bytes)
           (Asm_core.Perms.to_string s.Driver.Portable.perms)))
    m.Driver.Portable.segments;
  Buffer.contents buf
