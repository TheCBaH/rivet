(* Shared, pure multi-segment fixture and rendering logic (docs/design.md
   M5's real-browser harness). Extracted out of manifest_dump.ml, which used
   to hold this as a private module-level binding and print it directly, so
   that manifest_dump.ml and the browser smoke harness
   (test/browser/smoke.ml) both consume the exact same source and
   rendering rather than one copying the other and silently drifting from
   it. manifest_dump.ml's own committed cram baseline is the regression
   check that this extraction changed nothing about its output. *)

let multi_segment_source =
  {|	.text
	.globl start
start:
	movl $1, %eax
	ret
	.data
g:
	.long 42
	.bss
b:
	.zero 8
|}

let summarize (m : Rivet_driver.Portable.manifest) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf
    (Printf.sprintf "target=%s entry=%s\n" m.Rivet_driver.Portable.target
       (match m.Rivet_driver.Portable.entry with
       | Some e -> Printf.sprintf "0x%Lx" e
       | None -> "none"));
  Buffer.add_string buf
    (Printf.sprintf "exports: %s\n"
       (match m.Rivet_driver.Portable.exports with
       | [] -> "none"
       | l -> String.concat ", " (List.map (fun (n, a) -> Printf.sprintf "%s=0x%Lx" n a) l)));
  List.iter
    (fun (s : Rivet_driver.Portable.manifest_segment) ->
      Buffer.add_string buf
        (Printf.sprintf "%-6s address=0x%-10Lx bytes=%d %s\n" s.Rivet_driver.Portable.name
           s.Rivet_driver.Portable.address
           (String.length s.Rivet_driver.Portable.bytes)
           (Rivet_asm_core.Perms.to_string s.Rivet_driver.Portable.perms)))
    m.Rivet_driver.Portable.segments;
  Buffer.contents buf
