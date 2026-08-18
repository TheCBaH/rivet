let addrs target =
  let c = Target.config target in
  ( Target.hex c.Target.link.Target.text,
    Target.hex c.Target.link.Target.rodata,
    Target.hex c.Target.link.Target.data,
    Target.hex c.Target.link.Target.bss )

let oracle target =
  let text, rodata, data, bss = addrs target in
  Printf.sprintf
    "ENTRY(asm_test_entry)\n\
     SECTIONS\n\
     {\n\
    \  . = %s;\n\
    \  .text : { *(.text) }\n\
    \  . = %s;\n\
    \  .rodata : { *(.rodata .rodata.*) }\n\
    \  . = %s;\n\
    \  .data : { *(.data) }\n\
    \  . = %s;\n\
    \  .bss : { *(.bss) }\n\
    \  /DISCARD/ : { *(.eh_frame) *(.note.GNU-stack) *(.comment) *(.ARM.attributes) }\n\
     }\n"
    text rodata data bss

let exec target ~objs =
  let text, rodata, data, bss = addrs target in
  let c = Target.config target in
  (* The stub sits one 64 KiB step BELOW .text, so it cannot collide with the
     fixture's own placement however large .text grows. *)
  let start_addr = Target.hex (c.Target.link.Target.text - 0x10000) in
  (* PER-OBJECT input selection, for every one of the fixture's own N objects
     - not a wildcard, so a stray object placed in the same work directory
     could never silently join the link. *)
  let select suffix = String.concat " " (List.map (fun o -> o ^ suffix) objs) in
  Printf.sprintf
    "ENTRY(_start)\n\
     SECTIONS\n\
     {\n\
    \  . = %s;\n\
    \  .start : { start.o(.text.startup) }\n\
    \  . = %s;\n\
    \  .text : { %s }\n\
    \  . = %s;\n\
    \  .rodata : { %s }\n\
    \  . = %s;\n\
    \  .data : { %s }\n\
    \  . = %s;\n\
    \  .bss : { %s }\n\
    \  /DISCARD/ : {\n\
    \    *(.eh_frame) *(.note.GNU-stack) *(.comment)\n\
    \    *(.ARM.attributes) *(.ARM.exidx) *(.ARM.extab)\n\
    \    *(.riscv.attributes)\n\
    \  }\n\
     }\n"
    start_addr text (select "(.text)") rodata (select "(.rodata .rodata.*)") data (select "(.data)")
    bss (select "(.bss)")
