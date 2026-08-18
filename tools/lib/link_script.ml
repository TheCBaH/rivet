let addrs target =
  let c = Target.config target in
  ( Target.hex c.Target.link.Target.text,
    Target.hex c.Target.link.Target.rodata,
    Target.hex c.Target.link.Target.data )

let oracle target =
  let text, rodata, data = addrs target in
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
    \  /DISCARD/ : { *(.eh_frame) *(.note.GNU-stack) *(.comment) *(.ARM.attributes) }\n\
     }\n"
    text rodata data

let exec target =
  let text, rodata, data = addrs target in
  let c = Target.config target in
  (* The stub sits one 64 KiB step BELOW .text, so it cannot collide with the
     fixture's own placement however large .text grows. *)
  let start_addr = Target.hex (c.Target.link.Target.text - 0x10000) in
  Printf.sprintf
    "ENTRY(_start)\n\
     SECTIONS\n\
     {\n\
    \  . = %s;\n\
    \  .start : { start.o(.text.startup) }\n\
    \  . = %s;\n\
    \  .text : { fixture.o(.text) }\n\
    \  . = %s;\n\
    \  .rodata : { fixture.o(.rodata .rodata.*) }\n\
    \  . = %s;\n\
    \  .data : { fixture.o(.data) }\n\
    \  /DISCARD/ : {\n\
    \    *(.eh_frame) *(.note.GNU-stack) *(.comment)\n\
    \    *(.ARM.attributes) *(.ARM.exidx) *(.ARM.extab)\n\
    \    *(.riscv.attributes)\n\
    \  }\n\
     }\n"
    start_addr text rodata data
