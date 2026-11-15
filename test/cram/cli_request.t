The CLI passes the planning and binding options through to the engine, and
links several input files.

  $ asm() { ../../tool/asm.exe "$@"; }
  $ printf '\t.text\n\t.globl f\nf:\n\tcall ext\n\tret\n' > a.s
  $ printf '\t.data\n\t.globl d\nd:\n\t.long 5\n' > b.s

An extern may stay undefined in the plan; binding it needs its address.

  $ asm --target x86_64 --entry f --extern ext a.s
  segment .text size=6 zero=0 align=1 permissions=r-x
  entry f
  export f
  import ext
  unresolved target
  $ asm --target x86_64 --entry f --extern ext --import ext=0x1100 --fixed-base 0x1000 a.s
  section .text address=0x1000 size=6 permissions=r-x
  entry 0x1000
  export f = 0x1000

Two inputs, each segment at its own address, and an explicit export list.

  $ asm --target x86_64 --entry f --extern ext --import ext=0x1100 --fixed-base 0x1000 \
  >   --at .data=0x2000 --export f a.s b.s
  section .text address=0x1000 size=6 permissions=r-x
  section .data address=0x2000 size=4 permissions=rw-
  entry 0x1000
  export f = 0x1000

Segments overlap unless they are in different overlay groups.

  $ asm --target x86_64 --entry f --extern ext --import ext=0x1100 --fixed-base 0x1000 a.s b.s
  <synthesized by image>: error[bind.overlap]: segments .text at 0x1000 (6 bytes) and .data at 0x1000 (4 bytes) overlap
  [1]
  $ asm --target x86_64 --entry f --extern ext --import ext=0x1100 --fixed-base 0x1000 \
  >   --overlay .text --overlay .data --load-address .text=0x8000 --load-address .data=0x8010 a.s b.s
  section .text address=0x1000 load=0x8000 size=6 permissions=r-x
  section .data address=0x1000 load=0x8010 size=4 permissions=rw-
  entry 0x1000
  export f = 0x1000
  export d = 0x1000

A size limit.

  $ asm --target x86_64 --entry f --extern ext --max-size 2 a.s
  <synthesized by image>: error[image.too-large]: image is 6 bytes, limit is 2
  [1]
