# Printing and disassembling from another program

For a program that builds modules as values and wants their GNU source, a
disassembly of the bytes they assemble to, or a listing of a bound image -
for instance an in-browser view running under js_of_ocaml.

## GNU source

`Rivet_asm_core.Gnu_module.pp syntax ~instruction` with the target's
`Instruction.pp_gnu`; the contract is in `docs/architecture.md` ("GNU source
export"). `syntax.type_char` is `'@'` on x86 and RISC-V, `'%'` on AArch64.

## Disassembly

Every `Target.DRIVER`, and `Driver_direct.Pipeline_direct.Make (T)` for any
`ENCODE` `T`, has:

- `dump_disasm_canonical ?features ~address bytes`: one instruction per line,
  re-parseable to the same bytes;
- `dump_disasm_diagnostic ?features ?inspect ~address bytes`: address, bytes,
  text and form id in columns, for reading;
- `disassemble_records ?features ?inspect ~address bytes`: what both are
  rendered from, one `Target.disasm_record` per instruction or padding run
  (`address`, raw `bytes`, canonical `text`, `form` - `None` for padding -
  and, under `~inspect:true`, `refusal` and `alternatives`).

`Driver.Portable.disassemble_records_for target` is the same for a target
named by string.

## A vendor extension

A RISC-V extension defined outside Rivet (rows and components, validated by
`Riscv_family_encode.Extensions.make`) gets the whole driver, both
disassemblies included, by applying the functors:

```ocaml
module C =
  Riscv_family.Make_with
    (Riscv32_encode.Profile)
    (struct
      let definition = validated
    end)

module D = Pipeline.Make (C)   (* or Driver_direct.Pipeline_direct.Make (C) *)

let text =
  match Target_config.parse_spec "+xfoo" with
  | Ok features -> D.dump_disasm_canonical ~features ~address bytes
  | Error m -> failwith m
```

Its rows decode with operands and print with `C.Instruction.pp_gnu`; a row
of a disabled vendor component is a decode error, or a record with a
`refusal` under `~inspect:true`. `test/targets/test_vendor.ml` and
`test/gnu_text` are worked examples.

## Listings

`Image.bind_image` records every symbol's address (`Image.symbols`, locals
included) and, for each fixup naming one symbol, the patched field's address
and the addend (`Image.references`). `listing_records ?features image
~segment` disassembles one segment of the image with the symbols defined at
each record (`labels`) and the symbols its fixups name (`targets`);
`dump_listing` renders it:

```
_start:
  00001010  97 00 00 00  auipc x1, 0        <exp>
  00001014  e7 80 c0 00  jalr x1, 12(x1)    <exp>
```

`Driver.Portable.listing bound ~segment` is the same for a `Portable.bound`.

## The smallest closure

`rivet.driver` links all six targets through the registry; `Portable` needs
it. A program that prints and disassembles known targets can apply the
functors itself instead:

| Closure | Libraries |
| --- | --- |
| Through `Portable` | `rivet.driver` (every target) |
| Text pipeline | `rivet.text_pipeline` and each target's front end (`rivet.x86_64`, ...) |
| Direct pipeline | `rivet.driver_direct` and each target's encoder (`rivet.x86_64_encode`, ...) |

The direct pipeline has no lexer or parser in its closure (the layer audit
checks it). `scripts/jsoo-size.sh` builds one print-and-disassemble program
per closure with js_of_ocaml in the release profile and reports their sizes:

| Program (prints and disassembles) | Bytes | gzip -9 |
| --- | ---: | ---: |
| x86-64, AArch64, RV32 through `Portable` | 2,389,645 | 430,179 |
| the same through the text pipeline | 2,216,101 | 379,062 |
| the same through the direct pipeline | 2,134,048 | 363,211 |
| AArch64 and RV32 through the direct pipeline | 550,961 | 156,787 |

(js_of_ocaml 6.4.1, OCaml 4.14.3, measured 2026-11-20.)

x86-64 alone is about 1.6 MB of the direct closure (its encoder's generated
row tables are 6.5 MB of OCaml source); the lexer and parser cost about 80 kB,
`Portable` and the three targets it adds about 170 kB.
