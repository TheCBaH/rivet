let targets = [ "x86_32"; "x86_64"; "arm"; "aarch64"; "riscv32"; "riscv64" ]

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

(* Assemble [src] for [target], bind every segment at its own 16 MiB slot, and
   return the bound segments or the rendered diagnostics. *)
let assemble target src =
  match Driver.Registry.find target with
  | None -> failwith target
  | Some (module D : Target_intf.Target.DRIVER) -> (
      let source = Foundation.Span.source ~name:(target ^ ".s") ~contents:src in
      match D.assemble ~unit_name:"t" ~source () with
      | Error ds -> Error (String.trim (Foundation.Diag.render ds))
      | Ok laid_out -> (
          let plan = Image.plan_of laid_out in
          let addresses =
            List.mapi
              (fun i (s : Image.segment_plan) ->
                (s.Image.seg_name, Int64.of_int ((i + 1) * 0x1000000)))
              plan.Image.segments
          in
          match Image.bind_image laid_out ~addresses with
          | Error ds -> Error (String.trim (Foundation.Diag.render ds))
          | Ok img -> Ok img.Image.segments))

let segment name segs = List.find (fun (s : Image.segment) -> s.Image.name = name) segs

let show_data src =
  List.iter
    (fun t ->
      Printf.printf "%-8s " t;
      match assemble t src with
      | Ok segs -> print_endline (hex (segment ".data" segs).Image.bytes)
      | Error m -> print_endline ("error: " ^ List.hd (String.split_on_char '\n' m)))
    targets

let%expect_test ".align 0 follows each dialect" =
  show_data ".data\n.byte 1\n.align 0\n.byte 0xaa\n";
  [%expect
    {|
    x86_32   01aa
    x86_64   01aa
    arm      01000000aa
    aarch64  01aa
    riscv32  01aa
    riscv64  01aa |}]

let%expect_test ".align 3 is a byte count on x86 and an exponent elsewhere" =
  show_data ".data\n.byte 1\n.align 3\n.byte 0xaa\n";
  [%expect
    {|
    x86_32   error: x86_32.s:3:1: error[simplify.directive]: .align: the alignment is not a power of two
    x86_64   error: x86_64.s:3:1: error[simplify.directive]: .align: the alignment is not a power of two
    arm      0100000000000000aa
    aarch64  0100000000000000aa
    riscv32  0100000000000000aa
    riscv64  0100000000000000aa |}]

let%expect_test ".align 2 and .align 1" =
  show_data ".data\n.byte 1\n.align 2\n.byte 0xaa\n";
  show_data ".data\n.byte 1\n.align 1\n.byte 0xaa\n";
  [%expect
    {|
    x86_32   0100aa
    x86_64   0100aa
    arm      01000000aa
    aarch64  01000000aa
    riscv32  01000000aa
    riscv64  01000000aa
    x86_32   01aa
    x86_64   01aa
    arm      0100aa
    aarch64  0100aa
    riscv32  0100aa
    riscv64  0100aa |}]

let%expect_test ".balign rejects what is not a power of two" =
  show_data ".data\n.byte 1\n.balign 3\n.byte 0xaa\n";
  show_data ".data\n.byte 1\n.balign 0\n.byte 0xaa\n";
  show_data ".data\n.byte 1\n.balign 4\n.byte 0xaa\n";
  [%expect
    {|
    x86_32   error: x86_32.s:3:1: error[simplify.directive]: .balign: the alignment is not a power of two
    x86_64   error: x86_64.s:3:1: error[simplify.directive]: .balign: the alignment is not a power of two
    arm      error: arm.s:3:1: error[simplify.directive]: .balign: the alignment is not a power of two
    aarch64  error: aarch64.s:3:1: error[simplify.directive]: .balign: the alignment is not a power of two
    riscv32  error: riscv32.s:3:1: error[simplify.directive]: .balign: the alignment is not a power of two
    riscv64  error: riscv64.s:3:1: error[simplify.directive]: .balign: the alignment is not a power of two
    x86_32   01aa
    x86_64   01aa
    arm      01aa
    aarch64  01aa
    riscv32  01aa
    riscv64  01aa
    x86_32   01000000aa
    x86_64   01000000aa
    arm      01000000aa
    aarch64  01000000aa
    riscv32  01000000aa
    riscv64  01000000aa |}]

let%expect_test ".align 3 puts a .quad on an 8-byte boundary" =
  show_data ".data\n.byte 1\n.align 3\n.quad 2\n";
  [%expect
    {|
    x86_32   error: x86_32.s:3:1: error[simplify.directive]: .align: the alignment is not a power of two
    x86_64   error: x86_64.s:3:1: error[simplify.directive]: .align: the alignment is not a power of two
    arm      01000000000000000200000000000000
    aarch64  01000000000000000200000000000000
    riscv32  01000000000000000200000000000000
    riscv64  01000000000000000200000000000000 |}]

(* {1 Sections: flags, type, subsections} *)

let first_error src t =
  match assemble t src with Ok _ -> "accepted" | Error m -> List.hd (String.split_on_char '\n' m)

let show_segments src =
  List.iter
    (fun t ->
      Printf.printf "%s:" t;
      (match assemble t src with
      | Ok segs ->
          List.iter
            (fun (s : Image.segment) ->
              Printf.printf " %s %s a%d %s+%d" s.Image.name
                (Asm_core.Perms.to_string s.Image.perms)
                s.Image.alignment (hex s.Image.bytes) s.Image.zero_fill)
            segs
      | Error m -> print_string (" error: " ^ List.hd (String.split_on_char '\n' m)));
      print_newline ())
    [ "x86_64"; "aarch64" ]

let%expect_test "permissions come from the flag string" =
  show_segments
    ".section .text.startup,\"ax\",@progbits\n\
     .byte 1\n\
     .section .mydata,\"aw\",@progbits\n\
     .byte 2\n\
     .section .rodata,\"aw\"\n\
     .byte 3\n\
     .section .xyz,\"a\"\n\
     .byte 4\n";
  [%expect
    {|
    x86_64: .text.startup r-x a1 01+0 .mydata rw- a1 02+0 .rodata rw- a1 03+0 .xyz r-- a1 04+0
    aarch64: .text.startup r-x a1 01+0 .mydata rw- a1 02+0 .rodata rw- a1 03+0 .xyz r-- a1 04+0 |}]

let%expect_test "no flags: the name decides" =
  show_segments ".section .rodata\n.byte 1\n.section .foo\n.byte 2\n";
  [%expect
    {|
    x86_64: .rodata r-- a1 01+0 .foo rw- a1 02+0
    aarch64: .rodata r-- a1 01+0 .foo rw- a1 02+0 |}]

let%expect_test "an executable custom section is NOP-filled" =
  show_segments ".section .text.hot,\"ax\",@progbits\n.byte 1\n.balign 4\n.byte 2\n";
  [%expect
    {|
    x86_64: .text.hot r-x a4 010f1f0002+0
    aarch64: error: aarch64.s:3:1: error[image.align-fill]: the target supplies no 3-byte padding for a 4-byte alignment |}]

let%expect_test "NOBITS is inferred from a .bss or .tbss name when no type is given" =
  show_segments
    ".section .bss.x\n\
     .zero 4\n\
     .section .tbss.y\n\
     .zero 8\n\
     .section .bss.z,\"aw\",@progbits\n\
     .zero 2\n";
  [%expect
    {|
    x86_64: .bss.x rw- a1 +4 .tbss.y rw- a1 +8 .bss.z rw- a1 0000+0
    aarch64: .bss.x rw- a1 +4 .tbss.y rw- a1 +8 .bss.z rw- a1 0000+0 |}]

let%expect_test "subsection and .bss arguments are rejected" =
  List.iter
    (fun d -> Printf.printf "%-8s %s\n" d (first_error (d ^ "\n.byte 1\n") "x86_64"))
    [ ".text 1"; ".data 2"; ".bss 1"; ".text 0" ];
  [%expect
    {|
    .text 1  x86_64.s:1:1: error[simplify.directive]: .text: a subsection argument is not supported
    .data 2  x86_64.s:1:1: error[simplify.directive]: .data: a subsection argument is not supported
    .bss 1   x86_64.s:1:1: error[simplify.directive]: .bss: a subsection argument is not supported
    .text 0  x86_64.s:1:1: error[simplify.directive]: .text: a subsection argument is not supported |}]

let%expect_test "re-entering a section with other attributes is diagnosed" =
  List.iter
    (fun src -> print_endline (first_error src "x86_64"))
    [
      ".section .foo,\"aw\"\n.byte 1\n.section .foo,\"ax\"\n.byte 2\n";
      ".section .foo,\"aw\",@progbits\n.byte 1\n.section .foo,\"aw\",@nobits\n.byte 2\n";
      ".section .foo,\"aw\"\n.byte 1\n.section .foo,\"aw\"\n.byte 2\n";
      ".data\n.byte 1\n.section .data,\"aw\"\n.byte 2\n";
    ];
  [%expect
    {|
    x86_64.s:3:1: error[lower.section-attributes]: section .foo is re-entered with different attributes
    x86_64.s:3:1: error[lower.section-attributes]: section .foo is re-entered with different attributes
    accepted
    accepted |}]

let%expect_test ".comm alignment must be a power of two" =
  List.iter
    (fun src -> print_endline (first_error src "x86_64"))
    [ ".comm x,8,3\n"; ".comm x,8,8\n"; ".comm x,8,0\n" ];
  [%expect
    {|
    x86_64.s:1:1: error[simplify.directive]: .comm needs a non-negative size and a power-of-two alignment
    accepted
    x86_64.s:1:1: error[simplify.directive]: .comm needs a non-negative size and a power-of-two alignment |}]

let%expect_test "a large alignment costs the padding, not a table of every gap" =
  List.iter
    (fun (t, src) ->
      match assemble t src with
      | Error m -> print_endline m
      | Ok segs ->
          let b = (segment ".text" segs).Image.bytes in
          Printf.printf "%s: %d bytes, starts %s\n" t (String.length b) (hex (String.sub b 0 8)))
    [
      ("x86_64", ".text\nnop\n.p2align 20\nret\n");
      ("aarch64", ".text\n.long 0\n.p2align 20\n.long 1\n");
    ];
  [%expect
    {|
    x86_64: 1048577 bytes, starts 9066662e0f1f8400
    aarch64: 1048580 bytes, starts 000000001f2003d5 |}]

let%expect_test "a value wider than its directive is an error, not a truncation" =
  List.iter
    (fun d ->
      Printf.printf "%-28s " d;
      match assemble "x86_64" (".data\n" ^ d ^ "\n") with
      | Ok segs -> print_endline (hex (segment ".data" segs).Image.bytes)
      | Error m -> print_endline (List.hd (String.split_on_char '\n' m)))
    [
      ".byte 0x1234";
      ".byte 256";
      ".byte 255";
      ".byte -128";
      ".byte -129";
      ".short 0x10000";
      ".short -32768";
      ".long 0x100000000";
      ".long 0xffffffff";
      ".quad 0xffffffffffffffff";
      ".byte 0xffffffffffffffff";
    ];
  [%expect
    {|
    .byte 0x1234                 x86_64.s:2:1: error[lower.data]: value 4660 does not fit in 1 byte
    .byte 256                    x86_64.s:2:1: error[lower.data]: value 256 does not fit in 1 byte
    .byte 255                    ff
    .byte -128                   80
    .byte -129                   x86_64.s:2:1: error[lower.data]: value -129 does not fit in 1 byte
    .short 0x10000               x86_64.s:2:1: error[lower.data]: value 65536 does not fit in 2 bytes
    .short -32768                0080
    .long 0x100000000            x86_64.s:2:1: error[lower.data]: value 4294967296 does not fit in 4 bytes
    .long 0xffffffff             ffffffff
    .quad 0xffffffffffffffff     ffffffffffffffff
    .byte 0xffffffffffffffff     x86_64.s:2:1: error[lower.data]: value 18446744073709551615 does not fit in 1 byte |}]

let%expect_test "symbol assignment says it is unsupported" =
  print_endline (first_error ".text\nx = 1\n" "x86_64");
  [%expect
    {| x86_64.s:2:1: error[simplify.assignment]: symbol assignment (x = ...) is not supported |}]

(* {1 Addresses at or above 2^63} *)

let assemble_laid target src =
  match Driver.Registry.find target with
  | None -> failwith target
  | Some (module D : Target_intf.Target.DRIVER) -> (
      let source = Foundation.Span.source ~name:(target ^ ".s") ~contents:src in
      match D.assemble ~unit_name:"t" ~source () with
      | Ok l -> l
      | Error ds -> failwith (Foundation.Diag.render ds))

let first_line m = List.hd (String.split_on_char '\n' m)

let bind_show target src addresses =
  let l = assemble_laid target src in
  match Image.bind_image l ~addresses with
  | Error ds ->
      Printf.printf "%s: %s\n" target (first_line (String.trim (Foundation.Diag.render ds)))
  | Ok img ->
      List.iter
        (fun (s : Image.segment) ->
          Printf.printf "%s: %s at %Lx: %s\n" target s.Image.name s.Image.address
            (let h = hex s.Image.bytes in
             if String.length h > 40 then
               Printf.sprintf "%s... (%d bytes)" (String.sub h 0 32) (String.length s.Image.bytes)
             else h))
        img.Image.segments;
      List.iter (fun (n, a) -> Printf.printf "  export %s = %Lx\n" n a) img.Image.exports

let high_program = function
  | "x86_64" ->
      ".text\n.globl entry\nentry:\n\tcall f\n\tret\nf:\n\tret\n.data\n.quad f\n.quad f - entry\n"
  | "aarch64" ->
      ".text\n.globl entry\nentry:\n\tbl f\n\tret\nf:\n\tret\n.data\n.quad f\n.quad f - entry\n"
  | _ ->
      ".text\n\
       .globl entry\n\
       entry:\n\
       \tjal ra, f\n\
       \tret\n\
       f:\n\
       \tret\n\
       .data\n\
       .quad f\n\
       .quad f - entry\n"

let%expect_test "a program binds at 0xffff800000000000, code and data alike" =
  List.iter
    (fun t ->
      bind_show t (high_program t)
        [ (".text", 0xffff800000000000L); (".data", 0xffff800000100000L) ])
    [ "x86_64"; "aarch64"; "riscv64" ];
  [%expect
    {|
    x86_64: .text at ffff800000000000: e801000000c3c3
    x86_64: .data at ffff800000100000: 060000000080ffff0600000000000000
      export entry = ffff800000000000
    aarch64: .text at ffff800000000000: 02000094c0035fd6c0035fd6
    aarch64: .data at ffff800000100000: 080000000080ffff0800000000000000
      export entry = ffff800000000000
    riscv64: .text at ffff800000000000: ef0080006780000067800000
    riscv64: .data at ffff800000100000: 080000000080ffff0800000000000000
      export entry = ffff800000000000 |}]

let%expect_test "the last page below 2^64 holds a segment that ends at 2^64, and no more" =
  let page n = ".data\n.quad 7\n.zero " ^ string_of_int n ^ "\n" in
  bind_show "x86_64" (page 4088) [ (".data", 0xfffffffffffff000L) ];
  bind_show "x86_64" (page 4089) [ (".data", 0xfffffffffffff000L) ];
  [%expect
    {|
    x86_64: .data at fffffffffffff000: 07000000000000000000000000000000... (4096 bytes)
    x86_64: <synthesized by image>: error[bind.overflow]: segment .data at 0xfffffffffffff000 overflows with size 4097 |}]

let%expect_test "alignment and overlap are decided on unsigned addresses" =
  let src = ".text\n.p2align 12\nnop\n.data\n.p2align 4\n.byte 1\n" in
  bind_show "x86_64" src [ (".text", 0xffff800000000800L); (".data", 0xffff800000100000L) ];
  bind_show "x86_64" src [ (".text", 0xffff800000000000L); (".data", 0xffff800000000000L) ];
  bind_show "x86_64" src [ (".text", 0x7ffffffffffff000L); (".data", 0x8000000000000000L) ];
  [%expect
    {|
    x86_64: <synthesized by image>: error[bind.alignment]: segment .text needs 4096-byte alignment, address is 0xffff800000000800
    x86_64: <synthesized by image>: error[bind.overlap]: segments .text at 0xffff800000000000 (1 bytes) and .data at 0xffff800000000000 (1 bytes) overlap
    x86_64: .text at 7ffffffffffff000: 90
    x86_64: .data at 8000000000000000: 01 |}]

let%expect_test "a branch across the end of the address space is an error, not a short jump" =
  let src = ".text\n.globl entry\nentry:\n\tcall f\n.data\nf:\n\t.byte 1\n" in
  bind_show "x86_64" src [ (".text", 0xfffffffffffff000L); (".data", 0x1000L) ];
  bind_show "x86_64" src [ (".text", 0xfffffffffffff000L); (".data", 0xffffffffffff0000L) ];
  [%expect
    {|
    x86_64: <synthesized by image>: error[bind.fixup]: fixup target: the displacement wraps around the end of the address space
    x86_64: .text at fffffffffffff000: e8fb0fffff
    x86_64: .data at ffffffffffff0000: 01
      export entry = fffffffffffff000 |}]
