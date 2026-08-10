(* The linker: relaxation, fixup patching, and multi-segment binding.
   (.ai/asm_plan.md §8, §9; M2.C and M2.F0.)

   No target is linked. [Image] is parameterized over the fixup-kind type, so a
   locally declared kind exercises every path here without borrowing an
   architecture's vocabulary - and, more usefully, without an instruction
   encoder standing between the test and the thing under test. When a branch
   slice later lands, these stay as the tests that fail for a layout reason
   rather than for an encoding one.

   The synthetic kinds are modelled on x86's short and near branches, because
   that pair is why relaxation exists at all: GNU picks the two-byte encoding
   for a nearby branch, so an assembler that always emitted the long form would
   be correct and would still fail a byte-for-byte comparison. *)

open Foundation
open Asm_core

type kind = Rel8 | Rel32 | Abs32

let kind_name = function Rel8 -> "rel8" | Rel32 -> "rel32" | Abs32 -> "abs32"

(* [place] arrives already biased, so these do no PC arithmetic of their own -
   the bias is a property of the encoded form and travels in the fixup. *)
let evaluate kind ~place ~target =
  let d = Int64.sub target place in
  match kind with Rel8 | Rel32 -> Ok d | Abs32 -> Ok (Int64.logand target 0xFFFFFFFFL)

let origin = Origin.synthesized ~pass:"test" ()

let fixup ?(name = "target") ?(byte_offset = 1) ?(container = 4) ?(pc_bias = 5) ~kind ~range ~slices
    value =
  {
    Lowered_ast.kind;
    kind_name = kind_name kind;
    family = (match kind with Abs32 -> "abs" | _ -> "pcrel-branch");
    role = (match kind with Abs32 -> Lowered_ast.Data_address | _ -> Lowered_ast.Branch);
    name;
    slices;
    byte_offset;
    container;
    pc_bias;
    range;
    value;
    origin;
  }

let whole_container ~bits = [ { Lowered_ast.bit_offset = 0; bit_width = bits; value_lsb = 0 } ]

(* A two-rung ladder over one symbol, shaped like `jmp`: opcode byte then
   displacement, two bytes short and five bytes near. *)
let branch_to_expr e =
  Lowered_ast.Relax
    {
      alts =
        [
          {
            Lowered_ast.bytes = "\xeb\x00";
            form = "t.rel8";
            fixups =
              [
                fixup ~kind:Rel8 ~range:(Lowered_ast.Signed 8) ~byte_offset:1 ~container:1
                  ~pc_bias:2 ~slices:(whole_container ~bits:8) e;
              ];
          };
          {
            Lowered_ast.bytes = "\xe9\x00\x00\x00\x00";
            form = "t.rel32";
            fixups =
              [
                fixup ~kind:Rel32 ~range:(Lowered_ast.Signed 32) ~byte_offset:1 ~container:4
                  ~pc_bias:5 ~slices:(whole_container ~bits:32) e;
              ];
          };
        ];
      origin;
    }

let branch_to sym = branch_to_expr (Expr.Symbol sym)
let filler n = Lowered_ast.Bytes { bytes = String.make n '\x90'; form = None; fixups = []; origin }
let label name = Lowered_ast.Label_def { name; origin }

let sym ?(global = true) ?(section = ".text") name =
  {
    Lowered_ast.name;
    global;
    visibility = Lowered_ast.Default;
    kind = Directive.Notype;
    size = None;
    section;
  }

let text_section ?(align = 1) frags =
  {
    Lowered_ast.sec = { Lowered_ast.sec_name = ".text"; perms = Perms.rx; alignment = align };
    fragments = frags;
  }

let module_ ?(symbols = []) sections =
  { Lowered_ast.unit_name = "t"; sections; symbols; declared_sections = [] }

let plan ?(entry = "entry") m =
  Image.plan_image ~evaluate { Image.default_policy with entry_symbol = Some entry } [ m ]

let show_errors ds =
  List.iter (fun d -> Fmt.pr "%s: %s@." (Diagnostic.code d) (Diagnostic.message d)) ds

(* Bind at a nonzero base throughout. A zero base makes an absolute address and
   a section offset print identically, which is exactly the confusion these
   tests exist to catch. *)
let bind_at l addrs =
  match Image.bind_image l ~addresses:addrs with
  | Error ds ->
      show_errors ds;
      None
  | Ok img -> Some img

let hex s = Byte_cursor.to_hex s

let show_text img =
  match img.Image.segments with
  | s :: _ -> Fmt.pr "%s@." (hex s.Image.bytes)
  | [] -> Fmt.pr "(no segments)@."

(* {1 Relaxation} *)

let%expect_test "a reachable branch selects the short rung" =
  let m =
    module_
      ~symbols:[ sym "entry"; sym "here" ]
      [ text_section [ label "entry"; branch_to "here"; filler 4; label "here" ] ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l -> (
      Fmt.pr "size: %d@." (List.hd (Image.plan_of l).Image.segments).Image.init_size;
      match bind_at l [ (".text", 0x1000L) ] with None -> () | Some img -> show_text img));
  (* eb 04: two bytes, displacement 4 from the end of the branch. *)
  [%expect {|
    size: 6
    eb 04 90 90 90 90 |}]

let%expect_test "an out-of-reach branch is promoted to the near rung" =
  let m =
    module_
      ~symbols:[ sym "entry"; sym "far" ]
      [ text_section [ label "entry"; branch_to "far"; filler 200; label "far" ] ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l -> (
      Fmt.pr "size: %d@." (List.hd (Image.plan_of l).Image.segments).Image.init_size;
      match bind_at l [ (".text", 0x1000L) ] with
      | None -> ()
      | Some img -> (
          match img.Image.segments with
          | s :: _ -> Fmt.pr "%s@." (hex (String.sub s.Image.bytes 0 5))
          | [] -> ())));
  (* Five bytes, displacement 200. The short rung cannot hold it, so the
     fixpoint promoted once and stopped. *)
  [%expect {|
    size: 205
    e9 c8 00 00 00 |}]

let%expect_test "the boundary between the two rungs" =
  (* A signed 8-bit displacement reaches 127. The fixpoint has to promote at
     exactly 128 and not before, which is the one place an off-by-one in either
     the range check or the PC bias would show. *)
  let try_gap n =
    let m =
      module_
        ~symbols:[ sym "entry"; sym "here" ]
        [ text_section [ label "entry"; branch_to "here"; filler n; label "here" ] ]
    in
    match plan m with
    | Error ds -> show_errors ds
    | Ok l ->
        let size = (List.hd (Image.plan_of l).Image.segments).Image.init_size in
        Fmt.pr "gap %3d -> %s@." n (if size = n + 2 then "rel8" else "rel32")
  in
  List.iter try_gap [ 126; 127; 128 ];
  [%expect {|
    gap 126 -> rel8
    gap 127 -> rel8
    gap 128 -> rel32 |}]

let%expect_test "an absolute target does not relax, whatever the offsets say" =
  (* The target is the address 4, not a location in this section. Laid out at
     offset 0 the section-relative arithmetic reads displacement 4 - 2 = 2 and
     the short rung looks like a fit; bound at 0x1000 the real displacement is
     4 - 0x1002, which no signed byte holds. Relaxation is over by then, so
     picking the short rung here would produce an image that can only be
     rejected, not repaired.

     Layout therefore has to recognise that the target does not move with the
     section and keep the longest rung. Note that binding then succeeds, which
     is the point: the answer is not "diagnose an absolute branch", it is
     "an absolute branch is not relaxable". *)
  let m =
    module_
      ~symbols:[ sym "entry" ]
      [ text_section [ label "entry"; branch_to_expr (Expr.Const (Bigint.of_int 4)) ] ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l -> (
      Fmt.pr "size: %d@." (List.hd (Image.plan_of l).Image.segments).Image.init_size;
      match bind_at l [ (".text", 0x1000L) ] with None -> () | Some img -> show_text img));
  (* e9 fe ef ff ff: -4094, little-endian. *)
  [%expect {|
    size: 5
    e9 ff ef ff ff |}]

let%expect_test "a difference of two labels is absolute too" =
  (* Both symbols are in this section, so every symbol the expression names is
     evaluable here - and the value is still not a location: it does not move
     when the section does. A rule phrased as "all symbols are local" would
     admit this and get the same wrong answer as a bare constant. *)
  let m =
    module_
      ~symbols:[ sym "entry"; sym "a"; sym "b" ]
      [
        text_section
          [
            label "entry";
            branch_to_expr (Expr.Binary (Expr.Sub, Expr.Symbol "b", Expr.Symbol "a"));
            label "a";
            filler 4;
            label "b";
          ];
      ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l -> Fmt.pr "size: %d@." (List.hd (Image.plan_of l).Image.segments).Image.init_size);
  [%expect {| size: 9 |}]

let%expect_test "a backward branch reaches too" =
  let m =
    module_
      ~symbols:[ sym "entry"; sym "top" ]
      [ text_section [ label "entry"; label "top"; filler 4; branch_to "top" ] ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l -> ( match bind_at l [ (".text", 0x1000L) ] with None -> () | Some img -> show_text img));
  (* eb fa: -6, back over the filler and the branch itself. *)
  [%expect {| 90 90 90 90 eb fa |}]

let%expect_test "one promotion forces another" =
  (* Two branches over one gap, sized so that both fit while both are short and
     neither does once the first grows. A pass that decided each fragment once,
     in order, would leave the second short and out of range - which is why the
     loop recomputes every offset each round instead.

     126 bytes of filler: short/short puts the first branch 128 away, one past
     what a signed byte holds. *)
  let m =
    module_
      ~symbols:[ sym "entry"; sym "a"; sym "b" ]
      [
        text_section
          [
            label "entry"; branch_to "a"; branch_to "b"; filler 126; label "a"; filler 2; label "b";
          ];
      ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l ->
      let size = (List.hd (Image.plan_of l).Image.segments).Image.init_size in
      (* 5 + 5 + 126 + 2: both promoted. *)
      Fmt.pr "size: %d@." size);
  [%expect {| size: 138 |}]

let%expect_test "alignment padding participates, and costs minimality" =
  (* Padding is recomputed each round, so alignment cannot make the loop
     diverge. It can however make the result non-minimal, and this is that case
     made explicit rather than left to be discovered.

     All-short puts the target 130 away, so the branch promotes. Promoting adds
     three bytes *before* an align-16 boundary, which absorbs them - the section
     is 132 bytes either way - and the target is now 127 away, which the short
     rung would have held. Promotion-only does not go back.

     This is the withdrawn minimality claim in one test. It is safe (a rel32
     holding 127 is correct) but it is not what GNU would emit, so if a real
     fixture ever lands in this shape the byte-for-byte differential gate is
     what will say so. *)
  let m =
    module_
      ~symbols:[ sym "entry"; sym "here" ]
      [
        text_section ~align:16
          [
            label "entry";
            branch_to "here";
            filler 100;
            Lowered_ast.Align
              { boundary = 16; fills = Array.init 15 (fun i -> String.make (i + 1) '\x90'); origin };
            filler 20;
            label "here";
          ];
      ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l -> (
      Fmt.pr "size: %d@." (List.hd (Image.plan_of l).Image.segments).Image.init_size;
      match bind_at l [ (".text", 0x1000L) ] with
      | None -> ()
      | Some img -> (
          match img.Image.segments with
          | s :: _ -> Fmt.pr "chose: %s@." (hex (String.sub s.Image.bytes 0 5))
          | [] -> ())));
  [%expect {|
    size: 132
    chose: e9 7f 00 00 00 |}]

(* A ladder whose widest rung is still narrow, so "nothing reaches" is testable
   without allocating four gigabytes of filler to get out of rel32 range. *)
let short_only_branch sym =
  Lowered_ast.Relax
    {
      alts =
        [
          {
            Lowered_ast.bytes = "\xeb\x00";
            form = "t.rel8";
            fixups =
              [
                fixup ~kind:Rel8 ~range:(Lowered_ast.Signed 8) ~byte_offset:1 ~container:1
                  ~pc_bias:2 ~slices:(whole_container ~bits:8) (Expr.Symbol sym);
              ];
          };
          {
            Lowered_ast.bytes = "\xeb\x00\x90";
            form = "t.rel8-padded";
            fixups =
              [
                fixup ~kind:Rel8 ~range:(Lowered_ast.Signed 8) ~byte_offset:1 ~container:1
                  ~pc_bias:2 ~slices:(whole_container ~bits:8) (Expr.Symbol sym);
              ];
          };
        ];
      origin;
    }

let%expect_test "a branch nothing can reach is reported, not truncated" =
  (* The verification step after the fixpoint. Without it the loop would settle
     on the widest rung and emit an encoding that does not reach: a wrong
     program that assembles cleanly. *)
  let m =
    module_
      ~symbols:[ sym "entry"; sym "unreachable" ]
      [
        text_section
          [ label "entry"; short_only_branch "unreachable"; filler 300; label "unreachable" ];
      ]
  in
  (match plan m with Error ds -> show_errors ds | Ok _ -> Fmt.pr "accepted@.");
  [%expect
    {|
    image.relax-unreachable: no encoding of this instruction reaches its target: widest form t.rel8-padded still out of range at section offset 0
    |}]

(* {1 Patching} *)

let%expect_test "an absolute fixup is patched little-endian" =
  let m =
    module_
      ~symbols:[ sym "entry"; sym "g" ~section:".data" ]
      [
        text_section
          [
            label "entry";
            Lowered_ast.Bytes
              {
                bytes = "\xb8\x00\x00\x00\x00";
                form = Some "t.mov-imm32";
                fixups =
                  [
                    fixup ~kind:Abs32 ~range:(Lowered_ast.Bitpattern 32) ~byte_offset:1 ~container:4
                      ~pc_bias:0 ~slices:(whole_container ~bits:32) (Expr.Symbol "g");
                  ];
                origin;
              };
          ];
        {
          Lowered_ast.sec = { Lowered_ast.sec_name = ".data"; perms = Perms.rw; alignment = 4 };
          fragments =
            [
              label "g";
              Lowered_ast.Bytes { bytes = "\x14\x00\x00\x00"; form = None; fixups = []; origin };
            ];
        };
      ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l -> (
      match bind_at l [ (".text", 0x40000000L); (".data", 0x40020000L) ] with
      | None -> ()
      | Some img -> show_text img));
  [%expect {| b8 00 00 02 40 |}]

let%expect_test "a fixup out of range is a diagnostic, not a truncation" =
  (* The value is checked against the fixup's declared range at bind, which is
     the only place the whole logical value exists: at encode time it is a
     placeholder that trivially fits. *)
  let m =
    module_
      ~symbols:[ sym "entry"; sym "g" ~section:".data" ]
      [
        text_section
          [
            label "entry";
            Lowered_ast.Bytes
              {
                bytes = "\xeb\x00";
                form = Some "t.rel8";
                fixups =
                  [
                    fixup ~kind:Rel8 ~range:(Lowered_ast.Signed 8) ~byte_offset:1 ~container:1
                      ~pc_bias:2 ~slices:(whole_container ~bits:8) (Expr.Symbol "g");
                  ];
                origin;
              };
          ];
        {
          Lowered_ast.sec = { Lowered_ast.sec_name = ".data"; perms = Perms.rw; alignment = 4 };
          fragments = [ label "g" ];
        };
      ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l -> (
      match bind_at l [ (".text", 0x40000000L); (".data", 0x40020000L) ] with
      | None -> ()
      | Some _ -> Fmt.pr "accepted@."));
  [%expect
    {|
    bind.fixup-range: fixup target: value 131070 does not fit s8 at section offset 1
    |}]

let%expect_test "a fixup naming an undefined symbol is rejected at plan time" =
  (* Not at bind: bind_image takes section addresses and has no import
     resolver, so a plan that could never be bound must not be produced. *)
  let m =
    module_ ~symbols:[ sym "entry" ] [ text_section [ label "entry"; branch_to "nowhere" ] ]
  in
  (match plan m with Error ds -> show_errors ds | Ok _ -> Fmt.pr "accepted@.");
  [%expect {| image.undefined: fixup target references undefined symbol nowhere |}]

let%expect_test "the plan reports its outstanding fixups" =
  let m =
    module_
      ~symbols:[ sym "entry"; sym "here" ]
      [ text_section [ label "entry"; branch_to "here"; label "here" ] ]
  in
  (* Printed as well as recorded, because contracts.md §1.5 puts [unresolved] in
     the plan dump: a plan that showed only what it had already decided would not
     say why §9's second step exists. *)
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l ->
      Fmt.pr "unresolved: %s@." (String.concat "," (Image.plan_of l).Image.unresolved);
      Fmt.pr "%a@." Image.pp_plan (Image.plan_of l));
  [%expect
    {|
    unresolved: target
    segment .text size=2 zero=0 align=1 permissions=r-x
    entry entry
    export entry
    export here
    unresolved target |}]

(* {1 Fixup observations}

   What the differential oracle classifies on. Every field is a separate column
   sourced from the module's own symbol table, so that changing the role, the
   binding, the section relation or the visibility independently changes an
   observation - nothing here recovers a role by parsing a kind name. *)

let observed m =
  match plan m with
  | Error ds -> show_errors ds
  | Ok l -> List.iter (fun o -> Fmt.pr "%a@." Image.pp_observation o) (Image.fixup_observations l)

let ref_to ?(kind = Abs32) ?(range = Lowered_ast.Bitpattern 32) e =
  Lowered_ast.Bytes
    {
      bytes = "\xb8\x00\x00\x00\x00";
      form = Some "t.mov-imm32";
      fixups =
        [
          fixup ~kind ~range ~byte_offset:1 ~container:4 ~pc_bias:0
            ~slices:(whole_container ~bits:32) e;
        ];
      origin;
    }

let%expect_test "an observation carries every column the classifier indexes on" =
  let m =
    module_
      ~symbols:
        [
          sym "entry";
          sym "g" ~section:".data";
          { (sym "hidden_local" ~global:false) with Lowered_ast.visibility = Lowered_ast.Hidden };
        ]
      [
        text_section
          [
            label "entry";
            (* A global object in another section. *)
            ref_to (Expr.Symbol "g");
            (* A local, hidden, same-section reference - each of those three a
               different column from the line above. *)
            ref_to (Expr.Symbol "hidden_local");
            (* A branch rather than a data address, so the role column moves
               without the kind name doing the work. *)
            ref_to ~kind:Rel32 ~range:(Lowered_ast.Signed 32) (Expr.Symbol "hidden_local");
            label "hidden_local";
          ];
        {
          Lowered_ast.sec = { Lowered_ast.sec_name = ".data"; perms = Perms.rw; alignment = 4 };
          fragments = [ label "g" ];
        };
      ]
  in
  observed m;
  [%expect
    {|
    .text	0x00000001	abs32	data-address	g	+0x0	global	default	other-section
    .text	0x00000006	abs32	data-address	hidden_local	+0x0	local	hidden	same-section
    .text	0x0000000b	rel32	branch	hidden_local	+0x0	local	hidden	same-section |}]

let%expect_test "an addend is normalized, not matched on its spelling" =
  (* [g + (2 + 2)] and [g + 4] are one reference and must produce one record.
     Matching the unfolded tree would call the first non-normalizable and drop
     it out of the record-for-record comparison entirely - a silently weaker
     gate, not a failing one. The commuted spelling is the same reference too.

     The last one genuinely is not a symbol plus an addend: an ELF record names
     one symbol, and this names two, so it is reported as itself and left to
     the post-link byte comparison. *)
  let m =
    module_
      ~symbols:[ sym "entry"; sym "g" ~section:".data"; sym "h" ~section:".data" ]
      [
        text_section
          [
            label "entry";
            ref_to
              (Expr.Binary
                 ( Expr.Add,
                   Expr.Symbol "g",
                   Expr.Binary (Expr.Add, Expr.Const (Bigint.of_int 2), Expr.Const (Bigint.of_int 2))
                 ));
            ref_to (Expr.Binary (Expr.Add, Expr.Const (Bigint.of_int 4), Expr.Symbol "g"));
            ref_to (Expr.Binary (Expr.Sub, Expr.Symbol "g", Expr.Const (Bigint.of_int 8)));
            ref_to (Expr.Binary (Expr.Sub, Expr.Symbol "h", Expr.Symbol "g"));
          ];
        {
          Lowered_ast.sec = { Lowered_ast.sec_name = ".data"; perms = Perms.rw; alignment = 4 };
          fragments = [ label "g"; filler 4; label "h" ];
        };
      ]
  in
  observed m;
  [%expect
    {|
    .text	0x00000001	abs32	data-address	g	+0x4	global	default	other-section
    .text	0x00000006	abs32	data-address	g	+0x4	global	default	other-section
    .text	0x0000000b	abs32	data-address	g	-0x8	global	default	other-section
    .text	0x00000010	abs32	data-address	non-normalizable	(h - g) |}]

(* {1 Multi-segment binding} *)

let two_sections () =
  module_
    ~symbols:[ sym "entry"; sym "g" ~section:".data" ]
    [
      text_section [ label "entry"; filler 16 ];
      {
        Lowered_ast.sec = { Lowered_ast.sec_name = ".data"; perms = Perms.rw; alignment = 4 };
        fragments = [ label "g"; filler 8 ];
      };
    ]

let%expect_test "two sections bind at their own addresses" =
  (match plan (two_sections ()) with
  | Error ds -> show_errors ds
  | Ok l -> (
      match bind_at l [ (".text", 0x40000000L); (".data", 0x40020000L) ] with
      | None -> ()
      | Some img -> Fmt.pr "%s@." (Fmt.to_to_string Image.pp img)));
  [%expect
    {|
    section .text address=0x40000000 size=16 permissions=r-x
    section .data address=0x40020000 size=8 permissions=rw-
    entry 0x40000000
    export entry = 0x40000000
    export g = 0x40020000 |}]

let%expect_test "overlapping segments are rejected" =
  (* Unreachable while there was only ever one segment, and the first thing
     that goes wrong once there are two: every existing caller assigned one base
     to all of them. *)
  (match plan (two_sections ()) with
  | Error ds -> show_errors ds
  | Ok l -> (
      match bind_at l [ (".text", 0x40000000L); (".data", 0x40000008L) ] with
      | None -> ()
      | Some _ -> Fmt.pr "accepted@."));
  [%expect
    {|
    bind.overlap: segments .text at 0x40000000 (16 bytes) and .data at 0x40000008 (8 bytes) overlap
    |}]
