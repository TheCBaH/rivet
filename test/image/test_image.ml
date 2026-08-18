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

let fixup ?(name = "target") ?(family = "") ?(pairing = Lowered_ast.Unpaired) ?(byte_offset = 1)
    ?(container = 4) ?(pc_bias = 5) ~kind ~range ~slices value =
  {
    Lowered_ast.kind;
    kind_name = kind_name kind;
    family =
      (if family <> "" then family else match kind with Abs32 -> "abs" | _ -> "pcrel-branch");
    role = (match kind with Abs32 -> Lowered_ast.Data_address | _ -> Lowered_ast.Branch);
    name;
    slices;
    byte_offset;
    container;
    pc_bias;
    range;
    value;
    pairing;
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

let sym ?(global = true) ?(weak = false) ?(section = ".text") name =
  {
    Lowered_ast.name;
    binding =
      (if weak then Lowered_ast.Weak else if global then Lowered_ast.Global else Lowered_ast.Local);
    visibility = Lowered_ast.Default;
    kind = Directive.Notype;
    size = None;
    section;
  }

let text_section ?(align = 1) frags =
  {
    Lowered_ast.sec =
      {
        Lowered_ast.sec_name = ".text";
        perms = Perms.rx;
        alignment = align;
        kind = Lowered_ast.Progbits;
      };
    fragments = frags;
  }

let module_ ?(unit_name = "t") ?(symbols = []) ?(commons = []) sections =
  { Lowered_ast.unit_name; sections; symbols; commons; declared_sections = [] }

let plan ?(entry = "entry") m =
  Image.plan_image ~evaluate { Image.default_policy with entry_symbol = Some entry } [ m ]

(* {2 Multi-module linking (M3 §4)} *)

let plan_many ?entry ms =
  Image.plan_image ~evaluate { Image.default_policy with entry_symbol = entry } ms

(* The payload only: a linker failure is wrapped now, and asm/docs/errors.md §3
   keeps Err provenance out of anything an expect baseline compares. *)
let show_errors e =
  List.iter
    (fun d -> Fmt.pr "%s: %s@." (Diagnostic.code d) (Diagnostic.message d))
    (Diag.diagnostics e)

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
          Lowered_ast.sec =
            {
              Lowered_ast.sec_name = ".data";
              perms = Perms.rw;
              alignment = 4;
              kind = Lowered_ast.Progbits;
            };
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
          Lowered_ast.sec =
            {
              Lowered_ast.sec_name = ".data";
              perms = Perms.rw;
              alignment = 4;
              kind = Lowered_ast.Progbits;
            };
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

(* {1 Paired fixups}

   A synthetic absolute split keeps this test target-independent: both words
   evaluate the head's target, while the tail retains its anchor expression in
   observations.  RISC-V uses the same layout mechanism for PC-relative
   HI20/LO12 pairs. *)

let pair_fixup ?(family = "split") ?(pairing = Lowered_ast.Unpaired) ~byte_offset value =
  fixup ~name:"pair" ~family ~pairing ~kind:Abs32 ~range:(Lowered_ast.Bitpattern 32) ~byte_offset
    ~container:4 ~pc_bias:0 ~slices:(whole_container ~bits:32) value

let pair_bytes fixups =
  Lowered_ast.Bytes { bytes = String.make 12 '\x00'; form = Some "t.pair"; fixups; origin }

let pair_plan frags =
  plan
    (module_
       ~symbols:[ sym "entry"; sym "target" ]
       [ text_section ((label "entry" :: frags) @ [ label "target" ]) ])

let%expect_test "an anchor tail evaluates against its high fixup target" =
  let fragment =
    pair_bytes
      [
        pair_fixup ~pairing:(Lowered_ast.Pair_head "hi") ~byte_offset:0 (Expr.Symbol "target");
        pair_fixup ~pairing:(Lowered_ast.Pair_tail (Lowered_ast.Anchor_symbol ".Lhi"))
          ~byte_offset:4 (Expr.Symbol ".Lhi");
        pair_fixup ~pairing:(Lowered_ast.Pair_tail (Lowered_ast.Anchor_symbol ".Lhi"))
          ~byte_offset:8 (Expr.Symbol ".Lhi");
      ]
  in
  (match pair_plan [ label ".Lhi"; fragment ] with
  | Error ds -> show_errors ds
  | Ok l -> ( match bind_at l [ (".text", 0x1000L) ] with None -> () | Some img -> show_text img));
  [%expect {|
    0c 10 00 00 0c 10 00 00 0c 10 00 00 |}]

let%expect_test "a sibling key is fragment-local" =
  let head =
    pair_bytes
      [ pair_fixup ~pairing:(Lowered_ast.Pair_head "k") ~byte_offset:0 (Expr.Symbol "target") ]
  in
  let tail =
    pair_bytes
      [
        pair_fixup ~pairing:(Lowered_ast.Pair_tail (Lowered_ast.Sibling_key "k")) ~byte_offset:4
          (Expr.Symbol "entry");
      ]
  in
  (match pair_plan [ head; tail ] with Error ds -> show_errors ds | Ok _ -> Fmt.pr "accepted@.");
  [%expect {| image.pair-missing-head: paired fixup k has no matching head |}]

let%expect_test "paired-fixup anchor failures are typed" =
  let attempt frags =
    match pair_plan frags with Error ds -> show_errors ds | Ok _ -> Fmt.pr "accepted@."
  in
  attempt
    [
      pair_bytes
        [
          pair_fixup ~pairing:(Lowered_ast.Pair_tail (Lowered_ast.Anchor_symbol ".Lmissing"))
            ~byte_offset:4 (Expr.Symbol ".Lmissing");
        ];
    ];
  attempt
    [
      label ".Lplain";
      pair_bytes
        [
          pair_fixup ~pairing:(Lowered_ast.Pair_tail (Lowered_ast.Anchor_symbol ".Lplain"))
            ~byte_offset:4 (Expr.Symbol ".Lplain");
        ];
    ];
  attempt
    [
      label ".Lwrong";
      pair_bytes
        [
          pair_fixup ~family:"high" ~pairing:(Lowered_ast.Pair_head "h") ~byte_offset:0
            (Expr.Symbol "target");
          pair_fixup ~family:"low"
            ~pairing:(Lowered_ast.Pair_tail (Lowered_ast.Anchor_symbol ".Lwrong")) ~byte_offset:4
            (Expr.Symbol ".Lwrong");
        ];
    ];
  [%expect
    {|
    image.pair-undefined-anchor: paired fixup anchor .Lmissing is undefined
    image.pair-missing-head: paired fixup .Lplain has no matching head
    image.pair-family: paired fixup .Lwrong has an incompatible head family |}]

let%expect_test "duplicate and ambiguous heads are rejected" =
  let attempt fixups =
    match pair_plan [ label ".Lhi"; pair_bytes fixups ] with
    | Error ds -> show_errors ds
    | Ok _ -> Fmt.pr "accepted@."
  in
  attempt
    [
      pair_fixup ~pairing:(Lowered_ast.Pair_head "same") ~byte_offset:0 (Expr.Symbol "target");
      pair_fixup ~pairing:(Lowered_ast.Pair_head "same") ~byte_offset:0 (Expr.Symbol "target");
    ];
  attempt
    [
      pair_fixup ~pairing:(Lowered_ast.Pair_head "one") ~byte_offset:0 (Expr.Symbol "target");
      pair_fixup ~pairing:(Lowered_ast.Pair_head "two") ~byte_offset:0 (Expr.Symbol "target");
      pair_fixup ~pairing:(Lowered_ast.Pair_tail (Lowered_ast.Anchor_symbol ".Lhi")) ~byte_offset:4
        (Expr.Symbol ".Lhi");
    ];
  [%expect
    {|
    image.pair-duplicate-head: duplicate paired-fixup head same
    image.pair-ambiguous-head: paired fixup .Lhi has multiple matching heads |}]

let%expect_test "an anchor may not cross sections" =
  let m =
    module_
      ~symbols:[ sym "entry"; sym "target"; sym "anchor" ~section:".data" ]
      [
        text_section
          [
            label "entry";
            pair_bytes
              [
                pair_fixup ~pairing:(Lowered_ast.Pair_tail (Lowered_ast.Anchor_symbol "anchor"))
                  ~byte_offset:4 (Expr.Symbol "anchor");
              ];
            label "target";
          ];
        {
          Lowered_ast.sec =
            {
              Lowered_ast.sec_name = ".data";
              perms = Perms.rw;
              alignment = 4;
              kind = Lowered_ast.Progbits;
            };
          fragments = [ label "anchor"; filler 4 ];
        };
      ]
  in
  (match plan m with Error ds -> show_errors ds | Ok _ -> Fmt.pr "accepted@.");
  [%expect {| image.pair-cross-section: paired fixup anchor anchor is in another section |}]

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
          Lowered_ast.sec =
            {
              Lowered_ast.sec_name = ".data";
              perms = Perms.rw;
              alignment = 4;
              kind = Lowered_ast.Progbits;
            };
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
          Lowered_ast.sec =
            {
              Lowered_ast.sec_name = ".data";
              perms = Perms.rw;
              alignment = 4;
              kind = Lowered_ast.Progbits;
            };
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
        Lowered_ast.sec =
          {
            Lowered_ast.sec_name = ".data";
            perms = Perms.rw;
            alignment = 4;
            kind = Lowered_ast.Progbits;
          };
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

let%expect_test "placement preflight accumulates independent failures in order" =
  (match plan (two_sections ()) with
  | Error ds -> show_errors ds
  | Ok l -> (
      match Image.bind_image l ~addresses:[ (".text", -1L); (".data", 2L) ] with
      | Ok _ -> Fmt.pr "accepted@."
      | Error error ->
          Diag.diagnostics error
          |> List.iter (fun diagnostic -> Fmt.pr "%s@." (Diagnostic.code diagnostic))));
  [%expect {|
    bind.overflow
    bind.overlap
    bind.alignment
    |}]

(* {1 Symtab: input-qualified symbol identity (M3 §2)}

   Pure data-model tests against [Symtab] directly, with no
   [plan_image] involved - multi-module planning is a later M3 step
   (`create-implementation-plan-for-golden-kettle.md`'s step 4). These three
   cases are exactly what that plan's step 2 requires proven before any
   parser change: local names never collide across inputs, a local shadows a
   same-spelled foreign global from inside its own input, and a foreign
   local can never be captured by name the way a flat, bare-string lookup
   over the old single-module [offsets] would have. *)

let def ?(binding = Lowered_ast.Local) ?(section = ".text") ?(offset = 0) unit_name name =
  {
    Image.Symtab.d_unit = unit_name;
    d_name = name;
    d_binding = binding;
    d_section = section;
    d_offset = offset;
  }

let reference unit_name name = { Image.Symtab.r_unit = unit_name; r_name = name }

let%expect_test "two modules each defining a same-named local do not collide" =
  let defs = [ def "a" "x" ~offset:4; def "b" "x" ~offset:8 ] in
  let local = Image.Symtab.build_local_table defs in
  (match (List.assoc_opt ("a", "x") local, List.assoc_opt ("b", "x") local) with
  | Some da, Some db -> Fmt.pr "a.x=%d b.x=%d@." da.Image.Symtab.d_offset db.Image.Symtab.d_offset
  | _ -> Fmt.pr "missing@.");
  [%expect {| a.x=4 b.x=8 |}]

let%expect_test "a reference resolves to its own input's local, never a same-spelled foreign global"
    =
  let defs = [ def "a" "x" ~offset:4; def "b" "x" ~binding:Lowered_ast.Global ~offset:8 ] in
  let local = Image.Symtab.build_local_table defs in
  let link_global = Image.Symtab.build_link_global_table defs in
  (match Image.Symtab.resolve_reference ~local ~link_global (reference "a" "x") with
  | Image.Symtab.Resolved_local d ->
      Fmt.pr "resolved to %s's local at %d@." d.Image.Symtab.d_unit d.Image.Symtab.d_offset
  | Image.Symtab.Resolved_global _ -> Fmt.pr "wrongly resolved to a foreign global@."
  | Image.Symtab.Unresolved -> Fmt.pr "wrongly unresolved@.");
  [%expect {| resolved to a's local at 4 |}]

let%expect_test
    "a foreign local is never visible, even under a name a bare-string lookup would find" =
  (* [x] is local in [a] and never exported - exactly the case where a flat,
     bare-string [offsets] lookup (the pre-M3 shape) would have silently
     handed [b]'s reference [a]'s definition. *)
  let defs = [ def "a" "x" ~offset:4 ] in
  let local = Image.Symtab.build_local_table defs in
  let link_global = Image.Symtab.build_link_global_table defs in
  (match Image.Symtab.resolve_reference ~local ~link_global (reference "b" "x") with
  | Image.Symtab.Resolved_local d -> Fmt.pr "wrongly captured as %s's local@." d.Image.Symtab.d_unit
  | Image.Symtab.Resolved_global _ -> Fmt.pr "wrongly captured as a global@."
  | Image.Symtab.Unresolved -> Fmt.pr "unresolved, as required@.");
  [%expect {| unresolved, as required |}]

(* {1 Multi-module linking through plan_image/bind_image (M3 §4, step 4)}

   Everything above proved the data model in isolation; these prove the
   wiring - [Image.plan_image] and [Image.bind_image] themselves, not
   [Symtab] called directly - which is the plan's own exit criterion: two
   inputs become one resolved, fixed-address image with no object files and
   no external linker. *)

let%expect_test "a cross-file reference resolves and patches exactly like the single-module case" =
  (* Same shape and same expected bytes as "an absolute fixup is patched
     little-endian" above, split across two inputs: [a] references [g], [b]
     defines it. If this prints anything else, cross-module resolution
     changed the bind result, not just its internal bookkeeping. *)
  let a =
    module_ ~unit_name:"a"
      ~symbols:[ sym "entry" ]
      [ text_section [ label "entry"; ref_to (Expr.Symbol "g") ] ]
  in
  let b =
    module_ ~unit_name:"b"
      ~symbols:[ sym "g" ~section:".data" ]
      [
        {
          Lowered_ast.sec =
            {
              Lowered_ast.sec_name = ".data";
              perms = Perms.rw;
              alignment = 4;
              kind = Lowered_ast.Progbits;
            };
          fragments =
            [
              label "g";
              Lowered_ast.Bytes { bytes = "\x14\x00\x00\x00"; form = None; fixups = []; origin };
            ];
        };
      ]
  in
  (match plan_many ~entry:"entry" [ a; b ] with
  | Error ds -> show_errors ds
  | Ok l -> (
      match bind_at l [ (".text", 0x40000000L); (".data", 0x40020000L) ] with
      | None -> ()
      | Some img -> show_text img));
  [%expect {| b8 00 00 02 40 |}]

let%expect_test "each input's fixup resolves to its own input's local, never the other's" =
  (* [x] is an anonymous local (no symbol record - like an ordinary branch
     target) in *both* inputs. A bare-string lookup over a flat, pre-M3
     [offsets] table could only ever find one of them; here each input's own
     reference must find its own [x], at its own, distinct merged offset. *)
  let a = module_ ~unit_name:"a" [ text_section [ ref_to (Expr.Symbol "x"); label "x" ] ] in
  let b = module_ ~unit_name:"b" [ text_section [ ref_to (Expr.Symbol "x"); label "x" ] ] in
  (match plan_many [ a; b ] with
  | Error ds -> show_errors ds
  | Ok l -> ( match bind_at l [ (".text", 0x1000L) ] with None -> () | Some img -> show_text img));
  (* a's own x is at merged offset 5 (0x1005); b's own x is at merged offset
     10 (0x100a), right after a's whole 5-byte contribution. Each fixup must
     find its own, not the other's. *)
  [%expect {| b8 05 10 00 00 b8 0a 10 00 00 |}]

let%expect_test "a same-name section fed both kinds by two inputs is rejected, not coerced" =
  let bss_section kind =
    {
      Lowered_ast.sec = { Lowered_ast.sec_name = ".bss"; perms = Perms.rw; alignment = 1; kind };
      fragments = [];
    }
  in
  let a = module_ ~unit_name:"a" [ bss_section Lowered_ast.Progbits ] in
  let b = module_ ~unit_name:"b" [ bss_section Lowered_ast.Nobits ] in
  (match plan_many [ a; b ] with Error ds -> show_errors ds | Ok _ -> Fmt.pr "accepted@.");
  [%expect {| image.kind-mismatch: section .bss is PROGBITS in one input and NOBITS in another |}]

let%expect_test "a same-name section's flag mismatch unions rather than rejects" =
  let flagged perms byte =
    {
      Lowered_ast.sec =
        { Lowered_ast.sec_name = ".flagtest"; perms; alignment = 1; kind = Lowered_ast.Progbits };
      fragments = [ Lowered_ast.Bytes { bytes = byte; form = None; fixups = []; origin } ];
    }
  in
  let a = module_ ~unit_name:"a" [ flagged Perms.ro "\x01" ] in
  let b = module_ ~unit_name:"b" [ flagged Perms.rw "\x02" ] in
  (match plan_many [ a; b ] with
  | Error ds -> show_errors ds
  | Ok l -> Fmt.pr "%s@." (Fmt.to_to_string Image.pp_plan (Image.plan_of l)));
  [%expect {| segment .flagtest size=2 zero=0 align=1 permissions=rw- |}]

let%expect_test "a merge-boundary gap is filled by the caller's callback, not silently zeroed" =
  let a =
    module_ ~unit_name:"a"
      [ text_section [ Lowered_ast.Bytes { bytes = "\x11"; form = None; fixups = []; origin } ] ]
  in
  let b =
    module_ ~unit_name:"b"
      [
        text_section ~align:4
          [ Lowered_ast.Bytes { bytes = "\x22"; form = None; fixups = []; origin } ];
      ]
  in
  let fill ~executable n = if executable then String.make n '\xaa' else String.make n '\x00' in
  (match Image.plan_image ~evaluate ~fill Image.default_policy [ a; b ] with
  | Error ds -> show_errors ds
  | Ok l -> ( match bind_at l [ (".text", 0x1000L) ] with None -> () | Some img -> show_text img));
  [%expect {| 11 aa aa aa 22 |}]

(* {1 NOBITS: logical extent, not emitted bytes (M3 §6, step 5)} *)

let bss_section ?(align = 4) frags =
  {
    Lowered_ast.sec =
      {
        Lowered_ast.sec_name = ".bss";
        perms = Perms.rw;
        alignment = align;
        kind = Lowered_ast.Nobits;
      };
    fragments = frags;
  }

let%expect_test "a NOBITS section reserves logical extent without writing real bytes" =
  let m =
    module_
      [
        text_section [ label "entry" ];
        bss_section
          [
            Lowered_ast.Zero { length = 4; origin };
            label "flag";
            Lowered_ast.Zero { length = 1; origin };
          ];
      ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l -> Fmt.pr "%s@." (Fmt.to_to_string Image.pp_plan (Image.plan_of l)));
  [%expect
    {|
    segment .text size=0 zero=0 align=1 permissions=r-x
    segment .bss size=0 zero=5 align=4 permissions=rw-
    entry entry |}]

let%expect_test "a NOBITS label lands after its reservation, and binds without real bytes" =
  let m =
    module_
      ~symbols:[ sym "entry"; sym "flag" ~section:".bss" ]
      [
        text_section [ label "entry" ];
        bss_section [ Lowered_ast.Zero { length = 4; origin }; label "flag" ];
      ]
  in
  (match plan m with
  | Error ds -> show_errors ds
  | Ok l -> (
      match Image.bind_image l ~addresses:[ (".text", 0x1000L); (".bss", 0x2000L) ] with
      | Error ds -> show_errors ds
      | Ok img ->
          (* Standing invariant (§6): every bound segment's real bytes and
             logical extent are asked separately - a NOBITS segment's own
             bytes are always empty, whatever its zero_fill says. *)
          List.iter
            (fun (s : Image.segment) ->
              Fmt.pr "%s bytes=%d zero_fill=%d@." s.Image.name (String.length s.Image.bytes)
                s.Image.zero_fill)
            img.Image.segments;
          Fmt.pr "%s@." (Fmt.to_to_string Image.pp img)));
  [%expect
    {|
    .text bytes=0 zero_fill=0
    .bss bytes=0 zero_fill=4
    section .text address=0x1000 size=0 permissions=r-x
    section .bss address=0x2000 size=4 permissions=rw-
    entry 0x1000
    export entry = 0x1000
    export flag = 0x2004 |}]

let%expect_test "initialized content inside a NOBITS section is rejected, not silently dropped" =
  let m =
    module_ ~symbols:[ sym "entry" ] [ text_section [ label "entry" ]; bss_section [ filler 4 ] ]
  in
  (match plan m with Error ds -> show_errors ds | Ok _ -> Fmt.pr "accepted@.");
  [%expect {| image.nobits-content: section .bss is NOBITS and cannot hold initialized content |}]

(* {1 .comm / .weak resolution (M3 §7)}

   Pairwise precedence, common-vs-local conflict, common folded alongside a
   user-written .bss, and undefined-weak substitution - all through
   plan_image/bind_image directly, not through Symtab, since what is under
   test here is the resolver's wiring (build_link_global_table's
   strong-first ordering, the common pre-pass, address_of's weak fallback),
   not the data model §1/§2 already proved. *)

let comm name size align = { Lowered_ast.comm_name = name; comm_size = size; comm_align = align }

let%expect_test "strong beats common: a real definition discards every .comm of the same name" =
  let a =
    module_ ~unit_name:"a"
      ~symbols:[ sym "x" ~section:".data" ]
      [
        {
          Lowered_ast.sec =
            {
              Lowered_ast.sec_name = ".data";
              perms = Perms.rw;
              alignment = 4;
              kind = Lowered_ast.Progbits;
            };
          fragments = [ label "x"; filler 4 ];
        };
      ]
  in
  let b = module_ ~unit_name:"b" ~commons:[ comm "x" 4 4 ] [] in
  (match plan_many [ a; b ] with
  | Error ds -> show_errors ds
  | Ok l -> Fmt.pr "%s@." (Fmt.to_to_string Image.pp_plan (Image.plan_of l)));
  (* No .bss segment at all: the common declaration never allocated
     anything, because [x] already has a real definition. *)
  [%expect {|
    segment .data size=4 zero=0 align=4 permissions=rw-
    export x |}]

let%expect_test "common beats weak: an undefined-weak reference finds the common allocation" =
  let a =
    module_ ~unit_name:"a"
      ~symbols:[ sym "entry"; sym "x" ~weak:true ]
      [ text_section [ label "entry"; ref_to (Expr.Symbol "x") ] ]
  in
  let b = module_ ~unit_name:"b" ~commons:[ comm "x" 4 4 ] [] in
  (match plan_many ~entry:"entry" [ a; b ] with
  | Error ds -> show_errors ds
  | Ok l -> (
      match bind_at l [ (".text", 0x1000L); (".bss", 0x2000L) ] with
      | None -> ()
      | Some img -> show_text img));
  (* b8 00 20 00 00: the common allocation's address (0x2000), not 0 - if
     this printed zero, common lost to the undefined-weak-resolves-to-0
     path instead of winning outright. *)
  [%expect {| b8 00 20 00 00 |}]

let%expect_test "strong beats weak, re-verified link-wide" =
  let a =
    module_ ~unit_name:"a"
      ~symbols:[ sym "entry"; sym "x" ~section:".data" ]
      [
        text_section [ label "entry" ];
        {
          Lowered_ast.sec =
            {
              Lowered_ast.sec_name = ".data";
              perms = Perms.rw;
              alignment = 4;
              kind = Lowered_ast.Progbits;
            };
          fragments = [ label "x"; filler 4 ];
        };
      ]
  in
  let b =
    module_ ~unit_name:"b"
      ~symbols:[ sym "x" ~weak:true ]
      [ text_section [ ref_to (Expr.Symbol "x") ] ]
  in
  (match plan_many ~entry:"entry" [ a; b ] with
  | Error ds -> show_errors ds
  | Ok l -> (
      match bind_at l [ (".text", 0x1000L); (".data", 0x2000L) ] with
      | None -> ()
      | Some img ->
          List.iter
            (fun (s : Image.segment) -> Fmt.pr "%s %s@." s.Image.name (hex s.Image.bytes))
            img.Image.segments));
  [%expect {|
    .text b8 00 20 00 00
    .data 90 90 90 90 |}]

let%expect_test "a .comm conflicting with a local definition in the same input is rejected" =
  let a =
    module_ ~unit_name:"a"
      ~symbols:[ sym "x" ~global:false ]
      ~commons:[ comm "x" 4 1 ]
      [ text_section [ label "x" ] ]
  in
  (match plan_many [ a ] with Error ds -> show_errors ds | Ok _ -> Fmt.pr "accepted@.");
  [%expect
    {| image.common-conflicts-local: common declaration of x conflicts with a local definition in the same input |}]

let%expect_test
    "a resolved common allocation is folded into a user-written .bss, not a second segment" =
  let a =
    module_ ~unit_name:"a"
      ~commons:[ comm "y" 4 4 ]
      [ bss_section [ Lowered_ast.Zero { length = 8; origin }; label "existing" ] ]
  in
  (match plan_many [ a ] with
  | Error ds -> show_errors ds
  | Ok l -> Fmt.pr "%s@." (Fmt.to_to_string Image.pp_plan (Image.plan_of l)));
  (* One .bss segment, sized 8 (existing) + 4 (y) = 12 - not two. *)
  [%expect {| segment .bss size=0 zero=12 align=4 permissions=rw- |}]

let%expect_test "an undefined weak reference resolves to 0, absolute and PC-relative alike" =
  let a =
    module_ ~unit_name:"a"
      ~symbols:[ sym "entry"; sym "w" ~weak:true ]
      [ text_section [ label "entry"; ref_to (Expr.Symbol "w"); branch_to "w" ] ]
  in
  (match plan_many ~entry:"entry" [ a ] with
  | Error ds -> show_errors ds
  | Ok l -> ( match bind_at l [ (".text", 0x1000L) ] with None -> () | Some img -> show_text img));
  (* b8 00 00 00 00: the absolute reference, patched to the literal value 0.
     Then the branch: [w] at address 0 is unreachable from a short rung, so
     relaxation keeps the long one - eb (short) never being selected is
     itself part of the proof that the target genuinely resolved to a fixed,
     far-away address (0) rather than merely "not failing". *)
  [%expect {| b8 00 00 00 00 e9 f6 ef ff ff |}]
