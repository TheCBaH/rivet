(* Property tests (.ai/asm_plan.md M2.J).

   What belongs here and what does not. An expect test states one input and its
   whole answer, which is the right shape for a contract - the transcript *is*
   the specification, and a reviewer reads it. A property states a law over a
   space too large to write down, and its transcript says only "1000 cases
   passed", which is worth much less to a reader and much more to a bug.

   So these are laws that no finite table could state honestly:

   - decoding is a right inverse of encoding, over generated instructions;
   - re-encoding a decoded word preserves the instruction, over generated
     *bytes* - the direction that needs no instruction generator, and that
     catches a form which decodes a pattern it cannot reproduce. Not
     byte-preserving: see the note on that property for why, and for the
     counterexample the first run of this file produced;
   - a split fixup joins and splits as an isomorphism, with the sign applied to
     the whole value rather than to a slice;
   - relaxation terminates, is feasible, and is stable (M2.F0's three narrowed
     claims - minimality is *not* among them and is not asserted here);
   - form selection is monotone in distance: a branch that has gone long does
     not come back short as its target moves further away.

   The plan requires every counterexample to be frozen as a readable expect
   case. That is the discipline this file is written for: a failure here is not
   fixed by re-running until it passes, it is reduced to one input and moved
   into the expect suite next to the contract it violates. *)

let seed = 20260809
(* Fixed, and printed with the summary. A property failure that cannot be
   reproduced from its own transcript reports something nobody can act on. *)

module Q = QCheck2

(* {1 The synthetic ISA}

   No target, for test/lib/codec's reason: the laws below are the codec's, and
   an instruction encoder standing between the generator and the tree would make
   a failure ambiguous about which of the two is wrong. *)

open Codec
open Codec_corpus.Synthetic_isa

let gen_reg = Q.Gen.oneof_list all_regs

let gen_insn =
  Q.Gen.oneof
    [
      Q.Gen.return Nop;
      Q.Gen.map2 (fun a b -> Add (a, b)) gen_reg gen_reg;
      (* Both sides of the short form's range boundary, which is the only thing
         any guard in this ISA branches on. *)
      Q.Gen.map2 (fun r v -> Li (r, Int64.of_int v)) gen_reg (Q.Gen.int_range 0 255);
      Q.Gen.map (fun v -> Jmp (Int64.of_int v)) (Q.Gen.int_range 0 4095);
    ]

let prop_encode_decode =
  Q.Test.make ~count:2000 ~name:"decode inverts encode" ~print:show_insn gen_insn (fun insn ->
      match encode isa insn with
      | Error _ ->
          (* Every generated value is in range by construction, so a failure to
             encode is itself a counterexample rather than a skipped case. *)
          false
      | Ok e -> (
          match decode_bits isa e.bits with
          | None -> false
          | Some d -> show_insn d.value = show_insn insn))

let prop_decode_reencode =
  (* The other direction, and the one that needs no instruction generator: bytes
     in, and if some form claims them it has to be able to produce that
     instruction again.

     Note what is *not* claimed. Re-encoding is not byte-preserving, and the
     first run of this file said so: it produced 0x2000, which is [li.long r0,
     0] and re-encodes as the two-byte [li.short]. That is §4.9's three levels
     working - syntax, semantic instruction, exact form - and the counterexample
     is already frozen next door as test_codec's "two encodings, one semantic
     instruction". Preserving the *form* across a text round trip is what
     [Codec.Relax]'s rung pin is for, and it applies to relaxation ladders
     rather than to every [Alt].

     So the law is that decoding is idempotent through an encode: whatever the
     assembler picks must mean the same thing. A form that decoded a pattern it
     could not reproduce at all would fail here, which is the bug worth
     catching. *)
  Q.Test.make ~count:5000 ~name:"re-encoding a decoded word preserves the instruction"
    ~print:(Printf.sprintf "0x%04x") (Q.Gen.int_range 0 0xFFFF) (fun w ->
      let bits = Bits.of_int64 ~width:16 (Int64.of_int w) in
      match decode_bits isa bits with
      | None -> true (* no form claims these bits; nothing is asserted *)
      | Some d -> (
          match encode isa d.value with
          | Error _ -> false
          | Ok e -> (
              match decode_bits isa e.bits with
              | Some d' -> show_insn d'.value = show_insn d.value
              | None -> false)))

(* {1 Split fixups}

   The law the [Iso_fun] boundary exists to carry: the codec sees two slices at
   non-adjacent positions and the logical value is one number. Signedness is
   applied once, to the whole - which is why the signed case is generated over
   negative values too, where per-slice extension gives the wrong answer for
   every input rather than for none. *)

let split_round codec v =
  match encode codec (Split v) with
  | Error _ -> false
  | Ok e -> (
      match decode_bits codec e.bits with
      | Some { value = Split v'; _ } -> Int64.equal v v'
      | None -> false)

let prop_split_unsigned =
  Q.Test.make ~count:2000 ~name:"a split fixup joins what it split" ~print:(Printf.sprintf "%d")
    (Q.Gen.int_range 0 0xFFFF) (fun v -> split_round split_hi_lo (Int64.of_int v))

let prop_split_signed =
  Q.Test.make ~count:2000 ~name:"a split fixup sign-extends the whole value"
    ~print:(Printf.sprintf "%d") (Q.Gen.int_range (-32768) 32767) (fun v ->
      split_round split_signed (Int64.of_int v))

(* {1 Relaxation}

   M2.F0's three claims, and only those three. Minimality is deliberately absent:
   alignment padding makes layout non-monotone in fragment size, so a smaller
   selection vector may exist and the plan withdraws the claim rather than
   asserting something it cannot prove. What replaces it is the exact-byte
   differential gate, which is a stronger statement about the cases that matter.

   The module is built directly rather than assembled from text, so a failure is
   about layout and not about a parser. *)

open Asm_core

let origin = Foundation.Origin.synthesized ~pass:"prop" ()

type kind = Rel8 | Rel32

let evaluate _ ~place ~target = Ok (Int64.sub target place)

let fixup ~kind ~range ~bits ~pc_bias ~container value =
  {
    Lowered_ast.kind;
    kind_name = (match kind with Rel8 -> "rel8" | Rel32 -> "rel32");
    family = "pcrel-branch";
    role = Lowered_ast.Branch;
    name = "target";
    slices = [ { Lowered_ast.bit_offset = 0; bit_width = bits; value_lsb = 0 } ];
    byte_offset = 1;
    container;
    pc_bias;
    range;
    value;
    pairing = Lowered_ast.Unpaired;
    origin;
  }

let branch_to sym =
  Lowered_ast.Relax
    {
      alts =
        [
          {
            Lowered_ast.bytes = "\xeb\x00";
            form = "t.rel8";
            fixups =
              [
                fixup ~kind:Rel8 ~range:(Lowered_ast.Signed 8) ~bits:8 ~pc_bias:2 ~container:1
                  (Expr.Symbol sym);
              ];
          };
          {
            Lowered_ast.bytes = "\xe9\x00\x00\x00\x00";
            form = "t.rel32";
            fixups =
              [
                fixup ~kind:Rel32 ~range:(Lowered_ast.Signed 32) ~bits:32 ~pc_bias:5 ~container:4
                  (Expr.Symbol sym);
              ];
          };
        ];
      origin;
    }

(* A section of [n] branches, each aimed at a label [k] branches further on,
   interleaved with filler and alignment. Alignment is in the generator on
   purpose: it is what makes layout non-monotone, and a generator that omitted it
   would only ever exercise the easy half of the fixpoint. *)
type shape = { hops : int list; fillers : int list; align : int }

let show_shape s =
  Printf.sprintf "{hops=[%s]; fillers=[%s]; align=%d}"
    (String.concat ";" (List.map string_of_int s.hops))
    (String.concat ";" (List.map string_of_int s.fillers))
    s.align

let gen_shape =
  Q.Gen.(
    let* n = int_range 1 6 in
    let* hops = list_size (return n) (int_range 0 3) in
    let* fillers = list_size (return n) (int_range 0 200) in
    let* align = oneof_list [ 1; 2; 4; 16 ] in
    return { hops; fillers; align })

let module_of s =
  let n = List.length s.hops in
  let label i = Printf.sprintf "L%d" i in
  let frags =
    List.concat
      (List.mapi
         (fun i hop ->
           let target = min (n - 1) (i + hop) in
           [
             Lowered_ast.Label_def { name = label i; origin };
             branch_to (label target);
             Lowered_ast.Bytes
               {
                 bytes = String.make (List.nth s.fillers i) '\x90';
                 form = None;
                 fixups = [];
                 origin;
               };
             Lowered_ast.Align
               {
                 boundary = s.align;
                 fills = Array.init (s.align - 1) (fun i -> String.make (i + 1) (Char.chr 0x90));
                 origin;
               };
           ])
         s.hops)
  in
  {
    Lowered_ast.unit_name = "p";
    sections =
      [
        {
          Lowered_ast.sec = { Lowered_ast.sec_name = ".text"; perms = Perms.rx; alignment = 1 };
          fragments = frags @ [ Lowered_ast.Label_def { name = label (n - 1); origin } ];
        };
      ];
    symbols =
      List.init n (fun i ->
          {
            Lowered_ast.name = label i;
            global = false;
            visibility = Lowered_ast.Default;
            kind = Directive.Notype;
            size = None;
            section = ".text";
          });
    declared_sections = [];
  }

let plan_shape s = Image.plan_image ~evaluate Image.default_policy [ module_of s ]

let prop_relax_feasible =
  (* Termination is not a separate assertion: the fixpoint either returns or the
     test does not, and a bounded [count] with a bounded shape is what turns
     "does not terminate" into a visible hang rather than a silent pass.

     What *is* asserted is feasibility. Planning either reports a fragment that
     cannot reach - which is a legitimate answer and not a failure - or every
     selection fits at the final offsets, which binding is what proves: a
     selection that did not fit would come back as bind.fixup-range. *)
  Q.Test.make ~count:500 ~name:"every relaxed selection fits, or the plan says it cannot"
    ~print:show_shape gen_shape (fun s ->
      match plan_shape s with
      | Error ds ->
          List.for_all
            (fun d -> Foundation.Diagnostic.code d = "image.relax-unreachable")
            (Foundation.Diag.diagnostics ds)
      | Ok l -> (
          match Image.bind_image l ~addresses:[ (".text", 0x400000L) ] with
          | Ok _ -> true
          | Error _ -> false))

let prop_relax_stable =
  (* Stability: the fixpoint's answer is a fixed point. Re-planning the same
     module reaches the same selections, so nothing about the result depends on
     how many iterations it took to get there. Compared by the bound bytes,
     which is the only thing a selection can change. *)
  Q.Test.make ~count:500 ~name:"relaxation reaches a fixed point" ~print:show_shape gen_shape
    (fun s ->
      let bytes_of () =
        match plan_shape s with
        | Error _ -> None
        | Ok l -> (
            match Image.bind_image l ~addresses:[ (".text", 0x400000L) ] with
            | Error _ -> None
            | Ok img -> ( match img.Image.segments with s :: _ -> Some s.Image.bytes | [] -> None))
      in
      bytes_of () = bytes_of ())

(* {1 Form selection}

   Monotone in distance, which is the property that makes a promotion-only
   fixpoint the right algorithm: a branch whose target moves further away never
   goes back to a shorter form. Stated over the real x86-64 assembler rather
   than the synthetic ladder, because this is where the rung labels and the
   range checks are the production ones. *)

let rung_at ~gap =
  let filler = String.concat "" (List.init gap (fun _ -> "\tret\n")) in
  let text = Printf.sprintf "\t.text\nentry:\n\tjmp far\n%sfar:\n\tret\n" filler in
  match Driver.Portable.assemble "x86_64" ~unit_name:"p" ~text with
  | Error _ -> None
  | Ok p -> (
      match Driver.Portable.bind_at p ~base:0x400000L with
      | Error _ -> None
      | Ok b -> (
          match Driver.Portable.section_bytes b ".text" with
          | Some s when String.length s > 0 -> Some (Char.code s.[0])
          | _ -> None))

let prop_selection_monotone =
  (* 0xeb is the short opcode and 0xe9 the near one, so the byte *is* the
     selection. The pair is generated ordered, and the law is that once the
     assembler has gone near it stays near. *)
  Q.Test.make ~count:120 ~name:"a branch that has gone near does not come back short"
    ~print:(fun (a, b) -> Printf.sprintf "gaps %d and %d" a b)
    Q.Gen.(
      let* a = int_range 0 140 in
      let* d = int_range 0 40 in
      return (a, a + d))
    (fun (a, b) ->
      match (rung_at ~gap:a, rung_at ~gap:b) with
      | Some x, Some y -> not (x = 0xe9 && y = 0xeb)
      | _ -> false)

let () =
  Printf.printf "asm property tests, seed %d\n%!" seed;
  let rand = Random.State.make [| seed |] in
  exit
    (QCheck_base_runner.run_tests ~colors:false ~verbose:false ~rand
       [
         prop_encode_decode;
         prop_decode_reencode;
         prop_split_unsigned;
         prop_split_signed;
         prop_relax_feasible;
         prop_relax_stable;
         prop_selection_monotone;
       ])
