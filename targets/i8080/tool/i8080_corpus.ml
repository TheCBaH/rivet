(* Every operand shape of every documented instruction, built with the typed
   builders, with the boundary values of its immediates. The unit tests check
   each against the text assembler and the oracle leg against asl. *)

open I8080_asm

let regs = [ b; c; d; e; h; l; m; a ]
let pairs : pair list = [ bc; de; hl; sp ]
let stacks : stack_pair list = [ bc; de; hl; psw ]
let inds : ind_pair list = [ bc; de ]
let d8 = [ 0; 1; 2; 0x3f; 0x7f; 0x80; 0xff; -1; -128 ]
let ports = [ 0; 1; 0x7f; 0x80; 0xff ]
let d16 = [ 0; 1; 0xff; 0x100; 0x1234; 0x7fff; 0x8000; 0xffff; -1; -32768 ]
let over xs f = List.concat_map f xs

let nullary =
  List.map
    (fun f -> f ())
    [
      nop;
      hlt;
      ret;
      rnz;
      rz;
      rnc;
      rc;
      rpo;
      rpe;
      rp;
      rm;
      xchg;
      xthl;
      sphl;
      pchl;
      ei;
      di;
      daa;
      cma;
      stc;
      cmc;
      rlc;
      rrc;
      ral;
      rar;
    ]

let unary_reg = [ inr; dcr; add; adc; sub; sbb; ana; xra; ora; cmp ]
let imm8 = [ adi; aci; sui; sbi; ani; xri; ori; cpi ]
let port = [ in_; out ]

let addr =
  [
    jmp;
    jnz;
    jz;
    jnc;
    jc;
    jpo;
    jpe;
    jp;
    jm;
    call;
    cnz;
    cz;
    cnc;
    cc;
    cpo;
    cpe;
    cp;
    cm;
    sta;
    lda;
    shld;
    lhld;
  ]

let all =
  List.concat
    [
      nullary;
      over unary_reg (fun f -> List.map f regs);
      over regs (fun x ->
          List.filter_map (fun y -> if x == m && y == m then None else Some (mov x y)) regs);
      over regs (fun r -> List.map (mvi r) d8);
      over pairs (fun p -> List.map (lxi p) d16);
      over [ inx; dcx; dad ] (fun f -> List.map f pairs);
      over [ ldax; stax ] (fun f -> List.map f inds);
      over [ push; pop ] (fun f -> List.map f stacks);
      List.init 8 rst;
      over imm8 (fun f -> List.map f d8);
      over port (fun f -> List.map f ports);
      over addr (fun f -> List.map f d16);
    ]

(* The same shapes with a symbol where an immediate or address goes. *)
let symbolic =
  let s = symbol "t" in
  List.concat
    [
      List.map (fun r -> mvi_e r s) regs;
      List.map (fun p -> lxi_e p s) pairs;
      List.map
        (fun f -> f s)
        [ adi_e; aci_e; sui_e; sbi_e; ani_e; xri_e; ori_e; cpi_e; in_e; out_e ];
      List.map
        (fun f -> f s)
        [
          jmp_e;
          jnz_e;
          jz_e;
          jnc_e;
          jc_e;
          jpo_e;
          jpe_e;
          jp_e;
          jm_e;
          call_e;
          cnz_e;
          cz_e;
          cnc_e;
          cc_e;
          cpo_e;
          cpe_e;
          cp_e;
          cm_e;
          sta_e;
          lda_e;
          shld_e;
          lhld_e;
        ];
    ]
