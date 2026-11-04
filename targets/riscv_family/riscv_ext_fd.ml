(* Hand-written single- and double-precision floating-point forms; the generated rows with feature
   f or d are added by {!Riscv_ext.component}. The fneg/fabs/fmv spellings are GNU aliases and cite the
   record they expand to. *)

module Opcode = Riscv_types.Opcode

let f_forms =
  [
    (Opcode.Flw, "rv_f/flw");
    (Opcode.Fsw, "rv_f/fsw");
    (Opcode.Fadd_s, "rv_f/fadd.s");
    (Opcode.Fsub_s, "rv_f/fsub.s");
    (Opcode.Fmul_s, "rv_f/fmul.s");
    (Opcode.Fdiv_s, "rv_f/fdiv.s");
    (Opcode.Fsgnj_s, "rv_f/fsgnj.s");
    (Opcode.Fsgnjn_s, "rv_f/fsgnjn.s");
    (Opcode.Fsgnjx_s, "rv_f/fsgnjx.s");
    (Opcode.Fmin_s, "rv_f/fmin.s");
    (Opcode.Fmax_s, "rv_f/fmax.s");
    (Opcode.Fsqrt_s, "rv_f/fsqrt.s");
    (Opcode.Fclass_s, "rv_f/fclass.s");
    (Opcode.Fmadd_s, "rv_f/fmadd.s");
    (Opcode.Fmsub_s, "rv_f/fmsub.s");
    (Opcode.Fnmsub_s, "rv_f/fnmsub.s");
    (Opcode.Fnmadd_s, "rv_f/fnmadd.s");
    (Opcode.Flt_s, "rv_f/flt.s");
    (Opcode.Feq_s, "rv_f/feq.s");
    (Opcode.Fle_s, "rv_f/fle.s");
    (Opcode.Fmv_x_w, "rv_f/fmv.x.w");
    (Opcode.Fmv_w_x, "rv_f/fmv.w.x");
    (Opcode.Fcvt_s_w, "rv_f/fcvt.s.w");
    (Opcode.Fcvt_s_wu, "rv_f/fcvt.s.wu");
    (Opcode.Fcvt_w_s, "rv_f/fcvt.w.s");
    (Opcode.Fcvt_wu_s, "rv_f/fcvt.wu.s");
    (Opcode.Fcvt_s_l, "rv_f/fcvt.s.l");
    (Opcode.Fcvt_l_s, "rv_f/fcvt.l.s");
    (Opcode.Fcvt_lu_s, "rv_f/fcvt.lu.s");
    (Opcode.Fcvt_s_lu, "rv_f/fcvt.s.lu");
    (Opcode.Fneg_s, "rv_f/fsgnjn.s");
    (Opcode.Fabs_s, "rv_f/fsgnjx.s");
    (Opcode.Fmv_s, "rv_f/fsgnj.s");
    (Opcode.Fmv_x_s, "rv_f/fmv.x.w");
    (Opcode.Fmv_s_x, "rv_f/fmv.w.x");
  ]

let d_forms =
  [
    (Opcode.Fld, "rv_d/fld");
    (Opcode.Fsd, "rv_d/fsd");
    (Opcode.Fadd_d, "rv_d/fadd.d");
    (Opcode.Fsub_d, "rv_d/fsub.d");
    (Opcode.Fmul_d, "rv_d/fmul.d");
    (Opcode.Fdiv_d, "rv_d/fdiv.d");
    (Opcode.Fsgnj_d, "rv_d/fsgnj.d");
    (Opcode.Fsgnjn_d, "rv_d/fsgnjn.d");
    (Opcode.Fsgnjx_d, "rv_d/fsgnjx.d");
    (Opcode.Fmin_d, "rv_d/fmin.d");
    (Opcode.Fmax_d, "rv_d/fmax.d");
    (Opcode.Fsqrt_d, "rv_d/fsqrt.d");
    (Opcode.Fclass_d, "rv_d/fclass.d");
    (Opcode.Fmadd_d, "rv_d/fmadd.d");
    (Opcode.Fmsub_d, "rv_d/fmsub.d");
    (Opcode.Fnmsub_d, "rv_d/fnmsub.d");
    (Opcode.Fnmadd_d, "rv_d/fnmadd.d");
    (Opcode.Fmv_x_d, "rv_d/fmv.x.d");
    (Opcode.Feq_d, "rv_d/feq.d");
    (Opcode.Fle_d, "rv_d/fle.d");
    (Opcode.Flt_d, "rv_d/flt.d");
    (Opcode.Fcvt_w_d, "rv_d/fcvt.w.d");
    (Opcode.Fcvt_wu_d, "rv_d/fcvt.wu.d");
    (Opcode.Fcvt_l_d, "rv_d/fcvt.l.d");
    (Opcode.Fcvt_d_w, "rv_d/fcvt.d.w");
    (Opcode.Fcvt_d_wu, "rv_d/fcvt.d.wu");
    (Opcode.Fcvt_s_d, "rv_d/fcvt.s.d");
    (Opcode.Fcvt_d_s, "rv_d/fcvt.d.s");
    (Opcode.Fcvt_lu_d, "rv_d/fcvt.lu.d");
    (Opcode.Fcvt_d_l, "rv_d/fcvt.d.l");
    (Opcode.Fcvt_d_lu, "rv_d/fcvt.d.lu");
    (Opcode.Fneg_d, "rv_d/fsgnjn.d");
    (Opcode.Fabs_d, "rv_d/fsgnjx.d");
    (Opcode.Fmv_d, "rv_d/fsgnj.d");
  ]

let f =
  Riscv_ext.component ~id:"riscv.f" ~feature:"f" ~requires:[ "zicsr" ]
    ~summary:"single-precision floating point" f_forms

let d =
  Riscv_ext.component ~id:"riscv.d" ~feature:"d" ~requires:[ "f" ]
    ~summary:"double-precision floating point" d_forms
