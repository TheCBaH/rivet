(* Hand-written single- and double-precision floating-point forms; the generated rows with feature
   f or d are added by {!Riscv_ext.component}. The fneg/fabs/fmv spellings are GNU aliases and cite the
   record they expand to. *)

let f_forms =
  [
    ("flw", "rv_f/flw");
    ("fsw", "rv_f/fsw");
    ("fadd.s", "rv_f/fadd.s");
    ("fsub.s", "rv_f/fsub.s");
    ("fmul.s", "rv_f/fmul.s");
    ("fdiv.s", "rv_f/fdiv.s");
    ("fsgnj.s", "rv_f/fsgnj.s");
    ("fsgnjn.s", "rv_f/fsgnjn.s");
    ("fsgnjx.s", "rv_f/fsgnjx.s");
    ("fmin.s", "rv_f/fmin.s");
    ("fmax.s", "rv_f/fmax.s");
    ("fsqrt.s", "rv_f/fsqrt.s");
    ("fclass.s", "rv_f/fclass.s");
    ("fmadd.s", "rv_f/fmadd.s");
    ("fmsub.s", "rv_f/fmsub.s");
    ("fnmsub.s", "rv_f/fnmsub.s");
    ("fnmadd.s", "rv_f/fnmadd.s");
    ("flt.s", "rv_f/flt.s");
    ("feq.s", "rv_f/feq.s");
    ("fle.s", "rv_f/fle.s");
    ("fmv.x.w", "rv_f/fmv.x.w");
    ("fmv.w.x", "rv_f/fmv.w.x");
    ("fcvt.s.w", "rv_f/fcvt.s.w");
    ("fcvt.s.wu", "rv_f/fcvt.s.wu");
    ("fcvt.w.s", "rv_f/fcvt.w.s");
    ("fcvt.wu.s", "rv_f/fcvt.wu.s");
    ("fcvt.s.l", "rv_f/fcvt.s.l");
    ("fcvt.l.s", "rv_f/fcvt.l.s");
    ("fcvt.lu.s", "rv_f/fcvt.lu.s");
    ("fcvt.s.lu", "rv_f/fcvt.s.lu");
    ("fneg.s", "rv_f/fsgnjn.s");
    ("fabs.s", "rv_f/fsgnjx.s");
    ("fmv.s", "rv_f/fsgnj.s");
    ("fmv.x.s", "rv_f/fmv.x.w");
    ("fmv.s.x", "rv_f/fmv.w.x");
  ]

let d_forms =
  [
    ("fld", "rv_d/fld");
    ("fsd", "rv_d/fsd");
    ("fadd.d", "rv_d/fadd.d");
    ("fsub.d", "rv_d/fsub.d");
    ("fmul.d", "rv_d/fmul.d");
    ("fdiv.d", "rv_d/fdiv.d");
    ("fsgnj.d", "rv_d/fsgnj.d");
    ("fsgnjn.d", "rv_d/fsgnjn.d");
    ("fsgnjx.d", "rv_d/fsgnjx.d");
    ("fmin.d", "rv_d/fmin.d");
    ("fmax.d", "rv_d/fmax.d");
    ("fsqrt.d", "rv_d/fsqrt.d");
    ("fclass.d", "rv_d/fclass.d");
    ("fmadd.d", "rv_d/fmadd.d");
    ("fmsub.d", "rv_d/fmsub.d");
    ("fnmsub.d", "rv_d/fnmsub.d");
    ("fnmadd.d", "rv_d/fnmadd.d");
    ("fmv.x.d", "rv_d/fmv.x.d");
    ("feq.d", "rv_d/feq.d");
    ("fle.d", "rv_d/fle.d");
    ("flt.d", "rv_d/flt.d");
    ("fcvt.w.d", "rv_d/fcvt.w.d");
    ("fcvt.wu.d", "rv_d/fcvt.wu.d");
    ("fcvt.l.d", "rv_d/fcvt.l.d");
    ("fcvt.d.w", "rv_d/fcvt.d.w");
    ("fcvt.d.wu", "rv_d/fcvt.d.wu");
    ("fcvt.s.d", "rv_d/fcvt.s.d");
    ("fcvt.d.s", "rv_d/fcvt.d.s");
    ("fcvt.lu.d", "rv_d/fcvt.lu.d");
    ("fcvt.d.l", "rv_d/fcvt.d.l");
    ("fcvt.d.lu", "rv_d/fcvt.d.lu");
    ("fneg.d", "rv_d/fsgnjn.d");
    ("fabs.d", "rv_d/fsgnjx.d");
    ("fmv.d", "rv_d/fsgnj.d");
  ]

let f =
  Riscv_ext.component ~id:"riscv.f" ~feature:"f" ~summary:"single-precision floating point" f_forms

let d =
  Riscv_ext.component ~id:"riscv.d" ~feature:"d" ~requires:[ "f" ]
    ~summary:"double-precision floating point" d_forms
