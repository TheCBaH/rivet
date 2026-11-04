(* Hand-written single- and double-precision floating-point forms; the generated rows with feature
   f or d are added by {!Riscv_ext.component}. The fneg/fabs/fmv spellings are GNU aliases and cite the
   record they expand to. *)

let f_forms = Riscv_ops.forms "f"
let d_forms = Riscv_ops.forms "d"

let f =
  Riscv_ext.component ~id:"riscv.f" ~feature:"f" ~requires:[ "zicsr" ]
    ~summary:"single-precision floating point" f_forms

let d =
  Riscv_ext.component ~id:"riscv.d" ~feature:"d" ~requires:[ "f" ]
    ~summary:"double-precision floating point" d_forms
