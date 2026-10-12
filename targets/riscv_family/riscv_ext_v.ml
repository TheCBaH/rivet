(* The vector extension and its crypto and BF16 subsets. Zvkb is the common part of Zvbb and the
   Zvkn and Zvks suites, so the forms they share are legal under either leaf. Zvkn and Zvks and
   their sums are implication-only groups, off by default. *)

module Req = Target_component.Req

let zvkb_forms =
  [
    "vandn.vv";
    "vandn.vx";
    "vbrev8.v";
    "vrev8.v";
    "vrol.vv";
    "vrol.vx";
    "vror.vv";
    "vror.vx";
    "vror.vi";
  ]

let legal mnemonic =
  if List.mem mnemonic zvkb_forms then Some (Req.Any [ Feature "zvbb"; Feature "zvkb" ]) else None

let v_forms =
  [
    ("vsetvl", "rv_v/vsetvl");
    ("vsetvli", "rv_v/vsetvli");
    ("vsetivli", "rv_v/vsetivli");
    ("vadd.vv", "rv_v/vadd.vv");
    ("vadd.vx", "rv_v/vadd.vx");
    ("vadd.vi", "rv_v/vadd.vi");
    ("vsub.vv", "rv_v/vsub.vv");
    ("vsub.vx", "rv_v/vsub.vx");
    ("vrsub.vx", "rv_v/vrsub.vx");
    ("vrsub.vi", "rv_v/vrsub.vi");
    ("vand.vv", "rv_v/vand.vv");
    ("vand.vx", "rv_v/vand.vx");
    ("vand.vi", "rv_v/vand.vi");
    ("vor.vv", "rv_v/vor.vv");
    ("vor.vx", "rv_v/vor.vx");
    ("vor.vi", "rv_v/vor.vi");
    ("vxor.vv", "rv_v/vxor.vv");
    ("vxor.vx", "rv_v/vxor.vx");
    ("vxor.vi", "rv_v/vxor.vi");
    ("vsll.vv", "rv_v/vsll.vv");
    ("vsll.vx", "rv_v/vsll.vx");
    ("vsll.vi", "rv_v/vsll.vi");
    ("vsrl.vv", "rv_v/vsrl.vv");
    ("vsrl.vx", "rv_v/vsrl.vx");
    ("vsrl.vi", "rv_v/vsrl.vi");
    ("vsra.vv", "rv_v/vsra.vv");
    ("vsra.vx", "rv_v/vsra.vx");
    ("vsra.vi", "rv_v/vsra.vi");
    ("vminu.vv", "rv_v/vminu.vv");
    ("vminu.vx", "rv_v/vminu.vx");
    ("vmin.vv", "rv_v/vmin.vv");
    ("vmin.vx", "rv_v/vmin.vx");
    ("vmaxu.vv", "rv_v/vmaxu.vv");
    ("vmaxu.vx", "rv_v/vmaxu.vx");
    ("vmax.vv", "rv_v/vmax.vv");
    ("vmax.vx", "rv_v/vmax.vx");
    ("vmul.vv", "rv_v/vmul.vv");
    ("vmul.vx", "rv_v/vmul.vx");
    ("vmulh.vv", "rv_v/vmulh.vv");
    ("vmulh.vx", "rv_v/vmulh.vx");
    ("vmulhu.vv", "rv_v/vmulhu.vv");
    ("vmulhu.vx", "rv_v/vmulhu.vx");
    ("vmulhsu.vv", "rv_v/vmulhsu.vv");
    ("vmulhsu.vx", "rv_v/vmulhsu.vx");
    ("vdivu.vv", "rv_v/vdivu.vv");
    ("vdivu.vx", "rv_v/vdivu.vx");
    ("vdiv.vv", "rv_v/vdiv.vv");
    ("vdiv.vx", "rv_v/vdiv.vx");
    ("vremu.vv", "rv_v/vremu.vv");
    ("vremu.vx", "rv_v/vremu.vx");
    ("vrem.vv", "rv_v/vrem.vv");
    ("vrem.vx", "rv_v/vrem.vx");
    ("vaadd.vv", "rv_v/vaadd.vv");
    ("vaadd.vx", "rv_v/vaadd.vx");
    ("vaaddu.vv", "rv_v/vaaddu.vv");
    ("vaaddu.vx", "rv_v/vaaddu.vx");
    ("vasub.vv", "rv_v/vasub.vv");
    ("vasub.vx", "rv_v/vasub.vx");
    ("vasubu.vv", "rv_v/vasubu.vv");
    ("vasubu.vx", "rv_v/vasubu.vx");
    ("vnclip.wv", "rv_v/vnclip.wv");
    ("vnclip.wx", "rv_v/vnclip.wx");
    ("vnclip.wi", "rv_v/vnclip.wi");
    ("vnclipu.wv", "rv_v/vnclipu.wv");
    ("vnclipu.wx", "rv_v/vnclipu.wx");
    ("vnclipu.wi", "rv_v/vnclipu.wi");
    ("vnsra.wv", "rv_v/vnsra.wv");
    ("vnsra.wx", "rv_v/vnsra.wx");
    ("vnsra.wi", "rv_v/vnsra.wi");
    ("vnsrl.wv", "rv_v/vnsrl.wv");
    ("vnsrl.wx", "rv_v/vnsrl.wx");
    ("vnsrl.wi", "rv_v/vnsrl.wi");
    ("vssrl.vv", "rv_v/vssrl.vv");
    ("vssrl.vx", "rv_v/vssrl.vx");
    ("vssrl.vi", "rv_v/vssrl.vi");
    ("vssra.vv", "rv_v/vssra.vv");
    ("vssra.vx", "rv_v/vssra.vx");
    ("vssra.vi", "rv_v/vssra.vi");
    ("vrgather.vv", "rv_v/vrgather.vv");
    ("vrgather.vx", "rv_v/vrgather.vx");
    ("vrgather.vi", "rv_v/vrgather.vi");
    ("vrgatherei16.vv", "rv_v/vrgatherei16.vv");
    ("vwaddu.vv", "rv_v/vwaddu.vv");
    ("vwaddu.vx", "rv_v/vwaddu.vx");
    ("vwadd.vv", "rv_v/vwadd.vv");
    ("vwadd.vx", "rv_v/vwadd.vx");
    ("vwsubu.vv", "rv_v/vwsubu.vv");
    ("vwsubu.vx", "rv_v/vwsubu.vx");
    ("vwsub.vv", "rv_v/vwsub.vv");
    ("vwsub.vx", "rv_v/vwsub.vx");
    ("vwaddu.wv", "rv_v/vwaddu.wv");
    ("vwaddu.wx", "rv_v/vwaddu.wx");
    ("vwadd.wv", "rv_v/vwadd.wv");
    ("vwadd.wx", "rv_v/vwadd.wx");
    ("vwsubu.wv", "rv_v/vwsubu.wv");
    ("vwsubu.wx", "rv_v/vwsubu.wx");
    ("vwsub.wv", "rv_v/vwsub.wv");
    ("vwsub.wx", "rv_v/vwsub.wx");
    ("vwmulu.vv", "rv_v/vwmulu.vv");
    ("vwmulu.vx", "rv_v/vwmulu.vx");
    ("vwmulsu.vv", "rv_v/vwmulsu.vv");
    ("vwmulsu.vx", "rv_v/vwmulsu.vx");
    ("vwmul.vv", "rv_v/vwmul.vv");
    ("vwmul.vx", "rv_v/vwmul.vx");
    ("vsext.vf2", "rv_v/vsext.vf2");
    ("vsext.vf4", "rv_v/vsext.vf4");
    ("vsext.vf8", "rv_v/vsext.vf8");
    ("vzext.vf2", "rv_v/vzext.vf2");
    ("vzext.vf4", "rv_v/vzext.vf4");
    ("vzext.vf8", "rv_v/vzext.vf8");
    ("vsadd.vv", "rv_v/vsadd.vv");
    ("vsadd.vx", "rv_v/vsadd.vx");
    ("vsadd.vi", "rv_v/vsadd.vi");
    ("vsaddu.vv", "rv_v/vsaddu.vv");
    ("vsaddu.vx", "rv_v/vsaddu.vx");
    ("vsaddu.vi", "rv_v/vsaddu.vi");
    ("vssub.vv", "rv_v/vssub.vv");
    ("vssub.vx", "rv_v/vssub.vx");
    ("vssubu.vv", "rv_v/vssubu.vv");
    ("vssubu.vx", "rv_v/vssubu.vx");
    ("vmand.mm", "rv_v/vmand.mm");
    ("vmandn.mm", "rv_v/vmandn.mm");
    ("vmor.mm", "rv_v/vmor.mm");
    ("vmxor.mm", "rv_v/vmxor.mm");
    ("vmorn.mm", "rv_v/vmorn.mm");
    ("vmnand.mm", "rv_v/vmnand.mm");
    ("vmnor.mm", "rv_v/vmnor.mm");
    ("vmxnor.mm", "rv_v/vmxnor.mm");
    ("vredsum.vs", "rv_v/vredsum.vs");
    ("vredand.vs", "rv_v/vredand.vs");
    ("vredor.vs", "rv_v/vredor.vs");
    ("vredxor.vs", "rv_v/vredxor.vs");
    ("vredminu.vs", "rv_v/vredminu.vs");
    ("vredmin.vs", "rv_v/vredmin.vs");
    ("vredmaxu.vs", "rv_v/vredmaxu.vs");
    ("vredmax.vs", "rv_v/vredmax.vs");
    ("vwredsumu.vs", "rv_v/vwredsumu.vs");
    ("vwredsum.vs", "rv_v/vwredsum.vs");
    ("vmseq.vv", "rv_v/vmseq.vv");
    ("vmseq.vx", "rv_v/vmseq.vx");
    ("vmseq.vi", "rv_v/vmseq.vi");
    ("vmsne.vv", "rv_v/vmsne.vv");
    ("vmsne.vx", "rv_v/vmsne.vx");
    ("vmsne.vi", "rv_v/vmsne.vi");
    ("vmsltu.vv", "rv_v/vmsltu.vv");
    ("vmsltu.vx", "rv_v/vmsltu.vx");
    ("vmslt.vv", "rv_v/vmslt.vv");
    ("vmslt.vx", "rv_v/vmslt.vx");
    ("vmsleu.vv", "rv_v/vmsleu.vv");
    ("vmsleu.vx", "rv_v/vmsleu.vx");
    ("vmsleu.vi", "rv_v/vmsleu.vi");
    ("vmsle.vv", "rv_v/vmsle.vv");
    ("vmsle.vx", "rv_v/vmsle.vx");
    ("vmsle.vi", "rv_v/vmsle.vi");
    ("vmsgtu.vx", "rv_v/vmsgtu.vx");
    ("vmsgtu.vi", "rv_v/vmsgtu.vi");
    ("vmsgt.vx", "rv_v/vmsgt.vx");
    ("vmsgt.vi", "rv_v/vmsgt.vi");
    ("vslideup.vx", "rv_v/vslideup.vx");
    ("vslideup.vi", "rv_v/vslideup.vi");
    ("vslidedown.vx", "rv_v/vslidedown.vx");
    ("vslidedown.vi", "rv_v/vslidedown.vi");
    ("vslide1up.vx", "rv_v/vslide1up.vx");
    ("vslide1down.vx", "rv_v/vslide1down.vx");
    ("vmacc.vv", "rv_v/vmacc.vv");
    ("vmacc.vx", "rv_v/vmacc.vx");
    ("vnmsac.vv", "rv_v/vnmsac.vv");
    ("vnmsac.vx", "rv_v/vnmsac.vx");
    ("vmadd.vv", "rv_v/vmadd.vv");
    ("vmadd.vx", "rv_v/vmadd.vx");
    ("vnmsub.vv", "rv_v/vnmsub.vv");
    ("vnmsub.vx", "rv_v/vnmsub.vx");
    ("vwmaccu.vv", "rv_v/vwmaccu.vv");
    ("vwmaccu.vx", "rv_v/vwmaccu.vx");
    ("vwmacc.vv", "rv_v/vwmacc.vv");
    ("vwmacc.vx", "rv_v/vwmacc.vx");
    ("vwmaccsu.vv", "rv_v/vwmaccsu.vv");
    ("vwmaccsu.vx", "rv_v/vwmaccsu.vx");
    ("vwmaccus.vx", "rv_v/vwmaccus.vx");
    ("vid.v", "rv_v/vid.v");
    ("viota.m", "rv_v/viota.m");
    ("vcompress.vm", "rv_v/vcompress.vm");
    ("vcpop.m", "rv_v/vcpop.m");
    ("vfirst.m", "rv_v/vfirst.m");
    ("vmsbf.m", "rv_v/vmsbf.m");
    ("vmsif.m", "rv_v/vmsif.m");
    ("vmsof.m", "rv_v/vmsof.m");
    ("vadc.vvm", "rv_v/vadc.vvm");
    ("vadc.vxm", "rv_v/vadc.vxm");
    ("vadc.vim", "rv_v/vadc.vim");
    ("vmadc.vvm", "rv_v/vmadc.vvm");
    ("vmadc.vxm", "rv_v/vmadc.vxm");
    ("vmadc.vim", "rv_v/vmadc.vim");
    ("vmadc.vv", "rv_v/vmadc.vv");
    ("vmadc.vx", "rv_v/vmadc.vx");
    ("vmadc.vi", "rv_v/vmadc.vi");
    ("vsbc.vvm", "rv_v/vsbc.vvm");
    ("vsbc.vxm", "rv_v/vsbc.vxm");
    ("vmsbc.vvm", "rv_v/vmsbc.vvm");
    ("vmsbc.vxm", "rv_v/vmsbc.vxm");
    ("vmsbc.vv", "rv_v/vmsbc.vv");
    ("vmsbc.vx", "rv_v/vmsbc.vx");
    ("vmerge.vvm", "rv_v/vmerge.vvm");
    ("vmerge.vxm", "rv_v/vmerge.vxm");
    ("vmerge.vim", "rv_v/vmerge.vim");
    ("vmv.s.x", "rv_v/vmv.s.x");
    ("vmv.x.s", "rv_v/vmv.x.s");
    ("vmv.v.i", "rv_v/vmv.v.i");
    ("vmv.v.v", "rv_v/vmv.v.v");
    ("vmv.v.x", "rv_v/vmv.v.x");
    ("vmv1r.v", "rv_v/vmv1r.v");
    ("vmv2r.v", "rv_v/vmv2r.v");
    ("vmv4r.v", "rv_v/vmv4r.v");
    ("vmv8r.v", "rv_v/vmv8r.v");
    ("vsmul.vv", "rv_v/vsmul.vv");
    ("vsmul.vx", "rv_v/vsmul.vx");
    ("vfadd.vv", "rv_v/vfadd.vv");
    ("vfadd.vf", "rv_v/vfadd.vf");
    ("vfsub.vv", "rv_v/vfsub.vv");
    ("vfsub.vf", "rv_v/vfsub.vf");
    ("vfrsub.vf", "rv_v/vfrsub.vf");
    ("vfmul.vv", "rv_v/vfmul.vv");
    ("vfmul.vf", "rv_v/vfmul.vf");
    ("vfdiv.vv", "rv_v/vfdiv.vv");
    ("vfdiv.vf", "rv_v/vfdiv.vf");
    ("vfrdiv.vf", "rv_v/vfrdiv.vf");
    ("vfmin.vv", "rv_v/vfmin.vv");
    ("vfmin.vf", "rv_v/vfmin.vf");
    ("vfmax.vv", "rv_v/vfmax.vv");
    ("vfmax.vf", "rv_v/vfmax.vf");
    ("vfsgnj.vv", "rv_v/vfsgnj.vv");
    ("vfsgnj.vf", "rv_v/vfsgnj.vf");
    ("vfsgnjn.vv", "rv_v/vfsgnjn.vv");
    ("vfsgnjn.vf", "rv_v/vfsgnjn.vf");
    ("vfsgnjx.vv", "rv_v/vfsgnjx.vv");
    ("vfsgnjx.vf", "rv_v/vfsgnjx.vf");
    ("vfsqrt.v", "rv_v/vfsqrt.v");
    ("vfrsqrt7.v", "rv_v/vfrsqrt7.v");
    ("vfrec7.v", "rv_v/vfrec7.v");
    ("vfclass.v", "rv_v/vfclass.v");
    ("vfredosum.vs", "rv_v/vfredosum.vs");
    ("vfredusum.vs", "rv_v/vfredusum.vs");
    ("vfredmin.vs", "rv_v/vfredmin.vs");
    ("vfredmax.vs", "rv_v/vfredmax.vs");
    ("vmfeq.vv", "rv_v/vmfeq.vv");
    ("vmfeq.vf", "rv_v/vmfeq.vf");
    ("vmfle.vv", "rv_v/vmfle.vv");
    ("vmfle.vf", "rv_v/vmfle.vf");
    ("vmflt.vv", "rv_v/vmflt.vv");
    ("vmflt.vf", "rv_v/vmflt.vf");
    ("vmfne.vv", "rv_v/vmfne.vv");
    ("vmfne.vf", "rv_v/vmfne.vf");
    ("vmfgt.vf", "rv_v/vmfgt.vf");
    ("vmfge.vf", "rv_v/vmfge.vf");
    ("vfmv.f.s", "rv_v/vfmv.f.s");
    ("vfmv.s.f", "rv_v/vfmv.s.f");
    ("vfmv.v.f", "rv_v/vfmv.v.f");
    ("vfmerge.vfm", "rv_v/vfmerge.vfm");
    ("vfcvt.xu.f.v", "rv_v/vfcvt.xu.f.v");
    ("vfcvt.x.f.v", "rv_v/vfcvt.x.f.v");
    ("vfcvt.f.xu.v", "rv_v/vfcvt.f.xu.v");
    ("vfcvt.f.x.v", "rv_v/vfcvt.f.x.v");
    ("vfcvt.rtz.xu.f.v", "rv_v/vfcvt.rtz.xu.f.v");
    ("vfcvt.rtz.x.f.v", "rv_v/vfcvt.rtz.x.f.v");
    ("vfmadd.vv", "rv_v/vfmadd.vv");
    ("vfmadd.vf", "rv_v/vfmadd.vf");
    ("vfnmadd.vv", "rv_v/vfnmadd.vv");
    ("vfnmadd.vf", "rv_v/vfnmadd.vf");
    ("vfmsub.vv", "rv_v/vfmsub.vv");
    ("vfmsub.vf", "rv_v/vfmsub.vf");
    ("vfnmsub.vv", "rv_v/vfnmsub.vv");
    ("vfnmsub.vf", "rv_v/vfnmsub.vf");
    ("vfmacc.vv", "rv_v/vfmacc.vv");
    ("vfmacc.vf", "rv_v/vfmacc.vf");
    ("vfnmacc.vv", "rv_v/vfnmacc.vv");
    ("vfnmacc.vf", "rv_v/vfnmacc.vf");
    ("vfmsac.vv", "rv_v/vfmsac.vv");
    ("vfmsac.vf", "rv_v/vfmsac.vf");
    ("vfnmsac.vv", "rv_v/vfnmsac.vv");
    ("vfnmsac.vf", "rv_v/vfnmsac.vf");
    ("vfslide1up.vf", "rv_v/vfslide1up.vf");
    ("vfslide1down.vf", "rv_v/vfslide1down.vf");
    ("vfwadd.vv", "rv_v/vfwadd.vv");
    ("vfwadd.vf", "rv_v/vfwadd.vf");
    ("vfwadd.wv", "rv_v/vfwadd.wv");
    ("vfwadd.wf", "rv_v/vfwadd.wf");
    ("vfwsub.vv", "rv_v/vfwsub.vv");
    ("vfwsub.vf", "rv_v/vfwsub.vf");
    ("vfwsub.wv", "rv_v/vfwsub.wv");
    ("vfwsub.wf", "rv_v/vfwsub.wf");
    ("vfwmul.vv", "rv_v/vfwmul.vv");
    ("vfwmul.vf", "rv_v/vfwmul.vf");
    ("vfwredosum.vs", "rv_v/vfwredosum.vs");
    ("vfwredusum.vs", "rv_v/vfwredusum.vs");
    ("vfwcvt.xu.f.v", "rv_v/vfwcvt.xu.f.v");
    ("vfwcvt.x.f.v", "rv_v/vfwcvt.x.f.v");
    ("vfwcvt.f.xu.v", "rv_v/vfwcvt.f.xu.v");
    ("vfwcvt.f.x.v", "rv_v/vfwcvt.f.x.v");
    ("vfwcvt.f.f.v", "rv_v/vfwcvt.f.f.v");
    ("vfwcvt.rtz.xu.f.v", "rv_v/vfwcvt.rtz.xu.f.v");
    ("vfwcvt.rtz.x.f.v", "rv_v/vfwcvt.rtz.x.f.v");
    ("vfncvt.xu.f.w", "rv_v/vfncvt.xu.f.w");
    ("vfncvt.x.f.w", "rv_v/vfncvt.x.f.w");
    ("vfncvt.f.xu.w", "rv_v/vfncvt.f.xu.w");
    ("vfncvt.f.x.w", "rv_v/vfncvt.f.x.w");
    ("vfncvt.f.f.w", "rv_v/vfncvt.f.f.w");
    ("vfncvt.rod.f.f.w", "rv_v/vfncvt.rod.f.f.w");
    ("vfncvt.rtz.xu.f.w", "rv_v/vfncvt.rtz.xu.f.w");
    ("vfncvt.rtz.x.f.w", "rv_v/vfncvt.rtz.x.f.w");
    ("vfwmacc.vv", "rv_v/vfwmacc.vv");
    ("vfwmacc.vf", "rv_v/vfwmacc.vf");
    ("vfwmsac.vv", "rv_v/vfwmsac.vv");
    ("vfwmsac.vf", "rv_v/vfwmsac.vf");
    ("vfwnmacc.vv", "rv_v/vfwnmacc.vv");
    ("vfwnmacc.vf", "rv_v/vfwnmacc.vf");
    ("vfwnmsac.vv", "rv_v/vfwnmsac.vv");
    ("vfwnmsac.vf", "rv_v/vfwnmsac.vf");
    ("vle8.v", "rv_v/vle8.v");
    ("vle16.v", "rv_v/vle16.v");
    ("vle32.v", "rv_v/vle32.v");
    ("vle64.v", "rv_v/vle64.v");
    ("vse8.v", "rv_v/vse8.v");
    ("vse16.v", "rv_v/vse16.v");
    ("vse32.v", "rv_v/vse32.v");
    ("vse64.v", "rv_v/vse64.v");
    ("vlm.v", "rv_v/vlm.v");
    ("vsm.v", "rv_v/vsm.v");
    ("vle8ff.v", "rv_v/vle8ff.v");
    ("vle16ff.v", "rv_v/vle16ff.v");
    ("vle32ff.v", "rv_v/vle32ff.v");
    ("vle64ff.v", "rv_v/vle64ff.v");
    ("vlse8.v", "rv_v/vlse8.v");
    ("vlse16.v", "rv_v/vlse16.v");
    ("vlse32.v", "rv_v/vlse32.v");
    ("vlse64.v", "rv_v/vlse64.v");
    ("vsse8.v", "rv_v/vsse8.v");
    ("vsse16.v", "rv_v/vsse16.v");
    ("vsse32.v", "rv_v/vsse32.v");
    ("vsse64.v", "rv_v/vsse64.v");
    ("vluxei8.v", "rv_v/vluxei8.v");
    ("vluxei16.v", "rv_v/vluxei16.v");
    ("vluxei32.v", "rv_v/vluxei32.v");
    ("vluxei64.v", "rv_v/vluxei64.v");
    ("vloxei8.v", "rv_v/vloxei8.v");
    ("vloxei16.v", "rv_v/vloxei16.v");
    ("vloxei32.v", "rv_v/vloxei32.v");
    ("vloxei64.v", "rv_v/vloxei64.v");
    ("vsuxei8.v", "rv_v/vsuxei8.v");
    ("vsuxei16.v", "rv_v/vsuxei16.v");
    ("vsuxei32.v", "rv_v/vsuxei32.v");
    ("vsuxei64.v", "rv_v/vsuxei64.v");
    ("vsoxei8.v", "rv_v/vsoxei8.v");
    ("vsoxei16.v", "rv_v/vsoxei16.v");
    ("vsoxei32.v", "rv_v/vsoxei32.v");
    ("vsoxei64.v", "rv_v/vsoxei64.v");
    ("vl1re8.v", "rv_v/vl1re8.v");
    ("vl1re16.v", "rv_v/vl1re16.v");
    ("vl1re32.v", "rv_v/vl1re32.v");
    ("vl1re64.v", "rv_v/vl1re64.v");
    ("vl2re8.v", "rv_v/vl2re8.v");
    ("vl2re16.v", "rv_v/vl2re16.v");
    ("vl2re32.v", "rv_v/vl2re32.v");
    ("vl2re64.v", "rv_v/vl2re64.v");
    ("vl4re8.v", "rv_v/vl4re8.v");
    ("vl4re16.v", "rv_v/vl4re16.v");
    ("vl4re32.v", "rv_v/vl4re32.v");
    ("vl4re64.v", "rv_v/vl4re64.v");
    ("vl8re8.v", "rv_v/vl8re8.v");
    ("vl8re16.v", "rv_v/vl8re16.v");
    ("vl8re32.v", "rv_v/vl8re32.v");
    ("vl8re64.v", "rv_v/vl8re64.v");
    ("vs1r.v", "rv_v/vs1r.v");
    ("vs2r.v", "rv_v/vs2r.v");
    ("vs4r.v", "rv_v/vs4r.v");
    ("vs8r.v", "rv_v/vs8r.v");
  ]

let zvbb_forms =
  [
    ("vandn.vv", "rv_zvbb/vandn.vv");
    ("vandn.vx", "rv_zvbb/vandn.vx");
    ("vbrev.v", "rv_zvbb/vbrev.v");
    ("vbrev8.v", "rv_zvbb/vbrev8.v");
    ("vclz.v", "rv_zvbb/vclz.v");
    ("vcpop.v", "rv_zvbb/vcpop.v");
    ("vctz.v", "rv_zvbb/vctz.v");
    ("vrev8.v", "rv_zvbb/vrev8.v");
    ("vrol.vv", "rv_zvbb/vrol.vv");
    ("vrol.vx", "rv_zvbb/vrol.vx");
    ("vror.vv", "rv_zvbb/vror.vv");
    ("vror.vx", "rv_zvbb/vror.vx");
    ("vror.vi", "rv_zvbb/vror.vi");
    ("vwsll.vv", "rv_zvbb/vwsll.vv");
    ("vwsll.vx", "rv_zvbb/vwsll.vx");
    ("vwsll.vi", "rv_zvbb/vwsll.vi");
  ]

let zvkned_forms =
  [
    ("vaesdf.vv", "rv_zvkned/vaesdf.vv");
    ("vaesdf.vs", "rv_zvkned/vaesdf.vs");
    ("vaesdm.vv", "rv_zvkned/vaesdm.vv");
    ("vaesdm.vs", "rv_zvkned/vaesdm.vs");
    ("vaesef.vv", "rv_zvkned/vaesef.vv");
    ("vaesef.vs", "rv_zvkned/vaesef.vs");
    ("vaesem.vv", "rv_zvkned/vaesem.vv");
    ("vaesem.vs", "rv_zvkned/vaesem.vs");
    ("vaesz.vs", "rv_zvkned/vaesz.vs");
    ("vaeskf1.vi", "rv_zvkned/vaeskf1.vi");
    ("vaeskf2.vi", "rv_zvkned/vaeskf2.vi");
  ]

let zvbc_forms =
  [
    ("vclmul.vv", "rv_zvbc/vclmul.vv");
    ("vclmul.vx", "rv_zvbc/vclmul.vx");
    ("vclmulh.vv", "rv_zvbc/vclmulh.vv");
    ("vclmulh.vx", "rv_zvbc/vclmulh.vx");
  ]

let zvknha_forms =
  [
    ("vsha2ms.vv", "rv_zvknha/vsha2ms.vv");
    ("vsha2ch.vv", "rv_zvknha/vsha2ch.vv");
    ("vsha2cl.vv", "rv_zvknha/vsha2cl.vv");
  ]

let zvksed_forms =
  [
    ("vsm4k.vi", "rv_zvksed/vsm4k.vi");
    ("vsm4r.vv", "rv_zvksed/vsm4r.vv");
    ("vsm4r.vs", "rv_zvksed/vsm4r.vs");
  ]

let zvkg_forms = [ ("vghsh.vv", "rv_zvkg/vghsh.vv"); ("vgmul.vv", "rv_zvkg/vgmul.vv") ]
let zvksh_forms = [ ("vsm3c.vi", "rv_zvksh/vsm3c.vi"); ("vsm3me.vv", "rv_zvksh/vsm3me.vv") ]

let zvfbfmin_forms =
  [
    ("vfwcvtbf16.f.f.v", "rv_zvfbfmin/vfwcvtbf16.f.f.v");
    ("vfncvtbf16.f.f.w", "rv_zvfbfmin/vfncvtbf16.f.f.w");
  ]

let zvfbfwma_forms =
  [
    ("vfwmaccbf16.vv", "rv_zvfbfwma/vfwmaccbf16.vv");
    ("vfwmaccbf16.vf", "rv_zvfbfwma/vfwmaccbf16.vf");
  ]

let v = Riscv_ext.component ~id:"riscv.v" ~feature:"v" ~requires:[ "d" ] ~summary:"vector" v_forms

let leaf feature ?(implies = []) summary forms =
  Riscv_ext.component ~id:("riscv." ^ feature) ~feature ~requires:[ "v" ] ~implies ~legal ~summary
    forms

let zvbb = leaf "zvbb" ~implies:[ "zvkb" ] "vector basic bit manipulation" zvbb_forms
let zvkned = leaf "zvkned" "vector AES block cipher" zvkned_forms
let zvbc = leaf "zvbc" "vector carry-less multiply" zvbc_forms
let zvknha = leaf "zvknha" "vector SHA-2 (SHA-256)" zvknha_forms
let zvksed = leaf "zvksed" "vector SM4 block cipher" zvksed_forms
let zvkg = leaf "zvkg" "vector GCM/GMAC" zvkg_forms
let zvksh = leaf "zvksh" "vector SM3 hash function" zvksh_forms
let zvfbfmin = leaf "zvfbfmin" "vector BF16 conversion" zvfbfmin_forms
let zvfbfwma = leaf "zvfbfwma" "vector BF16 widening multiply-accumulate" zvfbfwma_forms
let zvkb = leaf "zvkb" "vector cryptography bit manipulation" []
let zvknhb = leaf "zvknhb" ~implies:[ "zvknha" ] "vector SHA-2 (SHA-256 and SHA-512)" []

let group feature summary implies =
  Riscv_ext.component ~id:("riscv." ^ feature) ~feature ~implies ~default_on:false ~summary []

let zvkn = group "zvkn" "vector NIST algorithm suite" [ "zvkned"; "zvknhb"; "zvkb" ]
let zvks = group "zvks" "vector ShangMi algorithm suite" [ "zvksed"; "zvksh"; "zvkb" ]
let zvknc = group "zvknc" "vector NIST suite with carry-less multiply" [ "zvkn"; "zvbc" ]
let zvkng = group "zvkng" "vector NIST suite with GCM" [ "zvkn"; "zvkg" ]
let zvksc = group "zvksc" "vector ShangMi suite with carry-less multiply" [ "zvks"; "zvbc" ]
let zvksg = group "zvksg" "vector ShangMi suite with GCM" [ "zvks"; "zvkg" ]
