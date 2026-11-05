(* requires: rivet.x86_32_asm rivet.x86_64_asm *)
let _ = X86_64_asm.[ add rax rbx; add_i 5L rax; add_m (mem rsp) rbx; vaddps ymm1 ymm2 ymm3 ]
let _ = X86_32_asm.[ add eax ebx; addl_im 1L (mem eax); movzb ah ecx; ret ]
