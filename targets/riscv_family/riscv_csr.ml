let range prefix ?(suffix = "") first last base =
  List.init
    (last - first + 1)
    (fun i -> (Printf.sprintf "%s%d%s" prefix (first + i) suffix, base + first + i))

let all =
  [
    ("fflags", 0x001);
    ("frm", 0x002);
    ("fcsr", 0x003);
    ("vstart", 0x008);
    ("vxsat", 0x009);
    ("vxrm", 0x00a);
    ("vcsr", 0x00f);
    ("seed", 0x015);
    ("cycle", 0xc00);
    ("time", 0xc01);
    ("instret", 0xc02);
    ("vl", 0xc20);
    ("vtype", 0xc21);
    ("vlenb", 0xc22);
    ("cycleh", 0xc80);
    ("timeh", 0xc81);
    ("instreth", 0xc82);
    ("sstatus", 0x100);
    ("sie", 0x104);
    ("stvec", 0x105);
    ("scounteren", 0x106);
    ("senvcfg", 0x10a);
    ("scountinhibit", 0x120);
    ("sscratch", 0x140);
    ("sepc", 0x141);
    ("scause", 0x142);
    ("stval", 0x143);
    ("sip", 0x144);
    ("stimecmp", 0x14d);
    ("stimecmph", 0x15d);
    ("satp", 0x180);
    ("scountovf", 0xda0);
    ("mvendorid", 0xf11);
    ("marchid", 0xf12);
    ("mimpid", 0xf13);
    ("mhartid", 0xf14);
    ("mconfigptr", 0xf15);
    ("mstatus", 0x300);
    ("misa", 0x301);
    ("medeleg", 0x302);
    ("mideleg", 0x303);
    ("mie", 0x304);
    ("mtvec", 0x305);
    ("mcounteren", 0x306);
    ("menvcfg", 0x30a);
    ("mstatush", 0x310);
    ("medelegh", 0x312);
    ("menvcfgh", 0x31a);
    ("mcountinhibit", 0x320);
    ("mscratch", 0x340);
    ("mepc", 0x341);
    ("mcause", 0x342);
    ("mtval", 0x343);
    ("mip", 0x344);
    ("mtinst", 0x34a);
    ("mtval2", 0x34b);
    ("mseccfg", 0x747);
    ("mseccfgh", 0x757);
    ("mcycle", 0xb00);
    ("minstret", 0xb02);
    ("mcycleh", 0xb80);
    ("minstreth", 0xb82);
    ("tselect", 0x7a0);
    ("tdata1", 0x7a1);
    ("tdata2", 0x7a2);
    ("tdata3", 0x7a3);
    ("dcsr", 0x7b0);
    ("dpc", 0x7b1);
    ("dscratch0", 0x7b2);
    ("dscratch1", 0x7b3);
  ]
  @ range "hpmcounter" 3 31 0xc00
  @ range "hpmcounter" ~suffix:"h" 3 31 0xc80
  @ range "mhpmcounter" 3 31 0xb00
  @ range "mhpmcounter" ~suffix:"h" 3 31 0xb80
  @ range "mhpmevent" 3 31 0x320 @ range "pmpcfg" 0 15 0x3a0 @ range "pmpaddr" 0 63 0x3b0

let by_name = Hashtbl.create 256
let by_number = Hashtbl.create 256

let () =
  List.iter
    (fun (n, v) ->
      Hashtbl.replace by_name n v;
      Hashtbl.replace by_number v n)
    all

let number n = Hashtbl.find_opt by_name n
let name v = Hashtbl.find_opt by_number v
