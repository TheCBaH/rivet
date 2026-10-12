type relation = Specialization | Disjoint_operands | Exclusive_by_configuration

let approved =
  [
    ("sspopchk", "mop.r.28", Specialization);
    ("ssrdp", "mop.r.28", Specialization);
    ("sspush", "mop.rr.7", Specialization);
    ("zext.w", "add.uw", Specialization);
    ("fabs.q", "fsgnjx.q", Specialization);
    ("fmv.q", "fsgnj.q", Specialization);
    ("fneg.q", "fsgnjn.q", Specialization);
    ("fabs.h", "fsgnjx.h", Specialization);
    ("fmv.h", "fsgnj.h", Specialization);
    ("fneg.h", "fsgnjn.h", Specialization);
    ("c.sspopchk", "c.mop.5", Specialization);
    ("c.sspush", "c.mop.1", Specialization);
    ("c.sext.w", "c.addiw", Specialization);
    ("c.sspopchk", "c.lui", Disjoint_operands);
    ("c.sspush", "c.lui", Disjoint_operands);
    ("c.mop.1", "c.lui", Disjoint_operands);
    ("c.mop.3", "c.lui", Disjoint_operands);
    ("c.mop.5", "c.lui", Disjoint_operands);
    ("c.mop.7", "c.lui", Disjoint_operands);
    ("c.mop.9", "c.lui", Disjoint_operands);
    ("c.mop.11", "c.lui", Disjoint_operands);
    ("c.mop.13", "c.lui", Disjoint_operands);
    ("c.mop.15", "c.lui", Disjoint_operands);
    ("c.addi16sp", "c.lui", Disjoint_operands);
    ("cm.mva01s", "c.fsdsp", Exclusive_by_configuration);
    ("cm.mvsa01", "c.fsdsp", Exclusive_by_configuration);
    ("cm.pop", "c.fsdsp", Exclusive_by_configuration);
    ("cm.popret", "c.fsdsp", Exclusive_by_configuration);
    ("cm.popretz", "c.fsdsp", Exclusive_by_configuration);
    ("cm.push", "c.fsdsp", Exclusive_by_configuration);
    ("cm.jalt", "c.fsdsp", Exclusive_by_configuration);
  ]

let find a b =
  List.find_map
    (fun (x, y, r) -> if (x = a && y = b) || (x = b && y = a) then Some r else None)
    approved
