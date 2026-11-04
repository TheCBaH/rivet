type t = { index : int; row : Riscv_table_row.row }

let mint index row = { index; row }
let index t = t.index
let row t = t.row
let mnemonic t = t.row.Riscv_table_row.mnemonic
let feature t = t.row.Riscv_table_row.feature
let source t = t.row.Riscv_table_row.source
let equal a b = a.index = b.index && a.row = b.row
let compare a b = match Int.compare a.index b.index with 0 -> Stdlib.compare a.row b.row | c -> c
