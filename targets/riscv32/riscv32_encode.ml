module Profile = struct
  let name = "riscv32"
  let triple = "riscv32-linux-gnu"
  let xlen = 32
end

include Riscv_family_encode.Make (Profile)
