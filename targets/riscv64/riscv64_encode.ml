module Profile = struct
  let name = "riscv64"
  let triple = "riscv64-linux-gnu"
  let xlen = 64
end

include Riscv_family_encode.Make (Profile)
