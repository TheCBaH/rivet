let configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32im"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64im"; "-mabi=lp64"; "-mno-relax" ]
  | Target.X86_32 | Target.X86_64 -> []
  | Target.Arm | Target.Aarch64 -> []

(* One canonical, legal register/immediate assignment per pilot form_id.
   Deliberately avoids RISC-V's x0 and x86's accumulator (eax/al/ax/rax):
   GAS special-cases the accumulator for several x86 ALU/MOV immediate forms
   (a SHORTER accumulator-specific opcode - e.g. plain ADD's 0x05 rather than
   the general 0x81 /0 this pilot's ADD_GPRv_IMMz entry names), so canonical
   register choice already had to be verified empirically against a real
   assembler, not assumed. *)
let operands_for form_id =
  match form_id with
  | "riscv:add" | "riscv:sub" | "riscv:mul" | "riscv:addw" ->
      Some [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ]
  | "riscv:addi" -> Some [ ("rd", "a0"); ("rs1", "a1"); ("imm", "1") ]
  | "x86:ADD_GPRv_GPRv_01" | "x86:ADD_GPRv_GPRv_03" | "x86:MOV_GPRv_GPRv_89"
  | "x86:MOV_GPRv_GPRv_8B" ->
      Some [ ("src", "edx"); ("dest", "ecx") ]
  | "x86:ADD_GPRv_IMMz" ->
      (* add_gprv_immz_form's own operand names, distinct from the generic
         two_operand_gprv_form below - see Isa_norm_xed.ml. A large immediate
         (> signed-byte range) so GAS cannot silently pick the shorter imm8
         alternative instead of the imm32 one this entry names - see the "x86
         short-versus-full immediate" obligation. *)
      Some [ ("dest", "ecx"); ("imm", "1000000") ]
  | "x86:MOV_GPRv_IMMz" ->
      (* MOV_GPRv_IMMz is normalized by the generic two_operand_gprv_form
         (operand names "src"/"dest", "src" being whichever operand is the
         READ side - here the immediate), NOT add_gprv_immz_form's "dest"/
         "imm" naming; see Isa_norm_xed.ml's two_operand_gprv_form/norm_operand_of. *)
      Some [ ("src", "1000000"); ("dest", "ecx") ]
  | _ -> None

let ( let* ) = Result.bind

let build (entry : Isa_gen_pilot.pilot_entry) (form : Isa_norm_model.form) =
  match operands_for entry.form_id with
  | None ->
      Error
        (Printf.sprintf "Isa_gen_case_build has no canonical operand assignment for %s"
           entry.form_id)
  | Some operands ->
      let* line = Isa_gen_render.render_line form.syntax ~operands in
      Ok
        Isa_generated_case.
          {
            case_id = entry.form_id ^ ":canonical:" ^ Target.to_string entry.target;
            target = entry.target;
            form_id = entry.form_id;
            source_record_ids = form.source_record_ids;
            rule_ids = [ "canonical-spelling" ];
            operands;
            rendered_source = Isa_gen_render.render_source line;
            configuration = configuration_for entry.target;
            negative = false;
          }
