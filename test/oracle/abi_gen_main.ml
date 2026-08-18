(* CLI for the ABI generator (.ai/asm_plan.md M4 Phase 3.2).

   Usage: abi_gen_main --profile <name> --abi-version <n>

   Prints the generated GAS source to stdout. This is the buildable,
   invocable executable round 3's review required in place of a library
   module with no way to run it: [tools/asm-helpers.sh]'s [build_generated_one]
   invokes exactly this binary. *)

let profile_of_string = function
  | "x86_64" -> Some Abi_gen.X86_64
  | "x86_32" -> Some Abi_gen.X86_32
  | "arm" -> Some Abi_gen.Arm
  | "aarch64" -> Some Abi_gen.Aarch64
  | _ -> None

let usage () =
  prerr_endline "usage: abi_gen_main --profile <name> --abi-version <n>";
  prerr_endline "  supported profiles: x86_64, x86_32, arm, aarch64";
  exit 2

let () =
  let profile = ref None in
  let abi_version = ref None in
  let rec parse = function
    | "--profile" :: v :: rest ->
        profile := profile_of_string v;
        parse rest
    | "--abi-version" :: v :: rest ->
        (abi_version := match int_of_string_opt v with Some n -> Some n | None -> None);
        parse rest
    | [] -> ()
    | _ -> usage ()
  in
  parse (List.tl (Array.to_list Sys.argv));
  match (!profile, !abi_version) with
  | Some p, Some 3 -> print_string (Abi_gen.source p)
  | Some _, Some n ->
      Printf.eprintf
        "abi_gen_main: --abi-version %d is not supported (only 3 is generated; v1/v2\n\
         already have checked-in helpers under asm/helpers/)\n"
        n;
      exit 2
  | None, _ -> usage ()
  | _, None -> usage ()
