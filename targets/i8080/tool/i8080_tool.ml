let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let die fmt =
  Printf.ksprintf
    (fun m ->
      prerr_endline m;
      exit 1)
    fmt

(* asm <source> -o <image> [-l <listing>] [-s <symbols>] [-r LO-HI] *)
let asm args =
  let range = ref None in
  let rec parse (src, out, lst, sym) = function
    | [] -> (src, out, lst, sym)
    | "-o" :: o :: r -> parse (src, Some o, lst, sym) r
    | "-l" :: l :: r -> parse (src, out, Some l, sym) r
    | "-s" :: s :: r -> parse (src, out, lst, Some s) r
    | "-r" :: spec :: r ->
        (match String.split_on_char '-' spec with
        | [ lo; hi ] -> range := Some (int_of_string lo, int_of_string hi)
        | _ -> die "-r wants LO-HI");
        parse (src, out, lst, sym) r
    | f :: r when src = None -> parse (Some f, out, lst, sym) r
    | _ -> die "usage: i8080_tool asm <source> -o <image> [-l <listing>] [-s <symbols>] [-r LO-HI]"
  in
  match parse (None, None, None, None) args with
  | Some src, out, lst, sym -> (
      match I8080_as.assemble ~file:(Filename.basename src) (read_file src) with
      | Error ds -> die "%s" (I8080_as.render ds)
      | Ok img ->
          let origin, bytes = I8080_as.flatten ?range:!range img in
          Option.iter (fun o -> write_file o bytes) out;
          Option.iter (fun l -> write_file l (I8080_as.listing_to_string img ^ "\n")) lst;
          Option.iter
            (fun s ->
              write_file s
                (String.concat ""
                   (List.map (fun (n, v) -> Printf.sprintf "%s %04x\n" n v) img.I8080_as.symbols)))
            sym;
          Printf.printf "%04x %d\n" origin (String.length bytes))
  | _ -> die "usage: i8080_tool asm <source> -o <image>"

(* Deliberate model bugs, to show that the differential runs notice one. *)
let plant name =
  let mutate (module V : Compute.VALUE with type t = int) =
    let module R = I8080_run.Make (I8080_sem.Runnable (V)) in
    R.run
  in
  match name with
  | "ult" ->
      mutate
        (module struct
          include Compute.Concrete

          let ult a b = if a <= b then 1 else 0
        end)
  | "aux" ->
      mutate
        (module struct
          include Compute.Concrete

          let extract a ~hi ~lo = if hi = 4 && lo = 4 then 0 else Compute.Concrete.extract a ~hi ~lo
        end)
  | "carry" ->
      mutate
        (module struct
          include Compute.Concrete

          let add ~w a b = if w = 9 && a + b = 0x100 then 0 else Compute.Concrete.add ~w a b
        end)
  | n -> die "unknown planted bug %s (ult, aux, carry)" n

(* run <program> --shim <dir> [--max N] [--load ADDR] [--regs] [--dump FILE] *)
let run args =
  let prog = ref None and shim = ref None and max = ref max_int and regs = ref false in
  let console = ref None
  and states = ref None
  and trace = ref false
  and io_out = ref None
  and opcodes = ref None
  and planted = ref None in
  let dump = ref None and load = ref 0x100 and pc = ref None in
  let rec parse = function
    | [] -> ()
    | "--shim" :: d :: r ->
        shim := Some d;
        parse r
    | "--max" :: n :: r ->
        max := int_of_string n;
        parse r
    | "--load" :: n :: r ->
        load := int_of_string n;
        parse r
    | "--pc" :: n :: r ->
        pc := Some (int_of_string n);
        parse r
    | "--regs" :: r ->
        regs := true;
        parse r
    | "--trace" :: r ->
        trace := true;
        parse r
    | "--console" :: f :: r ->
        console := Some f;
        parse r
    | "--states" :: f :: r ->
        states := Some f;
        parse r
    | "--io-out" :: f :: r ->
        io_out := Some f;
        parse r
    | "--opcodes" :: f :: r ->
        opcodes := Some f;
        parse r
    | "--plant" :: n :: r ->
        planted := Some n;
        parse r
    | "--dump" :: f :: r ->
        dump := Some f;
        parse r
    | f :: r when !prog = None ->
        prog := Some f;
        parse r
    | _ -> die "usage: i8080_tool run <program> --shim <dir> [--max N] [--regs] [--dump FILE]"
  in
  parse args;
  match (!prog, !shim) with
  | Some prog, Some dir ->
      let shim =
        {
          I8080_run.boot = read_file (Filename.concat dir "shim-boot.bin");
          bdos = read_file (Filename.concat dir "shim-bdos.bin");
          handler = read_file (Filename.concat dir "shim-handler.bin");
        }
      in
      let run = match !planted with None -> I8080_run.run | Some n -> plant n in
      let state_log = Buffer.create 4096 in
      let before =
        Option.map
          (fun _ (g : I8080_sem.regs) ->
            Buffer.add_string state_log
              (Printf.sprintf "C%dZ%dM%dE%d A=%02X B=%02X%02X D=%02X%02X H=%02X%02X S=%04X P=%04X\n"
                 (g.flags land 1)
                 ((g.flags lsr 6) land 1)
                 ((g.flags lsr 7) land 1)
                 ((g.flags lsr 2) land 1)
                 g.ra g.rb g.rc g.rd g.re g.rh g.rl g.rsp g.rpc))
          !states
      in
      let r =
        run ?before ~max_instructions:!max ~load:!load ?pc:!pc
          ?trace:
            (if !trace then
               Some
                 (function
                 | I8080_sem.Ev_fetch (a, v) -> Printf.eprintf "fetch %04x %02x\n" a v
                 | Ev_read (a, v) -> Printf.eprintf "read %04x %02x\n" a v
                 | Ev_write (a, v) -> Printf.eprintf "write %04x %02x\n" a v
                 | Ev_in (a, v) -> Printf.eprintf "in %02x %02x\n" a v
                 | Ev_out (a, v) -> Printf.eprintf "out %02x %02x\n" a v)
             else None)
          ~shim (read_file prog)
      in
      print_string r.I8080_run.console;
      Option.iter (fun f -> write_file f r.I8080_run.console) !console;
      Option.iter (fun f -> write_file f (Buffer.contents state_log)) !states;
      Option.iter (fun f -> write_file f (Bytes.to_string r.I8080_run.memory)) !dump;
      Option.iter
        (fun f ->
          write_file f
            (String.concat ""
               (List.map (fun (p, v) -> Printf.sprintf "%02x %02x\n" p v) r.I8080_run.io_out)))
        !io_out;
      Option.iter
        (fun f ->
          write_file f
            (String.concat ""
               (List.filter_map Fun.id
                  (List.init 256 (fun i ->
                       if Bytes.get r.I8080_run.seen i <> '\000' then
                         Some (Printf.sprintf "%02x\n" i)
                       else None)))))
        !opcodes;
      let status =
        match r.I8080_run.status with
        | I8080_run.Exited -> "exited"
        | Halted -> "halted"
        | Limit -> "instruction limit"
        | Fault m -> "fault: " ^ m
      in
      Printf.eprintf "status=%s instructions=%d tstates=%d\n" status r.I8080_run.instructions
        r.I8080_run.tstates;
      (if !regs then
         let g = r.I8080_run.regs in
         Printf.eprintf "a=%02x f=%02x b=%02x c=%02x d=%02x e=%02x h=%02x l=%02x sp=%04x pc=%04x\n"
           g.I8080_sem.ra g.flags g.rb g.rc g.rd g.re g.rh g.rl g.rsp g.rpc);
      exit (if r.I8080_run.status = I8080_run.Exited then 0 else 2)
  | _ -> die "usage: i8080_tool run <program> --shim <dir>"

(* dis: every opcode byte as Rivet decodes and prints it, one line each: bytes
   (operands 2a / 00 20), canonical text, length, class. The oracle assembles
   the text with asl and compares the bytes. *)
let dis () =
  for b = 0 to 255 do
    let len = I8080_ops.timings.(b).I8080_ops.length in
    let operand i = if len = 2 then 0x2a else if i = 1 then 0x00 else 0x20 in
    let raw = String.init len (fun i -> Char.chr (if i = 0 then b else operand i)) in
    let bytes =
      String.concat " " (List.init len (fun i -> Printf.sprintf "%02x" (Char.code raw.[i])))
    in
    match I8080_text.canonical ~origin:0 raw with
    | Error _ -> die "dis: %02x does not decode" b
    | Ok c ->
        let text =
          match String.split_on_char '\n' c with
          | _ :: line :: _ -> String.trim line
          | _ -> die "dis: %02x: unexpected canonical text" b
        in
        let cls =
          match I8080_ops.form_of_byte b with Some (I8080_ops.Alias _) -> "alias" | _ -> "doc"
        in
        Printf.printf "%s\t%s\t%d\t%s\n" bytes text len cls
  done

(* builders: the corpus of typed builders, one line each in the layout of [dis]: bytes
   assembled through [I8080_asm.Program], the instruction's printed text, length, "builder". *)
let builders () =
  List.iter
    (fun i ->
      match I8080_asm.Program.assemble [ I8080_asm.insn i ] with
      | Error e ->
          die "builders: %s" (Foundation.Diagnostic.render_all (Foundation.Diag.diagnostics e))
      | Ok img ->
          let bytes = img.I8080_asm.Program.bytes in
          Printf.printf "%s\t%s\t%d\tbuilder\n"
            (String.concat " "
               (List.init (String.length bytes) (fun k ->
                    Printf.sprintf "%02x" (Char.code bytes.[k]))))
            (Format.asprintf "%a" I8080_encode.Instruction.pp i)
            (String.length bytes))
    I8080_corpus.all

let () =
  match List.tl (Array.to_list Sys.argv) with
  | "asm" :: args -> asm args
  | "run" :: args -> run args
  | [ "dis" ] -> dis ()
  | [ "builders" ] -> builders ()
  | [ "gen"; seed; units ] ->
      print_string (I8080_gen.program ~seed:(int_of_string seed) ~units:(int_of_string units))
  | _ -> die "usage: i8080_tool asm|run|dis|builders|gen ..."
