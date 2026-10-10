open Foundation

let bindings = [ ("host_f", 0x123456789abcdef0L); ("host_g", 0x0fedcba987654321L) ]

let text_bytes (laid : Image.laid_out) =
  let addresses =
    List.map
      (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0x10000L))
      (Image.plan_of laid).Image.segments
  in
  match Image.bind_image laid ~addresses with
  | Error e -> Error (Diag.render e)
  | Ok image -> (
      match
        List.find_opt (fun (s : Image.segment) -> s.Image.name = ".text") image.Image.segments
      with
      | Some s -> Ok s.Image.bytes
      | None -> Error "no .text")

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let failed = ref false

let () =
  let text target =
    let (module D : Target_intf.Target.DRIVER) = Option.get (Driver.Registry.find target) in
    let tramp = Result.get_ok (Native_exec.trampolines ~target bindings) in
    D.assemble_many ~entry:"host_f" [ ("host", Span.source ~name:"host" ~contents:tramp) ] ()
  in
  let report target typed =
    match (Result.map_error Diag.render (text target), typed) with
    | Ok a, Ok b -> (
        match (text_bytes a, text_bytes b) with
        | Ok x, Ok y ->
            Printf.printf "%s: typed %s text (%d bytes)\n" target
              (if String.equal x y then "equals" else "DIFFERS from")
              (String.length y);
            if not (String.equal x y) then (
              failed := true;
              Printf.printf "  text %s\n  typed %s\n" (hex x) (hex y))
        | Error e, _ | _, Error e ->
            failed := true;
            print_endline e)
    | Error e, _ | _, Error e ->
        failed := true;
        print_endline e
  in
  let module A = Driver_direct.Pipeline_direct.Make (Aarch64_encode) in
  report "aarch64"
    (Result.map_error Diag.render
       (Result.bind
          (A.lower ~state:Aarch64_encode.default_state (Aarch64_encode.host_trampolines bindings))
          (fun l -> A.plan ~entry:"host_f" l)));
  let module X = Driver_direct.Pipeline_direct.Make (X86_64_encode) in
  report "x86_64"
    (Result.map_error Diag.render
       (Result.bind
          (X.lower ~state:X86_64_encode.default_state (X86_64_encode.host_trampolines bindings))
          (fun l -> X.plan ~entry:"host_f" l)))

let () = if !failed then exit 1
