open Rivet_foundation

let bindings = [ ("host_f", 0x123456789abcdef0L); ("host_g", 0x0fedcba987654321L) ]

let text_bytes (laid : Rivet_image.laid_out) =
  let addresses =
    List.map
      (fun (s : Rivet_image.segment_plan) -> (s.Rivet_image.seg_name, 0x10000L))
      (Rivet_image.plan_of laid).Rivet_image.segments
  in
  match Rivet_image.bind_image laid ~addresses with
  | Error e -> Error (Diag.render e)
  | Ok image -> (
      match
        List.find_opt
          (fun (s : Rivet_image.segment) -> s.Rivet_image.name = ".text")
          image.Rivet_image.segments
      with
      | Some s -> Ok s.Rivet_image.bytes
      | None -> Error "no .text")

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let failed = ref false

let () =
  let text target =
    let (module D : Rivet_target_intf.Target.DRIVER) =
      Option.get (Rivet_driver.Registry.find target)
    in
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
  let module A = Rivet_driver_direct.Pipeline_direct.Make (Rivet_aarch64_encode) in
  report "aarch64"
    (Result.map_error Diag.render
       (Result.bind
          (A.lower ~state:Rivet_aarch64_encode.default_state
             (Rivet_aarch64_encode.host_trampolines bindings))
          (fun l -> A.plan ~entry:"host_f" l)));
  let module X = Rivet_driver_direct.Pipeline_direct.Make (Rivet_x86_64_encode) in
  report "x86_64"
    (Result.map_error Diag.render
       (Result.bind
          (X.lower ~state:Rivet_x86_64_encode.default_state
             (Rivet_x86_64_encode.host_trampolines bindings))
          (fun l -> X.plan ~entry:"host_f" l)))

let () = if !failed then exit 1
