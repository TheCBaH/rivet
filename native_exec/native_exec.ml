(* Runs a laid-out image natively, in this process.

   One run owns one mapping for its whole life: reserve -> bind -> copy ->
   protect -> flush -> call -> read back -> unmap, with the unmap on every
   path. The mapping is a single reservation so every intra-image reference
   stays in range (adrp reaches +-4 GB, b/bl +-128 MB). Pages start
   read-write and each segment is switched to its final permissions before
   anything is called; no page is ever writable and executable at once.

   The code being run is trusted: a fault in it is a fault of this process. *)

open Foundation
open Asm_core

type io = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external page_size : unit -> int = "native_exec_page_size"
external map : int -> int64 = "native_exec_map"
external unmap : int64 -> int -> unit = "native_exec_unmap"
external protect : int64 -> int -> int -> unit = "native_exec_protect"
external clear_cache : int64 -> int -> unit = "native_exec_clear_cache"
external copy_in : int64 -> string -> unit = "native_exec_copy_in"
external copy_out : int64 -> int -> string = "native_exec_copy_out"
external call : int64 -> io -> int64 = "native_exec_call"
external host_isa_stub : unit -> string = "native_exec_host_isa"

(* The target name of the ISA this process runs, if it is one the assembler
   targets. Only images for that target can be run here. *)
let host_isa = match host_isa_stub () with "" -> None | s -> Some s

(* What a run does to memory, in order, for tests to assert on. *)
type event =
  | Mapped of { base : int64; size : int }
  | Protected of { segment : string; address : int64; size : int; perms : Perms.t }
  | Flushed of { segment : string; address : int64; size : int }
  | Calling of { entry : int64 }
  | Unmapped of { base : int64; size : int }

type outcome = { value : int64; globals : (string * string) list }

type error =
  | No_entry  (** the image has no entry symbol *)
  | Bind of string  (** [Image.bind_image] rejected the chosen addresses (rendered) *)
  | Missing_global of string  (** a requested global is not exported, or has no size *)
  | Os of string  (** a system call failed *)
  | Signal of int  (** isolated only: the child was killed by this signal (OCaml numbering) *)
  | Timeout of float  (** isolated only: the call ran longer than this many seconds *)
  | Child of string  (** isolated only: the child failed before reporting a result *)
  | Foreign_target of { target : string; host : string option }
      (** the image was built for a target this host cannot run in process *)

let signal_name n =
  List.assoc_opt n
    [
      (Sys.sigsegv, "SIGSEGV");
      (Sys.sigbus, "SIGBUS");
      (Sys.sigill, "SIGILL");
      (Sys.sigfpe, "SIGFPE");
      (Sys.sigtrap, "SIGTRAP");
      (Sys.sigabrt, "SIGABRT");
      (Sys.sigkill, "SIGKILL");
      (Sys.sigalrm, "SIGALRM");
    ]
  |> Option.value ~default:(Printf.sprintf "signal %d" n)

let pp_error ppf = function
  | No_entry -> Fmt.string ppf "the image has no entry symbol"
  | Bind m -> Fmt.string ppf m
  | Missing_global n -> Fmt.pf ppf "global %s is not exported with a known size" n
  | Os m -> Fmt.string ppf m
  | Signal n -> Fmt.pf ppf "generated code was killed by %s" (signal_name n)
  | Timeout t -> Fmt.pf ppf "generated code ran longer than %gs" t
  | Child m -> Fmt.pf ppf "isolated run failed: %s" m
  | Foreign_target { target; host } ->
      Fmt.pf ppf "%s code cannot run natively on this host (%s)" target
        (Option.value host ~default:"an ISA no target matches")

let align_up n a = if a <= 1 then n else (n + a - 1) / a * a

let prot_bits (p : Perms.t) =
  (if p.read then 1 else 0) lor (if p.write then 2 else 0) lor if p.execute then 4 else 0

(* Page-aligned offsets for every segment inside one reservation, the
   reservation's size, and the strictest alignment any segment asked for. *)
let layout (plan : Image.plan) =
  let page = page_size () in
  let max_align =
    List.fold_left (fun m (s : Image.segment_plan) -> max m s.alignment) page plan.segments
  in
  let offsets, total =
    List.fold_left
      (fun (acc, off) (s : Image.segment_plan) ->
        let off = align_up off (max page s.alignment) in
        ((s.seg_name, off) :: acc, align_up (off + s.init_size + s.zero_fill) page))
      ([], 0) plan.segments
  in
  (List.rev offsets, max total page, max_align)

let run_here ?(observe = fun (_ : event) -> ()) ?(read_globals = []) (laid : Image.laid_out)
    ~(io : io) =
  let plan = Image.plan_of laid in
  if plan.entry = None then Error No_entry
  else
    let offsets, total, max_align = layout plan in
    (* mmap only promises page alignment; over-reserve so a stricter
       segment alignment can be met by moving the base up. *)
    let size = total + (max_align - page_size ()) in
    match map size with
    | exception Failure m -> Error (Os m)
    | mapping ->
        observe (Mapped { base = mapping; size });
        let base = Int64.of_int (align_up (Int64.to_int mapping) max_align) in
        let finally () =
          unmap mapping size;
          observe (Unmapped { base = mapping; size })
        in
        Fun.protect ~finally (fun () ->
            let addresses =
              List.map (fun (n, off) -> (n, Int64.add base (Int64.of_int off))) offsets
            in
            match Image.bind_image laid ~addresses with
            | Error e -> Error (Bind (Diag.render e))
            | Ok (image : Image.t) -> (
                try
                  List.iter (fun (s : Image.segment) -> copy_in s.address s.bytes) image.segments;
                  List.iter
                    (fun (s : Image.segment) ->
                      let size = align_up (String.length s.bytes + s.zero_fill) (page_size ()) in
                      if size > 0 then begin
                        protect s.address size (prot_bits s.perms);
                        observe
                          (Protected
                             { segment = s.name; address = s.address; size; perms = s.perms })
                      end)
                    image.segments;
                  List.iter
                    (fun (s : Image.segment) ->
                      let size = String.length s.bytes in
                      if Perms.executable s.perms && size > 0 then begin
                        clear_cache s.address size;
                        observe (Flushed { segment = s.name; address = s.address; size })
                      end)
                    image.segments;
                  let entry = Option.get image.entry in
                  observe (Calling { entry });
                  let value = call entry io in
                  let read n =
                    match (List.assoc_opt n image.exports, List.assoc_opt n image.symbol_sizes) with
                    | Some a, Some sz -> Ok (n, copy_out a (Int64.to_int sz))
                    | _ -> Error (Missing_global n)
                  in
                  let rec read_all acc = function
                    | [] -> Ok { value; globals = List.rev acc }
                    | n :: rest -> (
                        match read n with Ok g -> read_all (g :: acc) rest | Error e -> Error e)
                  in
                  read_all [] read_globals
                with Failure m -> Error (Os m)))

(* {1 Isolation}

   [~isolate:true] performs the whole run in a forked child, so a fault or a
   hang in generated code costs the child, not this process. The child sends
   back the return value, the io buffer and the requested globals over a
   pipe and leaves with [_exit], so none of this process's [at_exit]
   handlers or buffered output runs twice. A wall-clock limit is enforced
   with [alarm] in the child: SIGALRM is reported as [Timeout]. *)

let send fd s =
  let b = Bytes.unsafe_of_string s in
  let rec go off =
    if off < Bytes.length b then go (off + Unix.write fd b off (Bytes.length b - off))
  in
  go 0

let recv_all fd =
  let buf = Buffer.create 4096 and chunk = Bytes.create 65536 in
  let rec go () =
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buf chunk 0 n;
        go ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
  in
  go ();
  Buffer.contents buf

let io_contents (io : io) = String.init (Bigarray.Array1.dim io) (fun i -> io.{i})

let run_isolated ~timeout_s ?read_globals laid ~(io : io) =
  let rd, wr = Unix.pipe ~cloexec:true () in
  flush_all ();
  match Unix.fork () with
  | 0 ->
      Unix.close rd;
      ignore (Unix.alarm (max 1 (int_of_float (Float.ceil timeout_s))));
      let reply =
        match run_here ?read_globals laid ~io with
        | Ok o -> Marshal.to_string (Ok (o, io_contents io) : (outcome * string, error) result) []
        | Error e -> Marshal.to_string (Error e : (outcome * string, error) result) []
      in
      send wr reply;
      Unix._exit 0
  | pid -> (
      Unix.close wr;
      let reply = Fun.protect ~finally:(fun () -> Unix.close rd) (fun () -> recv_all rd) in
      let rec wait () =
        try snd (Unix.waitpid [] pid) with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
      in
      match wait () with
      | Unix.WSIGNALED n when n = Sys.sigalrm -> Error (Timeout timeout_s)
      | Unix.WSIGNALED n | Unix.WSTOPPED n -> Error (Signal n)
      | Unix.WEXITED 0 when reply <> "" -> (
          match (Marshal.from_string reply 0 : (outcome * string, error) result) with
          | Ok (o, bytes) ->
              String.iteri (fun i c -> io.{i} <- c) bytes;
              Ok o
          | Error e -> Error e)
      | Unix.WEXITED n -> Error (Child (Printf.sprintf "exited with status %d and no result" n)))

(* [target] names the target [laid] was assembled for. When it is given and
   is not the host's ISA, nothing is mapped or called: running another ISA's
   bytes in this process would fault at best. *)
let run ?target ?observe ?read_globals ?(isolate = false) ?(timeout_s = 10.0) laid ~io =
  match target with
  | Some t when host_isa <> Some t -> Error (Foreign_target { target = t; host = host_isa })
  | _ ->
      if isolate then run_isolated ~timeout_s ?read_globals laid ~io
      else run_here ?observe ?read_globals laid ~io
