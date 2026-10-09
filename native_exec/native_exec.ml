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
external call_entry : int64 -> io -> int64 = "native_exec_call"
external host_isa_stub : unit -> string = "native_exec_host_isa"
external symbol_stub : string -> int64 = "native_exec_symbol"
external io_address : io -> int64 = "native_exec_io_address"

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
  | Closed  (** the loaded image was closed *)
  | Missing_symbol of string  (** a host symbol to bind does not resolve in this process *)
  | Global_size of { name : string; expected : int; actual : int }
      (** [write_global] was given other than the global's size in bytes *)

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
  | Closed -> Fmt.string ppf "the loaded image was closed"
  | Missing_symbol n -> Fmt.pf ppf "host symbol %s does not resolve in this process" n
  | Global_size { name; expected; actual } ->
      Fmt.pf ppf "global %s is %d bytes, not the %d written" name expected actual

(* {1 Host symbols}

   The image linker is closed: an undefined symbol is an error. A host
   function is therefore bound by defining its name in an extra unit, whose
   code loads the address into a scratch register and jumps to it. The
   address is absolute because a direct branch cannot reach an arbitrary
   libc address; argument and return registers are untouched, so the callee
   returns straight to the image's caller. Addresses belong to this process
   and must not be cached across processes. *)

let host_symbol name = match symbol_stub name with 0L -> None | a -> Some a

let valid_symbol n =
  n <> ""
  && String.for_all
       (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' | '$' -> true | _ -> false)
       n

let trampoline ~target name addr =
  let q k = Int64.to_int (Int64.logand (Int64.shift_right_logical addr (16 * k)) 0xffffL) in
  let head = Printf.sprintf {|    .globl %s
    .type %s, @function
|} name name in
  match target with
  | "aarch64" ->
      Ok
        (Printf.sprintf
           {|%s%s:
    movz x16, #%d
    movk x16, #%d, lsl #16
    movk x16, #%d, lsl #32
    movk x16, #%d, lsl #48
    br x16
|}
           head name (q 0) (q 1) (q 2) (q 3))
  | "x86_64" -> Ok (Printf.sprintf {|%s%s:
    movabsq $%Ld, %%r11
    jmp *%%r11
|} head name addr)
  | "riscv64" ->
      Ok
        (Printf.sprintf
           {|    .balign 8
%s%s:
    auipc t1, 0
    ld t1, 16(t1)
    jr t1
    nop
    .quad %Ld
|}
           head name addr)
  | t -> Error (Foreign_target { target = t; host = host_isa })

(* Assembly text, for the assembler of [target], defining every name as a
   jump to its address. Assemble it as one more unit beside the code that
   calls the names. *)
let trampolines ~target bindings =
  List.iter
    (fun (n, _) -> if not (valid_symbol n) then invalid_arg ("Native_exec.trampolines: " ^ n))
    bindings;
  List.fold_left
    (fun acc (n, a) ->
      match (acc, trampoline ~target n a) with
      | Ok text, Ok t -> Ok (text ^ t)
      | (Error _ as e), _ | _, (Error _ as e) -> e)
    (Ok "    .text\n") bindings

(* [trampolines] for names resolved in this process; the first one that does
   not resolve is reported and nothing is generated. *)
let bind_host ~target names =
  let rec resolve acc = function
    | [] -> trampolines ~target (List.rev acc)
    | n :: rest -> (
        match if valid_symbol n then host_symbol n else None with
        | Some a -> resolve ((n, a) :: acc) rest
        | None -> Error (Missing_symbol n))
  in
  resolve [] names

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

(* {1 Loading}

   [load] owns everything up to a callable image: reserve, bind, copy,
   protect, flush. [call] only invokes already-loaded code, any number of
   times, and never maps or unmaps. [close] makes later calls fail with
   [Closed] and releases the mapping once no call is active, so the unmap
   happens exactly once, after the last call has returned. The caller owns
   the io buffer of each call; nothing here is shared between calls but the
   immutable code and data of the image. *)

type loaded = {
  mapping : int64;
  size : int;
  image : Image.t;
  entry : int64;
  observe : event -> unit;
  active : int Atomic.t;
  closed : bool Atomic.t;
  released : bool Atomic.t;
}

let release t =
  if Atomic.compare_and_set t.released false true then begin
    unmap t.mapping t.size;
    t.observe (Unmapped { base = t.mapping; size = t.size })
  end

let leave t = if Atomic.fetch_and_add t.active (-1) = 1 && Atomic.get t.closed then release t

let close t =
  Atomic.set t.closed true;
  if Atomic.get t.active = 0 then release t

let with_open t f =
  Atomic.incr t.active;
  Fun.protect
    ~finally:(fun () -> leave t)
    (fun () -> if Atomic.get t.closed then Error Closed else f ())

let load ?(observe = fun (_ : event) -> ()) ?target (laid : Image.laid_out) =
  match target with
  | Some t when host_isa <> Some t -> Error (Foreign_target { target = t; host = host_isa })
  | _ -> (
      let plan = Image.plan_of laid in
      if plan.entry = None then Error No_entry
      else
        let offsets, total, max_align = layout plan in
        (* mmap only promises page alignment; over-reserve so a stricter
           segment alignment can be met by moving the base up. *)
        let size = total + (max_align - page_size ()) in
        match map size with
        | exception Failure m -> Error (Os m)
        | mapping -> (
            observe (Mapped { base = mapping; size });
            let discard () =
              unmap mapping size;
              observe (Unmapped { base = mapping; size })
            in
            let base = Int64.of_int (align_up (Int64.to_int mapping) max_align) in
            let populate () =
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
                    Ok image
                  with Failure m -> Error (Os m))
            in
            match populate () with
            | exception e ->
                discard ();
                raise e
            | Error e ->
                discard ();
                Error e
            | Ok image ->
                Ok
                  {
                    mapping;
                    size;
                    image;
                    entry = Option.get image.entry;
                    observe;
                    active = Atomic.make 0;
                    closed = Atomic.make false;
                    released = Atomic.make false;
                  }))

let call t ~(io : io) =
  with_open t (fun () ->
      t.observe (Calling { entry = t.entry });
      try Ok (call_entry t.entry io) with Failure m -> Error (Os m))

(* The bytes of an exported global with a known size, read from the live
   mapping. *)
let read_global t name =
  with_open t (fun () ->
      match (List.assoc_opt name t.image.exports, List.assoc_opt name t.image.symbol_sizes) with
      | Some a, Some sz -> ( try Ok (copy_out a (Int64.to_int sz)) with Failure m -> Error (Os m))
      | _ -> Error (Missing_global name))

(* Overwrites an exported global of known size with exactly that many bytes. The
   segment holding it must be writable; writing a read-only one faults. *)
let write_global t name bytes =
  with_open t (fun () ->
      match (List.assoc_opt name t.image.exports, List.assoc_opt name t.image.symbol_sizes) with
      | Some a, Some sz ->
          let expected = Int64.to_int sz and actual = String.length bytes in
          if expected <> actual then Error (Global_size { name; expected; actual })
          else ( try Ok (copy_in a bytes) with Failure m -> Error (Os m))
      | _ -> Error (Missing_global name))

let run_here ?observe ?(read_globals = []) (laid : Image.laid_out) ~(io : io) =
  match load ?observe laid with
  | Error e -> Error e
  | Ok t ->
      Fun.protect
        ~finally:(fun () -> close t)
        (fun () ->
          match call t ~io with
          | Error e -> Error e
          | Ok value ->
              let rec read_all acc = function
                | [] -> Ok { value; globals = List.rev acc }
                | n :: rest -> (
                    match read_global t n with
                    | Ok bytes -> read_all ((n, bytes) :: acc) rest
                    | Error e -> Error e)
              in
              read_all [] read_globals)

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
