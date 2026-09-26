(* Tool_parallel: forked sharding keeps input order, runs in more than one process, and turns a
   worker failure into an error rather than a partial result. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let xs = List.init 23 Fun.id

let test_order () =
  match Tool_parallel.map ~jobs:4 (fun x -> x * x) xs with
  | Ok ys -> check "results come back in input order" (ys = List.map (fun x -> x * x) xs)
  | Error _ -> check "map succeeds" false

let test_forked () =
  let parent = Unix.getpid () in
  match Tool_parallel.map ~jobs:4 (fun _ -> Unix.getpid ()) xs with
  | Ok pids ->
      check "jobs run outside the parent" (not (List.mem parent pids));
      check "jobs are spread over several workers" (List.length (List.sort_uniq compare pids) = 4)
  | Error _ -> check "map succeeds" false

let test_sequential () =
  let parent = Unix.getpid () in
  match Tool_parallel.map ~jobs:1 (fun _ -> Unix.getpid ()) xs with
  | Ok pids -> check "jobs=1 runs in-process" (List.for_all (( = ) parent) pids)
  | Error _ -> check "map succeeds" false

let test_closures () =
  match Tool_parallel.map ~jobs:3 (fun x () -> x + 1) xs with
  | Ok fs ->
      check "closure results survive the worker boundary"
        (List.map (fun f -> f ()) fs = List.map succ xs)
  | Error _ -> check "map succeeds" false

let test_failure () =
  match Tool_parallel.map ~jobs:3 (fun x -> if x = 7 then failwith "boom" else x) xs with
  | Ok _ -> check "a raising job is an error" false
  | Error _ -> check "a raising job is an error" true

let () =
  print_endline "tool-parallel:";
  test_order ();
  test_forked ();
  test_sequential ();
  test_closures ();
  test_failure ();
  if !failures > 0 then (
    Printf.printf "tool-parallel: %d of %d checks FAILED\n" !failures !checks;
    exit 1)
  else Printf.printf "tool-parallel: all %d checks passed\n" !checks
