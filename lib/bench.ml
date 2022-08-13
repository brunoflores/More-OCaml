module Test = struct
  type t = { name : string; f : unit -> unit -> unit }

  let name { name; _ } = name
  let create ~name f = { name; f }
end

type result = { test : Test.t; duration_init : float; duration_test : float }

let display (results : result list) =
  List.iter
    (fun r ->
      Printf.printf "%s: %f: %f\n" r.test.name r.duration_init r.duration_test)
    results

let ints n =
  let state = Hashtbl.create n in
  let choice = ref 0 in
  let list =
    List.init n (fun _ ->
        let attempt () =
          let i = Random.int (n + 1) in
          if not (Hashtbl.mem state i) then (
            choice := i;
            Hashtbl.add state i ())
        in
        while !choice = 0 do
          attempt ()
        done;
        let v = !choice in
        choice := 0;
        v)
  in
  list

let run (tests : Test.t list) =
  let run_one ({ Test.f = init; _ } as test) =
    let start = Unix.gettimeofday () in
    let initialised = init () in
    let duration_init = Unix.gettimeofday () -. start in
    let start = Unix.gettimeofday () in
    let _ = initialised () in
    let duration_test = Unix.gettimeofday () -. start in
    { test; duration_init; duration_test }
  in
  let rec run_all results tests =
    match tests with
    | [] -> results
    | test :: more -> run_one test :: run_all results more
  in
  let results = run_all [] tests in
  display results
