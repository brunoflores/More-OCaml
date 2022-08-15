open More_ocaml

let insertion =
  Random.self_init ();
  let n = 50_000 in
  let ord = List.init n (fun i -> i) in
  let unord = List.init n (fun _ -> Random.int (n + 1)) in
  let t name f = Core_bench.Bench.Test.create f ~name in
  [
    t "insertion ordered" (fun () -> ignore (Sets.SetList.set_of_list ord));
    t "insertion unordered" (fun () -> ignore (Sets.SetList.set_of_list unord));
  ]
  |> Core_bench.Bench.make_command

let membership =
  Random.self_init ();
  let n = 50_000 in
  let ord = List.init n (fun i -> i) in
  let unord = List.init n (fun _ -> Random.int (n + 1)) in
  let indices = List.init 100_000 (fun i -> i) in
  let ordered = Sets.SetList.set_of_list ord in
  let unordered = Sets.SetList.set_of_list unord in
  let t name f = Core_bench.Bench.Test.create f ~name in
  [
    t "membership ordered" (fun () ->
        List.iter
          (fun i ->
            let _ = Sets.SetList.member i ordered in
            ())
          indices);
    t "membership unordered" (fun () ->
        List.iter
          (fun i ->
            let _ = Sets.SetList.member i unordered in
            ())
          indices);
  ]
  |> Core_bench.Bench.make_command

let () =
  let open Core in
  Command_unix.run
    (Command.group ~summary:"Several benchmakrs"
       [ ("insertion", insertion); ("membership", membership) ])
