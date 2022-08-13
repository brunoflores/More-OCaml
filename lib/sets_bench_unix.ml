open More_ocaml

let n = 50_000
let nums = List.init n (fun i -> i)
let rand = Bench.ints n
let indices = List.init n (fun _ -> Random.int 100_000)

let implementations =
  [
    ("Lists", (module Sets.SetList : Sets.SetType));
    ("Trees", (module Sets.SetTree : Sets.SetType));
    ("Red-black trees", (module Sets.SetRedBlack : Sets.SetType));
    ("Hash tables", (module Sets.SetHashtbl : Sets.SetType));
  ]

let insertion name list (s : (module Sets.SetType)) =
  let module S = (val s) in
  let test () () = ignore @@ S.set_of_list list in
  Bench.Test.create ~name test

let membership name list (s : (module Sets.SetType)) =
  let module S = (val s) in
  let test () =
    let set = S.set_of_list list in
    fun () -> List.iter (fun i -> ignore @@ S.member i set) indices
  in
  Bench.Test.create ~name test

let tests =
  List.fold_left
    (fun acc (impl_name, impl_md) ->
      let insertion_sorted =
        insertion
          (Format.sprintf "%s: %s" impl_name "insertion sorted")
          nums impl_md
      in
      let insertion_unsorted =
        insertion
          (Format.sprintf "%s: %s" impl_name "insertion unsorted")
          rand impl_md
      in
      let membership_sorted =
        membership
          (Format.sprintf "%s: %s" impl_name "membership sorted")
          nums impl_md
      in
      let membership_unsorted =
        membership
          (Format.sprintf "%s: %s" impl_name "membership unsorted")
          rand impl_md
      in
      insertion_sorted :: insertion_unsorted :: membership_sorted
      :: membership_unsorted :: acc)
    [] implementations

let () = Bench.run tests
