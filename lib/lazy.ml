type 'a list = Nil | Cons of 'a * 'a list

(* No Nil constructor because a lazy list has no end *)
type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

(* given an integer n, build the lazy list of all integers n, n+1, n+2 ... *)
let rec lseq n = Cons (n, fun () -> lseq (n + 1))
let lhd (Cons (n, _)) = n
let ltl (Cons (_, tf)) = tf () (* force evaluation of the tail *)

let rec ltake (Cons (h, tf)) n =
  match n with 0 -> [] | _ -> h :: ltake (tf ()) (n - 1)

let rec ldrop (Cons (_, tf) as ll) n =
  match n with 0 -> ll | _ -> ldrop (tf ()) (n - 1)

let%test _ = ltake (lseq 0) 5 = [ 0; 1; 2; 3; 4 ]
let%test _ = ltake (ldrop (lseq 0) 20) 5 = [ 20; 21; 22; 23; 24 ]

let rec lmap f (Cons (h, tf)) = Cons (f h, fun () -> lmap f (tf ()))

let rec lfilter f (Cons (h, tf)) =
  if f h then Cons (h, fun () -> lfilter f (tf ())) else lfilter f (tf ())

(* find the cubes divisible by five *)
let cubes = lfilter (fun x -> x mod 5 = 0) (lmap (fun x -> x * x * x) (lseq 1))

(* this test will actually compute all the cubes from 1 to 27000, discarding
   everything that does not pass the filter:

   1, 8, 27, 64, 125, 216, 343, 512, 729, 1000, 1331, 1728, 2197, 2744, 3375,
   4096, 4913, 5832, 6859, 8000, 9261, 10648, 12167, 13824, 15625, 17576,
   19683, 21952, 24389, 27000 *)
let%test _ = ltake cubes 5 = [ 125; 1000; 3375; 8000; 15625 ]
