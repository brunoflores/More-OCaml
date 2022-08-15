type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term

let rec eval : type a. a term -> a = function
  | Int n -> n
  | Add -> fun x y -> x + y
  | App (f, x) -> (eval f) (eval x)

let sort_uniq (type s) (cmp : s -> s -> int) =
  let module S = Set.Make (struct
    type t = s

    let compare = cmp
  end) in
  fun l -> S.elements (List.fold_right S.add l S.empty)

let () =
  let add_one = App (Add, Int 1) in
  Printf.printf "%d\n" (eval (App (add_one, Int 1)))

let () =
  let sorted = sort_uniq Int.compare [ 2; 2; 1; 4 ] in
  List.iter (Printf.printf "%d") sorted;
  print_endline ""

module Maybe : Monad.Monad with type 'a m = 'a option = struct
  type 'a m = 'a option

  let return x = Some x
  let bind m f = match m with None -> None | Some x -> f x
  let ( >>= ) = bind
  let compose f g x = f x >>= fun y -> g y
  let ( >=> ) = compose
end

let computation' x y =
  let ( let* ) = Maybe.( >>= ) in
  let* v1 = Maybe.return x in
  let* v2 = Maybe.return y in
  Maybe.return @@ (v1 + v2)

let () =
  let open Maybe in
  (* a pure function: *)
  let incr = ( + ) in
  let computation x y =
    Maybe.return x >>= fun x ->
    Maybe.return y >>= fun y -> Maybe.return (incr x y)
  in
  (match computation 41 1 with None -> () | Some v -> Printf.printf "%d\n" v);
  match computation' 41 1 with None -> () | Some v -> Printf.printf "%d\n" v

module Writer : Monad.Monad with type 'a m = 'a * string = struct
  type 'a m = 'a * string

  let return x = (x, "")

  let bind m f =
    let v1, s1 = m in
    let v2, s2 = f v1 in
    (v2, s1 ^ s2)

  let ( >>= ) = bind
  let compose f g x = f x >>= fun y -> g y
  let ( >=> ) = compose
end

let ( >> ) f g x = x |> f |> g
let inc x = x + 1
let dec x = x - 1
let id = inc >> dec

module Loggable = struct
  open Writer

  let return = Writer.return

  let log (name : string) f : 'a -> 'a * string =
   fun x -> (f x, Printf.sprintf "Called %s on %i; " name x)

  let loggable (name : string) f : 'a * string -> 'a * string =
   fun m ->
    m >>= fun x ->
    log name f x >>= fun y -> return y

  let ( >=> ) = Writer.( >=> )
end

let () =
  let open Loggable in
  let inc_loggable = loggable "inc" inc in
  let dec_loggable = loggable "dec" dec in
  let id_loggable = inc_loggable >> dec_loggable in
  let what = log "inc" inc >=> log "dec" dec in
  (* *)
  let _ = id 42 in
  (* *)
  let v, l = id_loggable (return 42) in
  print_endline @@ string_of_int v;
  print_endline l;
  (* *)
  let v, l = what 42 in
  print_endline @@ string_of_int v;
  print_endline l
