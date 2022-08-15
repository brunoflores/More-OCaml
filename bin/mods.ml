module type S = sig
  val bump : unit -> int
end

module M : S = struct
  let counter = ref 0

  let bump () =
    let old = !counter in
    incr counter;
    old
end

let func (s : (module S)) =
  let module M = (val s) in
  let _ = M.bump () in
  ()

let () =
  Printf.printf "%d\n" (M.bump ());
  let _ = func (module M) in
  let _ = func (module M) in
  Printf.printf "%d\n" (M.bump ())

module type EQ = sig
  type t

  val eq : t -> t -> bool
end

module type LT = sig
  type t

  val lt : t -> t -> bool
end

module type ORD = sig
  include EQ
  include LT with type t := t
end

module Eq_int : EQ with type t = int = struct
  type t = int

  let eq = ( = )
end

module Lt_int : LT with type t = int = struct
  type t = int

  let lt = ( < )
end

module Ord_int : ORD with type t = int = struct
  include Eq_int
  include Lt_int
end

let () =
  let x = 1 in
  let y = 2 in
  assert ((not (Ord_int.eq x y)) && Ord_int.lt x y)

module type SHOW = sig
  type t

  val show : t -> string
end

type 'a show_impl = (module SHOW with type t = 'a)

module Show_int : SHOW with type t = int = struct
  type t = int

  let show = string_of_int
end

let show_int : int show_impl = (module Show_int)

let print : 'a show_impl -> 'a -> unit =
  fun (type a) (show : a show_impl) (x : a) ->
   let module Show = (val show : SHOW with type t = a) in
   print_endline @@ Show.show x

let print_int = print show_int
let () = print_int 42

module type NUM = sig
  type t

  val from_int : int -> t
  val ( + ) : t -> t -> t
end

type 'a num_impl = (module NUM with type t = 'a)

module Num_int : NUM with type t = int = struct
  type t = int

  let from_int x = x
  let ( + ) = ( + )
end

let num_int = (module Num_int : NUM with type t = int)

let sum : 'a num_impl -> 'a list -> 'a =
  fun (type a) (num : a num_impl) (ls : a list) ->
   let module Num = (val num : NUM with type t = a) in
   List.fold_right Num.( + ) ls (Num.from_int 0)

let () = assert (sum num_int [ 1; 2; 3; 4 ] = 10)

let print_incr : 'a show_impl * 'a num_impl -> 'a -> unit =
  fun (type a) ((show : a show_impl), (num : a num_impl)) (x : a) ->
   let module Num = (val num : NUM with type t = a) in
   let open Num in
   print show (x + from_int 1)

let print_incr_int (x : int) : unit = print_incr (show_int, num_int) x
let () = print_incr_int 41

let show_list : 'a show_impl -> 'a list show_impl =
  fun (type a) (show : a show_impl) ->
   let module Show = (val show : SHOW with type t = a) in
   (module struct
     type t = a list

     let show : t -> string =
      fun xs ->
       let rec go first = function
         | [] -> "]"
         | h :: t -> (if first then "" else ", ") ^ Show.show h ^ go false t
       in
       "[" ^ go true xs
   end : SHOW
     with type t = a list)

let () =
  let module Show = (val show_list show_int : SHOW with type t = int list) in
  print_endline @@ Show.show [ 1; 2; 3 ]

(* ------------------------------------------------ *)
(* https://okmij.org/ftp/Computation/typeclass.html *)
type 'a show = { show : 'a -> string }

let show_bool : bool show =
  { show = (function true -> "True" | false -> "False") }

let show_int : int show = { show = string_of_int }
let print : 'a show -> 'a -> unit = fun { show } x -> print_endline @@ show x

let () =
  print show_bool true;
  print show_int 42

type 'a num = { from_int : int -> 'a; add : 'a -> 'a -> 'a }

let sum : 'a num -> 'a list -> 'a =
 fun { from_int; add } ls -> List.fold_right add ls (from_int 0)

let num_int : int num = { from_int = (fun x -> x); add = Stdlib.( + ) }

let num_bool : bool num =
  {
    from_int = (function 0 -> false | _ -> true);
    add = (function true -> fun _ -> true | false -> fun x -> x);
  }

let () = assert (sum num_int [ 1; 2; 3 ] = 6)

let print_incr : 'a show * 'a num -> 'a -> unit =
 fun (show_dict, { from_int; add = ( + ) }) x -> print show_dict (x + from_int 1)

let print_incr_int : int -> unit = fun x -> print_incr (show_int, num_int) x
let () = print_incr_int 41

let show_list : 'a show -> 'a list show =
 fun { show } ->
  {
    show =
      (fun xs ->
        let rec go first = function
          | [] -> "]"
          | h :: t -> (if first then "" else ", ") ^ show h ^ go false t
        in
        "[" ^ go true xs);
  }

let () = assert ((show_list show_int).show [ 1; 2; 3 ] = "[1, 2, 3]")

type 'a eq = { eq : 'a -> 'a -> bool }

let eq_bool =
  {
    eq =
      (function
      | true -> ( function true -> true | _ -> false)
      | false -> ( function false -> true | _ -> false));
  }

let eq_int = { eq = (fun (x : int) (y : int) -> x = y) }

type 'a mul = { mul_super : 'a eq * 'a num; mul : 'a -> 'a -> 'a }

let mul_default : 'a eq * 'a num -> 'a mul =
 fun (({ eq }, { from_int; add = ( + ) }) as super) ->
  {
    mul_super = super;
    mul =
      (let rec loop x y =
         match () with
         | () when eq x (from_int 0) -> from_int 0
         | () when eq x (from_int 1) -> y
         | () -> y + loop (x + from_int (-1)) y
       in
       loop);
  }

let mul_bool : bool mul = mul_default (eq_bool, num_bool)
let mul_int : int mul = { mul_super = (eq_int, num_int); mul = Stdlib.( * ) }

let dot : 'a mul -> 'a list -> 'a list -> 'a =
 fun { mul_super = _eq, num; mul } xs ys -> sum num @@ List.map2 mul xs ys

let () =
  let v = dot mul_int [ 1; 2; 3 ] [ 4; 5; 6 ] in
  print_endline @@ string_of_int v;
  let v = dot mul_bool [ true; false; true ] [ false; true; true ] in
  print_endline @@ string_of_bool v
