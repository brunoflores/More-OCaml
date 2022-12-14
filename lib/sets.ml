module type SetType = sig
  type 'a t

  val set_of_list : 'a list -> 'a t
  val list_of_set : 'a t -> 'a list
  val insert : 'a -> 'a t -> 'a t
  val size : 'a t -> int
  val member : 'a -> 'a t -> bool
end

module SetList : SetType = struct
  type 'a t = 'a list

  let size = List.length
  let member = List.mem
  let list_of_set x = x
  let insert x l = if member x l then l else x :: l

  let rec set_of_list l =
    match l with [] -> [] | h :: t -> insert h (set_of_list t)
end

module SetTree : SetType = struct
  type 'a t = Lf | Br of 'a t * 'a * 'a t

  let rec list_of_set s =
    match s with
    | Lf -> []
    | Br (l, x, r) -> list_of_set l @ [ x ] @ list_of_set r

  let rec insert x s =
    match s with
    | Lf -> Br (Lf, x, Lf)
    | Br (l, y, r) as b ->
        if x = y then b
        else if x < y then Br (insert x l, y, r)
        else Br (l, y, insert x r)

  let rec set_of_list l =
    match l with [] -> Lf | h :: t -> insert h (set_of_list t)

  let rec size s = match s with Lf -> 0 | Br (l, _, r) -> 1 + size l + size r

  let rec member x s =
    match s with
    | Lf -> false
    | Br (_, y, _) when x = y -> true
    | Br (l, y, r) -> if x < y then member x l else member x r
end

module SetRedBlack : SetType = struct
  type colour = R | B
  type 'a t = Lf | Br of colour * 'a t * 'a * 'a t

  let rec list_of_set s =
    match s with
    | Lf -> []
    | Br (_, l, x, r) -> (x :: list_of_set l) @ list_of_set r

  let balance t =
    match t with
    | B, Br (R, Br (R, a, x, b), y, c), z, d
    | B, Br (R, a, x, Br (R, b, y, c)), z, d
    | B, a, x, Br (R, Br (R, b, y, c), z, d)
    | B, a, x, Br (R, b, y, Br (R, c, z, d)) ->
        Br (R, Br (B, a, x, b), y, Br (B, c, z, d))
    | a, b, c, d -> Br (a, b, c, d)

  let rec insert_inner x s =
    match s with
    | Lf -> Br (R, Lf, x, Lf)
    | Br (c, l, y, r) ->
        if x < y then balance (c, insert_inner x l, y, r)
        else if x > y then balance (c, l, y, insert_inner x r)
        else Br (c, l, y, r)

  let insert x s =
    match insert_inner x s with
    | Br (_, l, y, r) -> Br (B, l, y, r)
    | Lf -> assert false

  let rec set_of_list l =
    match l with [] -> Lf | h :: t -> insert h (set_of_list t)

  let rec size s =
    match s with Lf -> 0 | Br (_, l, _, r) -> 1 + size l + size r

  let rec member x s =
    match s with
    | Lf -> false
    | Br (_, l, y, r) -> x = y || if x > y then member x r else member x l
end

module SetHashtbl : SetType = struct
  type 'a t = ('a, unit) Hashtbl.t

  let list_of_set s = Hashtbl.fold (fun x () l -> x :: l) s []

  let set_of_list l =
    let s = Hashtbl.create (List.length l) in
    List.iter (fun x -> Hashtbl.add s x ()) l;
    s

  let member x s = Hashtbl.mem s x

  let insert x s =
    if not (member x s) then Hashtbl.add s x ();
    s

  let size = Hashtbl.length
end
