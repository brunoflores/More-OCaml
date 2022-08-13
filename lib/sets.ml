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

  let list_of_set x = x
  let insert x l = x :: l

  let rec set_of_list l =
    match l with [] -> [] | h :: t -> insert h (set_of_list t)

  let size = List.length
  let member = List.mem
end
