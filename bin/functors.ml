type comparison = Less | Equal | Greater

module type ORDERED_TYPE = sig
  type t

  val compare : t -> t -> comparison
end

module type SETFUNCTOR = sig
  type element
  type set

  val empty : set
  val add : element -> set -> set
  val member : element -> set -> bool
end

module Set (Elt : ORDERED_TYPE) : SETFUNCTOR with type element = Elt.t = struct
  type element = Elt.t
  type set = element list

  let empty = []

  let rec add x s =
    match s with
    | [] -> [ x ]
    | hd :: tl -> (
        match Elt.compare x hd with
        | Equal -> s (* x is already in s *)
        | Less -> x :: s (* x is smaller than all elements of s *)
        | Greater -> hd :: add x tl)

  let rec member x s =
    match s with
    | [] -> false
    | hd :: tl -> (
        match Elt.compare x hd with
        | Equal -> true (* x belongs to s *)
        | Less -> false (* x is smaller than all elements of s *)
        | Greater -> member x tl)
end

module OrderedString = struct
  type t = string

  let compare x y = if x = y then Equal else if x < y then Less else Greater
end

let () =
  let module AbstractStringSet = Set (OrderedString) in
  let s = AbstractStringSet.add "gee" AbstractStringSet.empty in
  let _ = AbstractStringSet.member "gee" s in
  ()
