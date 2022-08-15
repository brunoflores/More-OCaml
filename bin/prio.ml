module type PrioQueueSig = sig
  type priority = int
  type 'a queue

  exception Queue_is_empty

  val empty : 'a queue
  val insert : 'a queue -> priority -> 'a -> 'a queue
  val remove_top : 'a queue -> 'a queue
  val extract : 'a queue -> priority * 'a * 'a queue
end

module PrioQueue : PrioQueueSig = struct
  type priority = int
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

  let empty = Empty

  let rec insert queue prio elt =
    match queue with
    | Empty -> Node (prio, elt, Empty, Empty)
    | Node (p, e, left, right) ->
        if prio <= p then Node (prio, elt, insert right p e, left)
        else Node (p, e, insert right prio elt, left)

  exception Queue_is_empty

  let rec remove_top = function
    | Empty -> raise Queue_is_empty
    | Node (_, _, left, Empty) -> left
    | Node (_, _, Empty, right) -> right
    | Node
        ( _,
          _,
          (Node (lprio, lelt, _, _) as left),
          (Node (rprio, relt, _, _) as right) ) ->
        if lprio <= rprio then Node (lprio, lelt, remove_top left, right)
        else Node (rprio, relt, left, remove_top right)

  let extract = function
    | Empty -> raise Queue_is_empty
    | Node (prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
end

module PrioQueueOpt : sig
  include PrioQueueSig

  val _remove_top_opt : 'a queue -> 'a queue option
end = struct
  include PrioQueue

  let _remove_top_opt x = try Some (remove_top x) with Queue_is_empty -> None
end
