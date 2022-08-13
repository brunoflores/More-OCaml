(* folding over lists: *)

let rec fold_left f a l =
  match l with [] -> a | h :: t -> fold_left f (f a h) t

let all l = fold_left ( && ) true l
let any l = fold_left ( || ) false l
let setify l = fold_left (fun a e -> if List.mem e a then a else e :: a) [] l

let rec fold_right f l a =
  match l with [] -> a | h :: t -> f h (fold_right f t a)

let map f l = fold_right (fun e a -> f e :: a) l []

(* tail-recursive fold right with the cost of a reversal *)
let fold_right_tr f l e = fold_left (fun a e -> f e a) e (List.rev l)
let append x y = fold_right (fun e a -> e :: a) x y
let copy l = append l [] (* copy in terms of append *)

(* split a list of pairs into a pair of lists *)
let split l = fold_right (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) l ([], [])

(* folding over trees: *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec fold_tree f e t =
  match t with
  | Lf -> e
  | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)

let tree_size t = fold_tree (fun _ l r -> 1 + l + r) 0 t
let tree_sum t = fold_tree (fun x l r -> x + l + r) 0 t

(* standard tree traversals using a list accumulator: *)
let tree_preorder t = fold_tree (fun x l r -> [ x ] @ l @ r) [] t
let tree_inorder t = fold_tree (fun x l r -> l @ [ x ] @ r) [] t
let tree_postorder t = fold_tree (fun x l r -> l @ r @ [ x ]) [] t

let%test _ = 1 = 1

let%test _ =
  let t = Br (1, Br (0, Lf, Lf), Br (6, Br (4, Lf, Lf), Lf)) in
  tree_preorder t = [ 1; 0; 6; 4 ]
  && tree_inorder t = [ 0; 1; 4; 6 ]
  && tree_postorder t = [ 0; 4; 6; 1 ]
