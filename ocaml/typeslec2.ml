
let hd l = match l with
  | [] -> 0
  | hd::tl -> hd

type int_tree = 
  | Leaf of int
  | Node of int * int_tree * int_tree

let x = Node(12,Leaf(11),Leaf(13))

let rec find_max (t: int_tree) : int = match t with
    | Leaf (i) -> i
    | Node (x,t1,t2) -> find_max t2
  
let rec search t x : bool = match t with
  | Leaf i         -> (i = x)
  | Node (i,t1,t2) -> 
    if (i = x) then true
    else
    (if (x > i) then
       (search t2 x)
     else 
       (search t1 x))
    
type 'a tree = 
  | Leaf of 'a 
  | Node of 'a * 'a tree * 'a tree

type int_tree = int tree

type ('a, 'b) pair =
  | Pair of 'a * 'b


