(* ------------------------------------------------------ *)
(*   List examples                                        *)
(* ------------------------------------------------------ *)

(* An example type for integer lists. 
   Note that these are *separate* from the standard OCaml type 
   for lists, which are 'a lists. *)
type lst = 
  | Nil
  | Cons of int * lst

(* You build a value of type `lst` by using it's constructors.  You
   build it in a "russian nesting doll" fashion: start with a base
   constructor (a constructor that doesn't talk about `lst`
   recursively) and apply constructors to construct larger values.
*)

let a = Cons (23, Cons(13, Nil))

let add_another = Cons (1, a)

(* Calculate the length of an `lst`.
 * 
 * Note that (a : lst) means the parameter will be of type `lst`, and
 * `: int` means the result will be int 
 *)
let rec length (a : lst) : int = match a with (* I use `match` to consider the possible 
                                                 cases for `a` *)
  | Nil -> 0 (* empty list is length zero *)
  | Cons (_, tl) -> 1 + length tl (* Note I used a wildcard `_` because I didn't care
                                    what the element's value was. *)

(* Concatenate two lists.  Note that if I leave off the type
   annotations, OCaml will infer them.
*)

let rec concat a b : lst = 
  match a with
  | Nil -> b
  | Cons (hd,tl) -> Cons (hd, concat tl b)

let a_twice = concat a a
                      
(* Instructor's note: Don't be confused by `lst`.  It's *not* the same
 * thing as the standard library type `list`.  It's just a type that
 * behaves the same way. In particular, I can't use `List.length` on
 * elements of type `lst`.  If this confuses you, please ask for 
 * clarification.  It's just like if I defined a class `MyList` in Ruby, 
 * and defined some methods on it.  I couldn't use the standard array 
 * methods on `MyList`, only methods I explicitly defined.
 *)

(* ------------------------------------------------------ *)
(*   Binary tree examples                                 *)
(* ------------------------------------------------------ *)

(* A binary tree is a leaf, or a node that contains two subtrees. *)
type tree = 
  | Leaf of int
  | Node of int * tree * tree

(* An example tree *)
let example_tree : tree = 
  Node (23,(Leaf 13),(Leaf 42))

let rec height t = match t with
  | Leaf _ -> 1 (* A leaf has height 1 *)
  | Node (_,t1,t2) -> 1 + max (height t1) (height t2) (* I use the built in `max` *)

(* A tree is balanced when its children are balanced and have equal height *)
let rec balanced t = match t with
  | Leaf _ -> true 
  | Node (_,t1,t2) -> (balanced t1) && (balanced t2) && (height t1 = height t2)

(* Binary search *)
let rec includes t i = match t with
  | Leaf x -> x = i
  | Node (x,t1,t2) -> 
    if (x = i) then
      true
    else (if (x > i) then
            includes t2 i
          else 
            includes t1 i)

(* Turn a tree into a list of elements. *)
let rec elements (t : tree) : int list =
  match t with
  | Leaf i -> [i] (* Note that [i] = i :: [] *)
  | Node(i,t1,t2) -> [i] @ (elements t1) @ (elements t2) 

let elems_of_example = elements example_tree

(* ------------------------------------------------------ *)
(*   Practice problems                                    *)
(* ------------------------------------------------------ *)

(* Calculate the min element of a sorted tree.  Hint: Go down the left
   spine of the tree.  When you get to a point where you have nowhere
   else to go (a leaf), you've got the min element. *)
let rec min (t : tree) = failwith "undefined"  (* Note that you can use `failwith`
                                                  to generate a runtime exception *)

let rec max (t : tree) = failwith "undefined"

(* Insert i into sorted binary tree t.
 * As an example, to insert 12 into the following binary tree:
 *     
 *                       23
 *                     /    \ 
 *                    14    25
 *                    
 * I would get:
 *                       23
 *                     /    \ 
 *                    12    25
 *                      \
 *                      14
 *)
let rec insert (t : tree) (i : int) : tree = 
  failwith "undefined" 

(* Insert each element of l into t *)
let rec insert_many (t : tree) (i : int list) : tree =
  failwith "undefined"



