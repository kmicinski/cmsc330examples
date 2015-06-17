(* 
 * Higher order functions, parameter passing, and tail recursion.
 * 
 * CMSC 330, June 17, 2015
 *)

(*--------------------------------------------------------*)
(*  Introduction                                          *)
(*--------------------------------------------------------*)

(* Today we're going to talk about higher order functions.  Higher order functions
 * are functions that take other functions as their input.  First, let's show an
 * example of a function that is *not* higher order.
 *)

let plusone x = x + 1

(* This function has type:
 *   sum : int -> int
 * 
 * This function takes an int and produces another int.
 *)

(*
 * Note that this is simply a notational convenience for the syntax:
 *)

let plusone' = fun x -> x + 1

(* The `fun x ->` creates an anonymous function, whose body is `x + 1`.  To
 * call* a function in OCaml, we take it and put it next to its argument
 * (juxtaposition):
 *)

let x = (fun x -> x + 1) 23 (* Could have also written `plusone' 23` *)

(* These anonymous functions are sometimes called **lambdas**. *)

(*--------------------------------------------------------*)
(*  Infinite loops                                        *)
(*--------------------------------------------------------*)

(* When OCaml evaluates `(fun x -> x + 1) 23`, it takes the argument (in
   this case, 23), evaluates it, and then substitutes the argument
   into the body of the function. 

   The process of taking the (evaluated) argument and *slamming it
   into* the body of the function is called **reduction**.  Evaluation in functional
   languages happens primarily by means of reduction.

   Try doing it yourself: 
   
       (fun x -> x + 1) (23 + 42) 
       = 
       (fun x -> x + 1) (65)                     (evaluating argument)
       = 
       (65 + 1)                               (reduction) 
       = 
       66

       (fun x -> x + 1) ((fun x -> x) 13)          
       = 
       (fun x -> x + 1) (13)                     (evaluating argument via reduction)
       = 
       14                                     (reduction) 
       
   Now, here's a slightly trickier one.
   
   Let's say I define the following function:
*)

let rec loop = fun x -> loop x

(* What happens when I do the following: 
   
   loop 23 
   = 
   (fun x -> loop x) 23 
   = 
   loop 23                                    (reduction)                        
   = 
   (fun x -> loop x) 23  
   = 
   ...
   = 
   loop 23
   = 
   ...

   Infinite loop.
   
   Now let's say I do *this*.
*)

let x = (fun y -> 23) (loop 23)

(*--------------------------------------------------------*)
(*  A lecture on values                                   *)
(*--------------------------------------------------------*)

(* 
   I get..
   
   (fun y -> 23) (loop 23)
   = 
   (fun y -> 23) ((fun x -> loop x) 23)
   = 
   (fun y -> 23) (loop 23)                       (reducing argument)
   = 
   ...
   
   Another infinite loop!

   Why do I get this behavior?  Because whenever I want to reduce
   (call) functions in OCaml, I first evaluate their arguments.  This is why
   we call OCaml a *call by value* language.
   
   OCaml terms of the following form are called function applications.
   
   e1 e2

   (e.g., in the last example e1 = (fun y -> 23) and e2 = (loop 23)) We
   know we have to *apply* e1 to e2.  Notice that it's e1 to e2, **not**
   e2 to e1.  E.g., if we had `f x`, we would say that `f` is applied
   to `x`.  The function is always the thing being applied.

   In the following expression:
   
       (fun x -> x + 1) ((fun x -> x) 13)          
       ^                          ^
       ----------------------------
                 \     /
           top level application

   OCaml first evaluates the argument of (fun x -> x + 1), which is ((fun x
   -> x) 13).  Why doesn't OCaml evaluate ((fun x -> x) 13) to 13 first?
   Because function application happens from *outside in*.  
   
   In OCaml, values are terms of the following form:
     - (fun x -> ...)                   (functions)
     - x                             (variables) 
     - 23                            (usual primitive values)
     - Ctr (v1,...,vn)               (constructors and their arguments) 

   In other words, values are the *basic things* in the language.
   They are the places where computation ends.  When I evaluate the
   following expression:

       ((fun y -> y) (fun x -> x + 1))  ((fun x -> x) 23)
       |                 e1     |       e2      |
       |------------------------|---------------|

   I must do the following steps:
     - Evaluate e1 to a value, of the form `fun x -> ...`, calling that v1
     - Evaluate e2 to a value, v2
     - Take v2 and substitute it into v1 for x, 
     - Evaluate the result
   
   So we do:
     - Evalute e1:
       - Evalute e1's argument to a value, which it already is (fun x -> x + 1)
       - Apply e1's argument to (fun y -> y)
       - Get (fun x -> x + 1), which is v1
     - Evaluate e1's argument: 
       - Take (fun x -> x) and apply it to 23 via reduction
       - Get v2 = 23
     - Take v1 and apply it to v2 via reduction: `(fun x -> x + 1) 23`
       - 23 + 1
       - Evaluate that: 24
   
   Note, that applications, `e1 e2`, are explicitly **not** values.  If
   you see an application `e1 e2`, you *have* to evaluate the
   application before you're allowed to end the computation.

   If it's confusing to you, remember that 23 + 1 is just syntax for
   something like `plus 23 1`, where `plus` is a builtin plus operator
   for integers that has type `int -> int -> int`.
 *)

(* 
   Now, what happens when we write the following expression:
 *)

let x = (fun x -> "hello") loop

(* The application evaluates to "hello".  But why can this be!?
   `loop` is an infinite loop!  The reason is that `loop` = `fun x ->
   loop x`.  So when we evaluate:

    (fun x -> "hello") loop
    =                                         (evalute argument)
    (fun x -> "hello") (fun x -> loop x)
    =                                         (reduction)
    "hello"

   See!  Even though loop will create an infinite loop if *called*.
   It's never *called*, so the infinite loop never executes.

   It's kind of like if I a procedure in C named `loopforever`, and
   pass a *pointer* `loopforever` to a procedure.  If I only pass the
   pointer to `loopforever`, then I never actually *call* it unless I
   do so explicitly.
   
   So to recap:

     - Values are things where computation ends.  The values I want to
     *end up with*.
     - `fun x -> ...` is a value.
     - Applications are **not** values.  You have to compute them to a value.
*)

(*--------------------------------------------------------*)
(*  Mapping                                               *)
(*--------------------------------------------------------*)

(* Very frequently I'll have a list of values, and I'll want to do
   something to them.

   For example, let's say I want to convert a list of integers to a
   floating point value.
   
   I could write the following function:
*)

let rec convert_to_floats : int list -> float list = fun l ->
  match l with
  | [] -> []
  | hd::tl -> (float_of_int hd) :: (convert_to_floats tl)
                                   
(* Note I could have also written... 

let rec convert_to_floats (l : int list) : float list = 
   ...

   The latter is just an abbreviation for the former.  Note that the ->
   goes away and the type annotation goes before the :
*)

(* This is all fine and well, but it's actually a more general case of
   a pattern we see a lot in functional programming called *mapping* over a
   list.
   
   If we have a list
   
   +--------|--------|--------|--------+
   |   x1   |   x2   |   x3   |   x4   |
   +--------|--------|--------|--------+

   I could imagine taking an f, and applying it pointwise to each
   element of that list:

   +--------|--------|--------|--------+
   |   x1   |   x2   |   x3   |   x4   |
   +--------|--------|--------|--------+
       |        |         |        | 
       v        v         v        v
       f        f         f        f 
       |        |         |        | 
       v        v         v        v
   +--------|--------|--------|--------+
   |  f(x1) |  f(x2) |  f(x3) |  f(x4) |
   +--------|--------|--------|--------+

   I'm going to write a function, called `map`, that does this:
   
   
*)

let rec map (f : 'a -> 'b) (l : 'a list) : 'b list = 
  match l with
  | [] -> []
  | hd::tl -> (f hd) :: (map f tl)

(*
   Let's see what happens when I apply `map float_of_int [1;2]`
   
   map float_of_int [1;2] 
     =             
     (float_of_int 1) :: (map float_of_int [2]) 
     =             
     (1.0) :: (map float_of_int [2])     
     =             
     (1.0) :: (float_of_int 2 :: (map float_of_int []))
     =             
     (1.0) :: (2.0 :: (map float_of_int []))
     =             
     (1.0) :: (2.0 :: [])
     = 
     [1.0; 2.0]
   
   We see that for any list `l`, we have `map f [x1;...xn]` = 
   [f x1; f x2; ...; f xn]
*)

(*--------------------------------------------------------*)
(*  Folding                                               *)
(*--------------------------------------------------------*)

(*
   Let's think about the following list

   +--------|--------|--------+
   |   x1   |   x2   |   x3   |
   +--------|--------|--------+

  Let's say that I wanted to sum each element in the list.  I could
  write a function like this:
 *)

let rec sum_list (l : 'a list) = 
  match l with
  | [] -> 0
  | hd :: tl -> hd + (sum_list tl)

(*
 * 
 *)

