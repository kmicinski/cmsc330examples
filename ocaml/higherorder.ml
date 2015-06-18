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

(* When OCaml evaluates `(fun x -> x + 1) 23`, it takes the argument
   (in this case, 23), evaluates it, and then substitutes the argument
   into the body of the function.

   The process of taking the (evaluated) argument and *slamming it
   into* the body of the function is called **reduction**.  Evaluation
   in functional languages happens primarily by means of reduction.

   Try doing it yourself: 

       (fun x -> x + 1) (23 + 42) 
       = 
       (fun x -> x + 1) (65)                  (evaluating argument)
       = 
       (65 + 1)                               (reduction) 
       = 
       66

       (fun x -> x + 1) ((fun x -> x) 13)          
       = 
       (fun x -> x + 1) (13)                  (evaluating argument via reduction)
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
     - (fun x -> ...)                (functions)
     - x                             (variables) 
     - 23                            (usual primitive values)
     - Ctr (v1,...,vn)               (constructors and their arguments) 

   In other words, values are the *basic things* in the language.
   They are the places where computation ends.  When I evaluate the
   following expression:

       ((fun y -> y) (fun x -> x + 1))  ((fun x -> x) 23)
       |               e1              |       e2        |
       |-------------------------------|-----------------|

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

(* Let's think about another way that I could write `sum_list`, using
   an explicit accumulator.  Remember, we talked about the fact that
   we could thread state through recursive calls.  That is to say, we
   could pass an accumulator to `sum_list` (or rather, a helper
   function) explicitly:
*)

let sum_list' (l : 'a list) = 
  let rec h acc l = match l with
    | [] -> acc
    | hd::tl -> h (acc + hd) tl
  in
  h 0 l

(*--------------------------------------------------------*)
(*  A diversion: tail recursion                           *)
(*--------------------------------------------------------*)

(* The function we just defined, `h`, exhibits a specific type of
   recursion, named tail recursion.

   When the last thing a function does is call another function, we
   call that call a *tail call*.  For example, the call `f x` is a
   tail call here:
*)
let rec f x = 
  if (x = 0) then 1
  else 
    f (x-1)
   
(*
   By contrast, this function is *not* tail recursive:
*)
let fib x = 
  if (x = 0 || x = 1) then 1
  else 
    (f (x-1)) + (f (x-2))

(*
   Why isn't it tail recursive?  Well, ask yourself this question: is
   calling `f` the *last* thing that happens for `fib 2`?  It's not.
   First, we call `f (x-1)`, then `f (x-2)`, then we have to *use*
   those results to compute with them (in this case, to add them).

   Tail recursive functions are awesome because we can optimize them
   to not use the stack.  For example, think about what `fib 2` does:
   
   fib 2 = 
           f 1 + f 0
           ^-^   ^-^
            |     |
            -------
   create stack frames and execute
   
   Now let's think about what happens when we call:
   
   fib 3 
   = 
   f 2 + f 1
   = 
   (f 1 + f 0) + (1)
   = 
   (2) + (1)

   If you think back to your class 216 on low level programming,
   you'll know that creating a stack frame potentially wastes space.
   It turns out there's a nice way to optimize tail calls that
   basically just turns them into loops.  Here's another way we could
   write the fibonacci function:
*)

let tailrecfib x =
  let rec h a b num =
    if (num) = x then a
    else h b (a+b) (num+1)
  in
  h 1 1 1
    
(* Now, it's not obvious that this computes the fibonacci sequence.  I
   have to admit, I personally had to think hard about this for a
   minute or two.  To see why it does, let's think about how I would
   compute the fibonacci sequence by hand.
   
     1 1 2
      \|\| 
       2 3 5
        \|\| 
         5 8 13
          \|\| 
          13 21 34

   What did I do when I thought about this?  I wrote the first two
   elements of the sequence.  Then I summed them to get the third (2).
   Then I use the previous two to get the fourth, and so on.

   This is **exactly** what our function does.
   
   (assume x = 5)
   h 1 1 1 
   = \/|
   h 1 2 2
   = \/|
   h 2 3 3
   = \/|
   h 3 5 4
   = \/|
   h 5 8 5

   See how, in the evaluation of `h`, the second argument switches to
   become the first, and the new second argument is computed with the
   sum of the first two.  The last argument is just to know when to
   stop.  It's a counter to keep track of how many calls we've made so
   far.

   Note that everything here is just threading the state through `h`,
   like we've seen previously, just more complex.

   You see, this is because fibonacci is not a function only of the
   last number in the sequence.  Because if it were, we could easily
   make a tail recursive function out of it.  Instead, the fibonacci
   function is a function of the last **two** elements in the
   sequence.

   Now, why is `h` a tail call?  because `h` never needs to do any
   work after calling `h`.

   Tail calls have a special property: they never need to return.

   let rec h a b num =
     if (num) = x then a
     else h b (a+b) (num+1)

   See, because the function `h` never does anything *with* the result
   of its tail call, why create a stack frame at all?  In fact, tail
   calls are optimized to `goto`s.  In other words, that code will be
   turned into something like this (in C) when it's compiled:
   
   int h(int a, int b, int num) {
      int a' = a;
      int b' = b;
      int num' = num;
      while (num <> x) {
        a = b
        b = a+b
        num = num+1
      }
   }

   The fact that `h` is a tail call allows us to do this, because tail
   calls are never *used*.

   BIG NOTE:

   Writing functions in a tail recursive way is a small optimization.
   Compilers (including ocamlc) for functional languages have gotten
   very good at optimizing your code.  It's sometimes useful to know
   when it might be helpful to use a tail call rather than a non-tail
   call, but don't sweat it too much.  Premature optimization is the
   root of all evil, and whatnot.
*)

(*--------------------------------------------------------*)
(*  Back to fold                                          *)
(*--------------------------------------------------------*)

(* Now, back to sum_list', which we know is tail recursive:

let sum_list' (l : 'a list) = 
  let h acc l = match l with
    | [] -> acc
    | hd::tl -> h (acc + hd) tl
  in
  h 0 l

   It turns out that sum_list' is invoking a generic pattern over
   lists.  It's *folding* the plus operator over the list.  We might
   also say that it's accumulating the plus operator over the list.

   What do I mean by this.  Well, consider that we have the list: 
   
   [13; 52; 12] 
   
   What happens when I write
   
   (plus 13 (plus 52 (plus 12 0) ) )

   (Note I used the function `plus x y = x+y` rather than the infix
   operator just to bring the point home.)

   What if we had a function that did:

   (f (f (f 0 x1) x2) x3)

   For a generic function `f`?  Then we could slide in anything we
   wanted!  We could slide in `f = fun x y -> x+y`, or we could slide
   in `f = fun x y -> x*y`.

   But it would be kind of stupid to use 0 when we had `f = fun x y ->
   x*y`.  It would just turn everything to zero:
   
   ( ( (0 * x1) * x2) * x3)

   Insted, we need to use 1.  Notice that 0 is the identity for +, and
   1 is the identity for *.
   
   So, now we're going to generalize what our function does:

   (f (.. (f (f i x1) x2) .. xn)
    ^         ^
     ---------
     n fs here , where n = length(x)

   Our function is going to be called fold.  It's going to accept a
   list, a function that accepts two arguments (the *current* value,
   and the *next* value), and an initial value:
   
*)

let fold (update : 'a -> 'b -> 'a) (init : 'a) (lst : 'b list) : 'a =
  let rec h acc l = 
    match l with
    | [] -> acc
    | hd::tl -> h (update acc hd) tl
  in
  h init lst

(* 
   Now, let's write some example uses of fold
 *)

let sum_list = fun l -> fold (fun x y -> x+y) 0 

let mul_list = fun l -> fold (fun x y -> x*y) 0 

(* Take a list of integers, and concatenate all of them into a big
   string *)
let concat_ints_to_string : int list -> string = fun l ->
  fold (fun x y -> (string_of_int y ^ "|" ^ x)) "" l

(*--------------------------------------------------------*)
(*  Practice                                              *)
(*--------------------------------------------------------*)

(* Use fold to define a function that filters out all negative
   integers from a list
   
   filter [0; -2; 5] = [0;5]
 *)

let filter l = fold (fun acc next -> failwith "undefined") [] l

(* Note that you could *also* just write
   
   If you don't understand why this works, ASK!
 *)
let filter = fold (fun acc next -> failwith "undefined") []

(* Now (using fold) define a function filter, that accpets an
   arbitrary predicate f (a function from 'a -> bool), and removes
   from a list `l` every element e, for which (f e = false)
*)
let filter f l = failwith "undefined"

(* 
   Our version of `fold` works like this:
   (f (f (f i x1) x2) x3)

   Define a function `fold_right` that does this:
   (f x1 (f x1 (f x3 i) ) ) 

   Note that it will not be tail recursive.
 *)

(* Make the following functions tail recursive *)

let rec raise_x_to_the_n x n =
  if (n = 0) then
    1
  else 
    x * (raise_x_to_the_n x (n-1))

let rec multiply_each_element_by_two l = 
  match l with
  | [] -> []
  | hd::tl -> (2 * hd) :: (multiply_each_element_by_two tl)



