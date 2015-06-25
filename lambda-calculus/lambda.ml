(* 
   CMSC 330, Summer 2015
   
   Lectures on the Lambda calculus.
 *)

module L = List

(* -------------------------------------------------------------------------- *)
(* Introduction and defining lambda terms                                     *)
(* -------------------------------------------------------------------------- *)

(* Like turing machines, the lambda calculus is a primitive model of
   computation.  That is to say, any function that is computable
   (whose result can be computed via a mechanical means) can be
   computed with the lambda calculus.

   We start our exploration in programming languages with the lambda
   calculus because, unlike turing machines, the lambda calculus takes
   functions as the main abstraction.  We will see that this lends to
   a nice formalization of functional langauges, but for now we'll
   have to trust that that's what we're building towards.

   First we define the so called terms (programs) of the lambda
   calculus.  We assume there is an infinite set of variables V from
   which we can draw variables.

   t ::= 
      | x        <-- If x is a variable, then x is a term
      | (\x. t)  <-- If x is a variable, and t is a term, then \x. is a term
      | (e1 e2)  <-- If e1 and e2 are both terms, then (e1 e2) is a term 

   Conceptually, the lambda calculus term (\x. t) can be thought of
   as the OCaml term (fun x -> t).
   
   (e1 e2) is the *application* of e1 to e2.
*)

(* Let's define an OCaml datatype for lambda terms.  First, we will
   simply say that variables are strings: *)

type var = string 

(* Next we will inductively define lambda terms using OCaml. *)
type term =
  | Var of var         (* If x is a variable, then Var x is a term             *)
  | Lam of var * term  (* If x is a variable and t a term, Lam (x,t) is a term *)
  | App of term * term (* If t1 and t2 terms, App (t1,t2) is a term            *)

(* Note that we don't explicitly parse lambda terms.  Instead, we work
   with their OCaml representation.  For many purposes, working with
   the concrete syntax of lambda terms will just get in the way:
   there's no need to talk about parsing when we talk about how to
   perform computations. 

   When we work with the OCaml representation of a term, such as 
*)

let ex_0 : term = Lam("x",Lam("y",Var "x")) 

(* -------------------------------------------------------------------------- *)
(* Printing lambda terms                                                      *)
(* -------------------------------------------------------------------------- *)

(* 
   Rather than the concrete term:
   
   (\x. (\y. x))

   We say that we are working with the **abstract syntax** of the
 programming langauge.  Working with the abstract syntax of the
 programming language is great, because it means that most of the
 theory we want to define can really just be OCaml functions on
 datatypes.

  Because they are just OCaml datatypes, let's define a function to
  render lambda terms as strings.
*)

(* Let's define a function which will traverse a term and turn it into
   a string representation. *)
let rec string_of_term = function
  | Var x -> x
  | Lam (x,t) -> "(\\" ^ x ^ ". " ^ (string_of_term t) ^ ")"
  | App (t1,t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"

let pterm = fun x -> print_endline (string_of_term x)

(* And a few examples... *)
let x = "x"
let vx = Var x
let y = "y"
let vy = Var y
let ex_1 = Lam(x,App(vx,vx))  (* Note that we have to wrap "x" in a
                                 Var constructor here. *)
let ex_2 = App(ex_1,ex_1)

(* Now we can see our function in action:

    # pterm ex_2;;
    ((\x. (x x)) (\x. (x x)))
*)

(* -------------------------------------------------------------------------- *)
(* Reducing lambda terms                                                      *)
(* -------------------------------------------------------------------------- *)

(*
   How do we compute with the lambda calculus?  Well, we have a set of
   rules that we can use to rewrite terms into other terms.  If you
   think of computation in the lambda calculus as a game: we have
   various moves we can play.

   To start things off, I'm going to list what I see as the main
   source of "computation" in the lambda calculs, β reduction:

   `(\x. e1) e2` β-reduces to `e1 { x |-> e2 }`

   In other words, the result of applying (\x. e1) to e2 is e1, where
   we replace all the x's with e2.

   β reduction tells us how to apply functions.  It's just what we'd
   think it is: substitute the argument of the function into the body.
   It's just like what you did in elementary algebra:

   f(x) = x*x + 2, plug in 3 for x, get 3*3 + 2 out.

   Let's define β reduction as an OCaml function:

   let beta_reduce = function
     | App(Lam(x,e1),e2) -> substitute x e2 e1
     | _                 -> failwith "can't beta reduce"

   First, we need to know how to substitute values in the lambda
 calculus. This seems like it should be simple, let's look at a few
 examples:

   (\x. x x) (\y. y)

   To beta reduce this term, we need to replace x with `(\y. y)` in
   the term `x x`.  So, we do the obvious thing: the first and second
   x's get replaced with `(\y. y)`:

   (\x. x x) (\y. y) --> x x { x |-> (\y. y) } = (\y. y) (\y. y)

   In other words, to replace x with some term t, all we do is take
   all occurences of the variable x, and replace them with t.  We
   could code that up like this:
*)
let rec broken_substitute x t e = match e with
  | Var y -> if (x = y) then t else e (* If we have `y` and are trying to find `x`,
                                         don't replace it. Leave it alone. *)
  | Lam(y,e1) -> Lam(y,broken_substitute x t e1)
  | App(e1,e2) -> App((broken_substitute x t e1),(broken_substitute x t e2))

(* First, let's ask ourselves, why didn't we change the thing under
   the lambda?  E.g., why didn't we say: 

     Lam(broken_substitute x t y,broken_substitute x t e1)

   The answer is this: the thing that the lambda is binding is a
   string, not a term.  In the lambda calculus, it would be
   nonsensical to say this:
   
   (\x. (\x. x)) (\y.y) --> ((\y.y). (\y.y))

   Because that's not a well formed term.  The thing before the dot
   has to be a variable name, **not** a term.
*)
                    
(* -------------------------------------------------------------------------- *)
(* Bound / free variables / Capture avoiding substitution                     *)
(* -------------------------------------------------------------------------- *)

(*
   Our current technique is broken, let's see why:
   
   (\x. x (\x.x)) (\y.y)
   ^------------^ ^----^
         e1         e2

   Let's say I want to beta reduce the top level expression.  In other
   words, I want to apply `e1` to `e2`.  Let's see what happens if we
   just use our technique:

                                             (\x. x (\x. x)) (\y.y)
    beta reduces to                    -->   x (\x. x) { x |-> (\y.y) }
    and we use broken_substitute to get...   (\y.y) (\x.(\y.y))

    But this is **bad**.  Why?  Because we shouldn't have replaced the
    second x.  We should have left it alone.  It was rebound by the
    inner `(\x.x)`.
   
    Let's think about this by analogy to the OCaml term:
   
    (fun x -> (fun x -> x)) 12

    When I want to compute that, what do I do?  Well I plug in 12 for
    x.  But then I see a fun binding x in the inner term.  This
    rebinds x, meaning I should leave it alone.

    In general, this is called variable *capture*.  This is closely
    related to the concept of *free* variables.  Free variables are
    variables that aren't bound by a lambda. In the following
    expressions...
   
       (\x. y)           <-- y is a free variable (we sometimes say "y is free")
       (\x. \y. z)       <-- z is free
       (\x. \y. x y)     <-- Nothing is free (x and y are both bound)
       (\x. (\y. y) y)   <-- The **last** y is free

   It's worth talking about the last example.  Let's think about it by
   analogy to OCaml code:
   
   (fun x -> 
     ((fun y -> y) y))
                   ^
                   |
              This is the free one

   If you try to run that code, what happens?  OCaml will complain at
   you.  It will say, I don't know what the value of y is for that
   variable.

   If you ever get confused, remember that you can turn these lambda
   expressions into OCaml terms and then think about in OCaml.

   Now, let's define the free variables for a term: I'm going to treat
   lists as sets, so I need some helper functions:
*)

(* uniq makes a list uniq. *)
let uniq = L.fold_left (fun acc hd -> if (not (L.mem hd acc)) then hd::acc else acc) []
                    
(* Takes the union of two lists. *)
let union l1 l2 = uniq (l1@l2)

(* l1 \ l2 ... [1;2;3] \ [2;3] = [1] *)
let difference l1 l2 = L.fold_left
    (fun acc hd -> if (L.mem hd l2) then acc else hd::acc) [] l1

(* Removes an element from a list. *)
let remove l1 x = L.fold_left (fun acc hd -> if (hd = x) then acc else hd::acc) [] l1

let rec free_variables t = match t with 
  | Var x -> [x]
  | Lam (x,t) -> remove (free_variables t) x
  | App (t1,t2) -> union (free_variables t1) (free_variables t2)

(* Let's try it on some example terms... *)
let ex_3 = App(vx,vy)

(*
# free_variables vx;;
- : var list = ["x"]
# free_variables ex_1;;
- : var list = []
# free_variables ex_2;;
- : var list = []
# free_variables ex_3;;
- : var list = ["y"; "x"]  
 *)

(* We can calculate the bound varibles as the variables, minus the
   free variables. *)
let bound_variables t =
  let rec variables = function
    | Var x -> [x]
    | Lam (x,y) -> x :: (variables y)
    | App (t1,t2) -> uniq ((variables t1)@(variables t2))
  in
  difference (variables t) (free_variables t)

(* Now, this has been a bit of a diversion.  The reason we're messing
   around with this free and bound variable stuff is to fix our broken
   definition of substitute.  And we need that so we can define beta
   reduction.

   What's a simple fix to avoid capturing variables?  Well, when we
   substitute x for t inside an expression (\x. e), we first check to
   see if x is *free* inside of (\x. e).  This will happen when we
   have (e.g.,) `(\x. (\x. x) x)`.  In this case, we need to convert
   the x to a new variable that isn't the set of variables.
*)
  
(* -------------------------------------------------------------------------- *)
(* Alpha conversion                                                           *)
(* -------------------------------------------------------------------------- *)

(* Renaming variables inside expressions is called alpha conversion.

   For example, consider the expression:
   
   (\x. x x) (\y. y y)
   ^-------^
      e1 

   Inside e1, we can rename x to z, and we get an equivalent term:
   
   (\x. x x) (\y. y y) alpha converts to (\z. z z) (\y. y y)

   This will work, except for in one case: when the thing we're trying
   to alpha convert is *free* inside the expression.  For example,
   consider the following expression:
   
   e = (\x. y) 

   y is free inside of `e`.  So if we apply another term to it, it
   should simply beta reduce to y:
   
   (\x. y) z --> y
   
   But if we change x to `y`, that doesn't happen:
   
   (\y. y) z -> z

   This is bad, so we don't allow alpha conversion when a variable is
   free.

   We also have to take some care with how we alpha convert.  E.g., in
   converting the following expression:
   
       (\x. (\x. x) x)
   
   We can alpha convert to 
   
       (\y. (\x. x) y)
   
   But *not* 
   
       (\y. (\x. y) y)
*)

let rec improved_substitute x t e = match e with
  | Var y -> if (x = y) then t else e
  | Lam(y,t') ->
    if (x = y) then e
    else 
    if (not (L.mem y (free_variables t))) then
      Lam(y,(improved_substitute x t t'))
    else
      failwith ("error: " ^ y ^ "would be captured by substitution")
  | App(t1,t2) -> App((improved_substitute x t t1), (improved_substitute x t t2))
let substitute = improved_substitute

(* This still isn't quite perfect.  In the second case, where we
   currently raise an exception, we could instead rename the variable
   `y` inside t' to a fresh variable.  Doing this relies on a
   subroutine, `fresh`, which generates a variable not in `t`. *)

(* Exercise (2/3 stars)

   Implement `fresh`, which traverses a lambda term and generates a
   variable not in the set of variables.  One possible implementation
   might simply form variables of the form f<number>, and then search
   for it systematically.  This is not particularly efficient,
   however.

*)

(* Exercise: using fresh, extend improved_substitute to account for
   the final case, substituting with a fresh variable and renaming
   appropriately. *)

(* -------------------------------------------------------------------------- *)
(* Defining beta reduction                                                    *)
(* -------------------------------------------------------------------------- *)

(* Now implementing beta reduction is simple: *)

let beta = function
  | App(Lam(x,e1),e2) -> substitute x e2 e1
  | _                 -> failwith "can't beta reduce"

(* Note that beta reduction can be applied in various places within a
   lambda term.
   
   E.g., consider the following lambda term:
   
   ( (\x. x) (\x. x) )  ( (\y. y) (\y. y) )
   ^-----------------^  ^-----------------^
           e1                   e2

   We can beta reduce e1, *or* e2.  To allow us to implement this
   choice, we will annotate lambda terms with indices.
   
   As an example, pti produces the following for the aforementioned term:

    # pterm (App(App(x,x),App(x,x)));;
    (((\x. x) (\x. x)) ((\x. x) (\x. x)))
    # pti (App(App(x,x),App(x,x)));;
    0(1(2(\x. x) 3(\x. x)) 4(5(\x. x) 6(\x. x)))

   Now, we can choose to either beta reduce at index 1, or at index 4.
   None of the other indices are places where beta reduction could
   apply (they're just lambdas).
*)
let print_term_indices t =
  let rec h i t = 
    match t with
    | Var x -> (x,i)
    | Lam (x,t) ->
      let (a,i') = h (i+1) t in
      ((string_of_int i)^ "(\\" ^ x ^ ". " ^ a ^ ")",i')
    | App (t1,t2) ->
      let (a,i') = h (i+1) t1 in
      let (b,i'') = h (i') t2 in
      ((string_of_int i) ^ "(" ^ a ^ " " ^ b ^ ")",i'')
  in
  fst (h 0 t)

let pti = fun x -> print_endline (print_term_indices x)

(* Now, given a *)

let pp = Printf.printf

let beta_at_index t index = 
  let rec h i t = 
    match t with
    | Var x -> (t,i)
    | Lam (x,t) -> 
      let (a,i') = h (i+1) t in
      (Lam (x,a),i')
    | App (t1,t2) ->
      if (i = index) then
        (beta t,i+1)
      else
        (let (a,i') = h (i+1) t1 in
         let (b,i'') = h (i') t2 in
         (App(a,b),i''))
  in
  fst (h 0 t)

let bai = beta_at_index

(* -------------------------------------------------------------------------- *)
(* The Church-Rosser Theorem                                                  *)
(* -------------------------------------------------------------------------- *)

(* The lambda calculus is like a game.  Beta-reduction gives us a set
   of moves we can make.  But just like chess, we can't actually play
   the game unless we sit at the board and make decisions about what
   moves to make.

   Our choices in the lambda calculus are places where we might make
   various lambda reductions.
   
    # pterm (beta_at_index (App(App(x,x),App(x,x))) 1);;
    ((\x. x) ((\x. x) (\x. x)))
   
   Note that we also could have reduced at index 4: 

    # pterm (beta_at_index (App(App(x,x),App(x,x))) 4);;
    (((\x. x) (\x. x)) (\x. x))

   We might worry: are we making the *right* choices?  When there are
   multiple places that we can perform a beta reduction, how do we
   know which to choose?  The Church-Rosser theorem tells us that it
   doesn't matter.  No matter which choices we make, if we end up with
   a result, we could have ended up there making a different series of
   choices.

   **Theorem: Church-Rosser**

   From Wikipedia...

   > If there are two distinct reductions or sequences of reductions
   > that can be applied to the same term, then there exists a term
   > that is reachable from both results, by applying (possibly empty)
   > sequences of additional reductions.
   
   This is sometimes stated as the diamond property.  Let's see why: 
   
                     1((\x. x) (\x. x)) 4((\x. x) (\x. x))
                             /                   \
                       Reduce 1 first       Reduce 4 first
                           /                       \
            ((\x. x) ((\x. x) (\x. x)))   (((\x. x) (\x. x)) (\x. x))            
                        |                            |
                  (\x. x) (\x. x)             (\x. x) (\x. x)
                                 \           /
                                    (\x. x)

   See how the reductions form a diamond?  The idea is that it doesn't
   matter which one you choose.
*)

(* -------------------------------------------------------------------------- *)
(* Nonterminating programs                                                    *)
(* -------------------------------------------------------------------------- *)

(* It's possible to write nonterminating programs in the lambda
   calculus too.  Consider the following term `omega`: *)
let omeg = Lam("x",App(Var "x",Var "x"))
let omega = App(omeg,omeg)
    
(* Omega never terminates.  It loops forever.  

   Exercise: beta reduce omega.  Convince yourself it will never
   terminate.

   Exercise: Think about this, what does the Church-Rosser theorem say
   about the omega term?
*)

let s = "s"
let vs = Var "s"
let z = "z"
let vz = Var "z"
let w = "w"
let vw = Var w

let zero = Lam(s,Lam(z,vz))
let one = Lam(s,Lam(z,App(vs,vz)))
let two = Lam(s,Lam(z,App(vs,App(vs,vz))))

let s = Lam(w,Lam(y,Lam(x,(App(vy,(App(App(vw,vy),vx)))))))
