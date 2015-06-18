(*
 * Exploring the OCaml type system.
 * 
 * CMSC 330, June 18, 2015
 *)

(*--------------------------------------------------------*)
(*  Introduction                                          *)
(*--------------------------------------------------------*)

(* In the past few lectures you've seen me use types for OCaml
   functions, and might be slightly mystified by what they are.  Today
   we're going to explain and attempt to justify some of OCaml's type
   rules, and explain how to make things like polymorphic datatypes.

   Let's start out by giving an example rule OCaml enforces:
*)

let hd l : int = match l with 
  | [] -> 0
  | hd::tl -> hd

(* This is the familiar head function, for integer lists.  It's type
   is `int list -> int`, meaning it takes a list (containing
   integers), to an integer.

   A type is like a (legal) contract between your program and you.  It
   says, if your program compiles without giving any type errors, it's
   not *ever* going to throw a type exception at runtime.

   Why is this good?  Well, let's think about what would happen if we
   had erroneously defined `hd` this way:
   
  let hd l : int = match l with
    | [] -> []
    | hd::tl -> hd
   
   Let's think about what would happen if we then tried to do this:
   
   print_int [1;2;3] 
   (ocaml prints `1`)
   
   but then...
   
   print_int []

   If OCaml didn't enforce type rules, the program would crash.
   Because print_int only knows how to print integers.  It doesn't
   know how to print ints.

   Consider the following Ruby code:

   def print_int(i)
     puts (i+0)
   end
   def head(l)
     if (l.length == 0) then [] else l[0] end
   end

   and then...

   2.0.0-p0 :008 > print_int(head([1,2,3]))
   1
    => nil
    2.0.0-p0 :009 > print_int(head([]))
    TypeError: no implicit conversion of Fixnum into Array
	from (irb):2:in `+'
	from (irb):2:in `print_int'
	from (irb):9
	from /Users/micinski/.rvm/rubies/ruby-2.0.0-p0/bin/irb:16:in `<main>'

   So, OCaml let's us avoid these kinds of type errors!

   See what happens yourself when you try to define the previous
   function using OCaml:
   
    # let hd l : int = match l with
      | [] -> []
      | hd::tl -> hd
    ;;
    Error: This expression has type 'a list
       but an expression was expected of type int   
*)

(*--------------------------------------------------------*)
(*  Defining type safety                                  *)
(*--------------------------------------------------------*)

(* A type system for a programming language is **sound** if well-typed
   programs do not generate type errors at runtime.

   OCaml's type system is sound.  If a program compiles, you can be
   **sure** it will never blow up at runtime because of a type error.
   Note that it may still crash because of (e.g.,) an exception (e.g.,
   a match that isn't total).

   Programming languages where programs do not generate type errors
   are typically called type safe programming languages.
*)

(* Let's look at some of OCaml's type rules.  We've seen one just
   now:
   
   - Rule: all cases for a match must return the same type.
   
   Let's look at some more basic rules:
   
   - Integer literals have type `int`.
   - Float literals have type `float. 
     - etc.. for all basic types `bool`, `string`, etc...
 *)

let xint : int = 23
let xfloat : float = 23.0
let xstring : string = "23pointzero"

(* Let's look at the rule for matching, more formally
   
   In the expression e =
   match x with 
     | pat1 -> e1
     | ...  -> ...
     | patn -> en
   
   if  e1 has type 'a
   and ... has type ... 
   and en has type 'a

   then the expression e has type 'a.

   This is an *inductive* rule.  It allows us to build bigger
   expressions out of smaller assumptions (assumptions about
   constituent types of en).
*)

(*--------------------------------------------------------*)
(*  Type variables                                        *)
(*--------------------------------------------------------*)

(*   
   See that 'a?  It's called a type variable.  We typically pronounce
   it alpha (but write it 'a so we can type in ASCII).  The idea is
   that we could put any type there for 'a.  But when we use the
   *same* type variable 'a in our type equation, we implicitly mean
   that all the `'a`'s are *equal*.  This means a concrete instance of
   that rule would be:

   if  e1 has type int
   and ... has type ... 
   and en has type int
   
   then e has type int.

   Type variables are similar to quantifiers from logic.  Remember
   back in our logic class when we had the formula:
   
   forall x. x + 1 - 1 = x

   If we wanted to *use* that fact, we had to provide a concrete value
   for `x`.  So, that formula really stood for a bunch of statements:
   
     0 + 1 - 1 = 0 (x = 0)
     ...
     n + 1 - 1 = n (x = n)
     ...
   
   It's the same thing here.

*)

(*--------------------------------------------------------*)
(*  More type rules                                       *)
(*--------------------------------------------------------*)


(* Let's look at the rule for functions:

   if function f has type 'a -> 'b, and x has type 'a, then `f x` has
   type 'b.  Sometimes we will write typing judgements in this style:

   f : 'a -> 'b
   x : 'a 
   -------------
   f x : 'b

   This is called an **inference rule**.  The way you read it is as
   follows: if everything *above* the line is true.  Then the thing
   *below* the line is true.

   You can view an inference rule like a recipe.  If I want to make an
   element of type 'b, I need something, f, of type `'a -> 'b`, and
   something, x, of type `'a`, and then if I apply f to x, I get an `f
   x` of type `'b` when I put them together.

   Here's an example:
*)

let f : int -> int = fun x -> x+1
let x : int = 23
let y : int = f x

(* The rule for constructors is similar to functions.  In OCaml, you
   can think of constructors like functions.  If I have a datatype:
*)

type hex = 
  | Hex of int * int * int (* e.g., Hex (255,0,0) *)
  | Name of string         (* e.g., Name "red"    *)

(* Then you can think of Hex as a function that has the following
   type:
   
     Hex  : int * int * int -> color
     Name : string          -> color

   Constructors in OCaml aren't functions, for a silly technical
   reason, but that's the right way to think of them.  However, you
   can *make* them functions by doing this:
   
   
*)

let hex (i1,i2,i3) = Hex (i1,i2,i3)

(* When I *destruct* (match on) an element datatype, I am allowed to
   assume the elements have their constituent types.

   In other words, of 
     type t = 
       | Ctr1 of 'a1 * ... * 'n1
       | ...
       | Ctrk of 'ak * ... * 'nk

   if x : t, and I have expression:n
   
   match x with 
     | Ctr1 (x11, ..., xn1) -> e1
     | ...
     | Ctrk (x1k, ..., xnk) -> en
   
   In e1, I know that x11 : 'a1, xn1 : 'n1, 
   ...
   In ek, I know that x1k : 'ak, xnk : 'nk.
   
   An example...
*)

let convert_to_string (h:hex) = 
  match h with
  | Hex (i1,i2,i3) -> "<I don't know...>" (* Here I am allowed to know i1 : int *)
  | Name s         -> s                   (* Here I am allowed to know s : string *)

(* Let's look at the rule for `let x = e1 in e2`.

   It says: 
     - Figure out what x's type is by checking e1, 
       - Call that type you figured out 'a 
     - Inside of e2, *assume* that e1 has type 'a, now figure out e2's type
       - Calling that type 'b
     - The result of `let x = e1 in e2` is 'b.

   Here's an example:
*)

let compute = 
  let x = ((fun x -> x) 23) in
  x + 2

(* How do we do this? 
   
   - Calculate what x's type will be.

     - (fun x -> x) is `'a -> 'a`.  When applied to `int`, `(fun x ->
       x) 23 : int`
    
   - Look at the code x + 2.  How do we typecheck it?  
     - *We need to know the type of x*
     - But we **just figured out** that it's int.

     - So take `x + 2`, which is `+ : int -> int -> int` applied to `x
       : int`, which results in `int -> int`, which is then applied to
       `2 : int`, which gives us back an int.

   We write this rule like this:
   
   e1 : 'a 
   x : 'a |- e2 : 'b       <<< Notice this line
   -----------------
   let x = e1 in e2

   Notice the highlighted line.  The notation `x : 'a |- e2 : 'b`
   means this:
   
       If we assume x : 'a *in* e2, then e2 has type 'b.

   We'll come back to this in the section on type systems, but it's
   helpful to see it now.
*)

(*--------------------------------------------------------*)
(*  Do we really need `let`?                              *)
(*--------------------------------------------------------*)

(* Having `let` allows us to write programs that give names to things:
*)

let dist (x1,y1) (x2,y2) =
  let e1 = x1 -. x2 in
  let e2 = y1 -. y2 in
  let e1squared = e1 *. e1 in
  let e2squared = e2 *. e2 in
  sqrt (e1squared +. e2squared)

(* But let's think about it, couldn't we have just written it like
   this?
*)

let dist (x1,y1) (x2,y2) =
  (fun e1 -> 
    (fun e2 -> 
      (fun e1squared ->
         (fun e2squared -> sqrt (e1squared +. e2squared))
           (e2 *. e2))
        (e1 *. e1))
      (y1 -. y2))
    (x1 -. x2)

(* It turns out we *can*.  `let` is just lambda in another form! *)

(*--------------------------------------------------------*)
(*  Polymorphism                                          *)
(*--------------------------------------------------------*)

(* 
   Let's go back to the way we defined binary trees:
 *)

type inttree = 
  | IntLeaf of int
  | IntNode of int * inttree * inttree

(* This works for trees that contain integers.  But what if we want to
   have a tree that stores floatint point numbers.

   We really want a tree that takes an arbitrary type 'a, and "plugs
   it into" the holes where `int` is in `inttree`.  In OCaml, we can
   create such a type by prefixing it's name with a type variable, and
   then use that type variable.
*)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

(* This is a parameteric type.  We've defined a type of trees that is
   parameteric in 'a.

   In other words, concrete instances of 'a tree are things like:
   
   int tree = 
      | Leaf of int
      | Node of int * int tree * int tree

   float tree = 
      | Leaf of float
      | Node of float * float tree * float tree

   Think of `tree` sort of like a function, that works on *types*.  If
   we give `tree` an `int`, it's going to make us a `tree int` type by
   filling in the holes in that type declaration.
*)

(* Here's another example type illustrating how to use multiple type
   variables, you have to put them in parenthesis and separate them
   with commas. *)
type ('a, 'b) pair =
  | Pair of 'a * 'b

(* Write a function that pulls out the first element of the pair. *)
let first (a: ('a,'b) pair) : 'a = 
  match a with
  | Pair (a,b) -> a

(* It's worth pointing out that, just as `2 * 3` is an infix form for
   something like `mul 2 3`, `int * int` is syntactic sugar for
   something like `(int, int) pair`.  Except now the syntactic sugar
   is working on *types*, rather than terms.
*)

(* Now, let's think about how we would write a function search
   (implementing binary search), for an arbitrary tree of type `'a
   tree`.  Our original implementation went like this:
*)

let rec search_inttree (a : inttree) (x : int) : bool =
  match a with
  | IntLeaf y -> x = y
  | IntNode (y,t1,t2) -> 
    if (x = y) then true
    else 
      if (x < y) then search_inttree t1 x
      else search_inttree t2 x
          
(* But I *can't* do that here.  Why not?  Well, think about the type
   of `<`.  It has type `int -> int -> bool`.  But when I have an `'a
   tree`, I need to have something that goes from `'a -> 'a -> bool`.

   The function `<` is *too strict* to work on an arbitrary `<`.

   Instead, I'm going to write a function search, that accepts a
   parameter `compare` that will give me:
     - compare x y < 0 when x lessthan y (for the type 'a) 
     - compare x y = 0 when x equalto  y (for the type 'a) 
     - compare x y > 0  when y lessthan x (for the type 'a) 
*)

let rec search (t : 'a tree) (compare : 'a -> 'a -> int) (x : 'a) = 
  match t with
  | Leaf y -> (compare x y) = 0
  | Node (y,t1,t2) -> 
    if ((compare x y) = 0) then true
    else 
      if ((compare x y) = -1) then search t1 compare x
      else search t2 compare x
  
(* Now, what happens when we make an `int tree`.  How do we define
   search to line up with our previous definition? 
*)

let search_int_tree (t : int tree) (x : int) = search t (fun x y -> x - y) x

(*--------------------------------------------------------*)
(*  Forcing types                                         *)
(*--------------------------------------------------------*)

(* Let's say that I want to come up with a function that has the type
   `'a * 'a -> 'a`. I might try this
*)

let f (a,b) = a

(* But OCaml will tell me that f has type `'a * 'b -> 'a`, rather than
   what I want: 'a * 'a -> 'a. 

   I can explicitly *force* them to be the same by adding a type
   annotation.

*)

let f ((a : 'a),(b : 'a)) : 'a = a

(* a and b are *forced* to have the same type by OCaml's type
   inference engine, because I have syntactically identified the two
   'a's. *)

(*--------------------------------------------------------*)
(*  Type inference                                        *)
(*--------------------------------------------------------*)

(* Now, let's talk a little bit on type inference to demystify what's
   going on. 

   We're not going to be super formal: we'll get into that later.
   We're just going to give a high level intuition about what type
   inference is, and why it works.
   
   Let's think back to the function we talked about yesterday, do_twice: 
*)

let do_twice f = fun x -> f ( f ( x ) ) 

(* If I go type that in at OCaml, it will give me this: 
   
     (1) val do_twice : ('a -> 'a) -> 'a -> 'a = <fun>

   Why did it give me that?  How did it compute it?

   First of all, let's start with this question.  `do_twice` could
   also have this type:

     (2) val do_twice : (int -> int) -> int -> int = <fun>

   But that type would be a more specific version of (1).  In general
   we have the following rule:

     OCaml will always infer the *most general possible* type for a
     function, unless otherwise constrained.

   This is why our previous example, `f`, had type `'a * 'b -> 'a`.
   It would be *possible* for it to have type `'a * 'a -> 'a`, but
   that wouldn't be the most *general* type it could be assigned.
   Having a most general type is important for reasons we'll discuss
   later (it will allow more programs to type check).

   But it's still unclear, what mechanism does OCaml use to decide
   what type something should be?
   
   Well, it uses a few rules:

     - If we see a literal like `1`, we know it's type is (e.g.,) `int`

     - If we're in an environment where someone has *told* us that `x
       : 'a`, we assume x has type 'a.  An example of this would be
       let binding.  (Doing type inference for let binding is
       difficult if we don't know 'a a priori, we'll discuss that
       later, but won't worry about it for now.)

     - If we see a variable used as a function, we know it has to have
       a function type.  E.g., if we think f's type is 'a, but we see
       it used in code as `f 1`, we know it can't be just `'a`, but
       has to be `'a -> 'b`.

   And use the type constraints we talked about (at the beginning of
   this lecture) to guide us in going to a solution.

   When the OCaml type inference engine sets out to ascertain a type
   for a piece of code, it does based on a mechanism that makes an
   initial guess, and then *refines* that guess.

   Remember that we talked about this mechanism?  It was similar to
   the way we computed DFAs from NFAs.  We start with an initial
   guess, and then we go through a process of refining that guess
   until we end up with an answer.

   We'll formalize this idea later in the semester, but let's show an
   example of how it works on dotwice:
   
   let do_twice f = fun x -> f ( f ( x ) ) 

   OCaml looks at this piece of code and it says the following:
   
   - I need to figure out types for x, and for f.

   - Let's start by assuming they're f : 'a and x : 'b.

   - Now, look, can f be of type 'a?  No!  Because it's a function, so
     it has to be of a functional type, let's call it `'a -> 'c`.

   - Now, we know that f has to accept x's type as input, so we know
     that f *actually* has to have type `'a -> 'c`.

   - Now, is this our final type?  Well almost, but see, we feed f's
     result back into itself. So let's think about f as a box:
   
             +-------+
      'a --->|       |---> 'c
             |   f   | 
             |       | 
             +-------+
        
      But now we're tying the output of `f` back into its input:

             +-------+
      'a --->|       |---> 'c
       |     |   f   |      |
       |     |       |      |
       ^     +-------+      |
       ---------------------<
   
       But now, what do we know, 'c *has* to be equal to 'a!

       So now, assembling the equations, we know that we end up with
   the type `('a -> 'a) -> 'a -> 'a`.
*)
