(* 
 * DFAs in OCaml
 *)

(* Let's make a module for representing deterministic finite automata
   in OCaml.  We're going to see that the mathematical definition of a
   DFA, given by Wikipedia, is going to lead to a very simple
   implementation of DFAs in our setting. 

   First, let's think back to what DFAs were, when we introduced them
   formally. Wikipedia says:

     A deterministic finite automaton M is a 5-tuple, (Q, Σ, δ, q0,
     F), consisting of
      - a finite set of states (Q)
      - a finite set of input symbols called the alphabet (Σ)
      - a transition function (δ : Q × Σ → Q)
      - a start state (q0 ∈ Q)
      - a set of accepting states (F ⊆ Q)
   
   Now, let's translate this into OCaml!
*)

(* We're going to say states are just numbers.  This means that our
   set q is going to just be a set of integers.  We're just going to
   represent this as a list of integers.  For example, in [this
   machine](https://en.wikipedia.org/wiki/Deterministic_finite_automaton#/media/File:DFA_example_multiplies_of_3.svg),
   the set of states is just going to be represented (by us) using the
   list [0;1;2]
*)

(* The following line creates a type *alias*.  It tells the ocaml
   compiler to, whenever it sees `state`, treat that type as int.  Why
   not just use `int` everywhere?  Well, what happens later if we want
   to refactor our code?  We might want to change the definition of
   this type to (e.g.) string.
*)
type state = int

(* For our symbols, we're going to just use ocaml's built in char type *)
type symbol = char

(* To represent a transition function, we're actually going to represent a
   table.  The table is going to tell us where to go on a given input
   state q, and a given symbol s.

   ----------------|--------------|----------------
   | Current state | input symbol | Next state    |
   |---------------|--------------|---------------|
   |      0        |      '1'     |       1       |
   |      0        |      '0'     |       0       |
   |      1        |      '1'     |       1       |
   |      1        |      '0'     |       0       |
   |---------------|--------------|---------------|

   We're going to represent this as a list of *tuples* in OCaml, which
   generalize pairs.  Remember that a pair type is something of the
   form `'a * 'b` where 'a and 'b are any type (we call them type
   variables).  A triple is created simliarly: 'a * 'b * 'c
*)

type transition = int * symbol * int

(* Now we can literally translate the wikipedia definition of what a
   state machine is:
*)
type dfa_attempt = state list * symbol list * state * transition list * state list

(* Here's an example dfa *)
let d : dfa_attempt = 
  ([0;1],           (* State list *)
   ['0';'1'],       (* Alphabet *)
   0,               (* Start state *)
   [(0,'0',0);      (* transition 1 *)
    (0,'1',1);      (* transition 2 *)
    (1,'0',0);      (* transition 3 *)
    (1,'1',1)],     (* transition 4 *)
   [1])             (* Accepting states *)

(* This is all fine and well, but to access the set of states, we have
   to break apart the dfa.  It will help to write some accessor functions. *)
let states (s:dfa_attempt) = match s with
  | (s,_,_,_,_) -> s (* We use wildcards here, because we don't care about the
                        other components. *)

let transitions ((_,_,_,t,_):dfa_attempt) = t

(* Instead, there's another tool I can use.  I can use the record
   notation.  Records are similar to C structs: they allow me to group
   common information, and then name the fields with sensible labels
   so that I can use them in my programs later.

   https://realworldocaml.org/v1/en/html/records.html

   Let's define our DFA type using the record notation.
*)

type dfa = 
  { 
    states : state list;
    sigma : symbol list;
    start : state;
    transitions : transition list;
    accepting : state list;
  }

(* Here's an example DFA *)

let d : dfa = 
  { states = [0;1];
    sigma = ['0';'1'];
    start = 0;
    transitions =
      [(0,'0',0);
       (0,'1',1);
       (1,'0',0);
       (1,'1',1)];
    accepting = [1]
  }

(* To dereference a record, I use the .field notation *)
let states (dfa : dfa) = dfa.states

(* This is a function that takes in a DFA as input, and adds a transition. *)
let addTransition t dfa = { dfa with transitions = t::dfa.transitions }

(* Now we're going to define a function that lets us *run* a DFA on an input
   string.  This is going to be our trickiest example yet, so make
   sure you think through it to see what each piece is doing.
*)

(* We're going to define two helper functions. *)

(* `explode` takes a string `s`, and turns it into its individual
   characters.  This way we can run the DFA on the string "101"
   without explicitly writing ['1';'0';'1']
*)
                          
let explode s =
  (* Note that we define expl as a *helper* function.  Helper functions are very
     useful in functional programming, because they help us build larger
     programs from programs that operate on smaller items.  Not ethat
     the definition of `expl` is only visible *inside* of `explode`.
     It's a local function (local to explode), so you can't call it
     outside.  *)
  let rec expl i l =
    if i < 0 then l else
      expl (i - 1) (s.[i] :: l) in  (* s.[i] returns the ith element of s as a char *)
  expl (String.length s - 1) [];;   (* String.length s returns the length of s      *)

(* Let's reflect on how `explode "110"` is working.  First, explode
   will call `expl 2 []`.  Let's calculate that using the equation for
   it!

   expl 2 [] = 
     if (2 < 0) then [] else
       expl 1 ("110".[2] :: [])
     =                               (because if test false)
     expl 1 ("110".[2] :: [])
     =                               (definition of "110".[2] and ::)
     expl 1 (['0'])
     =
     if (1 < 0) then ['0'] else
       expl 0 ("110".[1] :: ['0'])
     =                               (because if test false, and defn of .[1] and ::)
     expl 0 (['1','0'])
     = 
     if (0 < 0) then ['1','0'] else
       expl -1 ("110".[0] :: ['1','0'])
     =     
     expl -1 (['0','1','0'])         (because if test false, and defn of .[0] and ::)
     = 
     if (0 < 0) then ['1','1','0'] else
       expl -1 ("110".[-1] :: ['1','1','0'])
     =                               (because if test true)
     ['1','1','0']

   This is how evaluation is happening in OCaml.  You unwind function
   definitions until you get to a case where the recursion stops, or
   "bottoms out."

   This function is actually *tail* recursive.  Which is something we'll talk
   about next lecture.
*)

(* Here's another helper function, that checks whether a list contains an element *)
let rec contains e l = 
  match l with
  | [] -> false
  | hd::tl -> if hd = e then true else contains e tl

(*
   Now, on to checking DFA acceptance.
   
   First, let's think about how we might run a DFA on paper, or in
   Ruby.  If I did it, I might keep a (mutable) variable that keeps
   track of what state I'm currently at, and then updates the state
   depending on that.

   Instead of doing that, I'm just going to write a function that tells me
   what state to go to *next* on an input.  I'm going to call this function
   `transition state input`.  In the formal definition of DFAs it's
   simply called δ.  But in our implementation of DFAs, it's a
   `transition list`.  We need to write a helper function that takes that
   transition list and turns it into a transition function.

   Let's say my input string is "110".

   How do I run the DFA?  Well, I:
    - Start at the beginning state, dfa.start
    - Call `transition dfa.start 1` and move to some state q2
    - Call `transition q2 1` and move to some state q3
    - Call `transition q3 0` and move to some state q4

   And how do I know if the DFA is accepting for that state or not?
   Well, I simply check to see if that state is contained in the set
   of accepting states.

   Now, the procedure I wrote above doesn't necessarily look obviously
   recursive.  But what happens when I write it like this:

        (transition 
           (transition
              (transition dfa.start '1') 
              '1')
           '0')

*)

let checkAccepts str dfa = 
  (* Get the list of symbols. *)
  let symbols = explode str in
  
  (* If I'm at state {state}, where do I go on {symbol}? *)
  let transition state symbol = 
    let rec find_state l = 
      match l with
      | (s1,sym,s2)::tl ->
        if (s1 = state && symbol = sym) then 
          (* I've found the place in the table where I've got what I'm
             looking for.
             
             E.g., in 
             ----------------|--------------|----------------
             | Current state | input symbol | Next state    |
             |---------------|--------------|---------------|
             |      0        |      '1'     |      *1*      |  <-- here
             |      0        |      '0'     |       0       |
             |      1        |      '1'     |       1       |
             |      1        |      '0'     |       0       |
             |---------------|--------------|---------------|

             If I called `transition 0 '1'`, I would be at the place
             marked `here`, and would return 1.  In OCaml this is
             represented as a list of triples, so I return the third
             element
          *)
          s2
        else
          (* Otherwise I continue my search.  This is the case where,
             in the above example, I look for `transition 1 '0'`, but
             I'm at "here." I know I haven't found the place in the
             lookup table yet, so I keep going *)
          find_state tl
      | _ -> failwith "no next state"
    in
    find_state dfa.transitions
  in
  
  (* Now I'm going to define `run_dfa`, which is going to do the
     following:
     
         (transition 
           (transition
              (transition dfa.start '1')               ( **line 3** )
              '1')
           '0')

     But it's going to work with any string, which is the list in
     `symbols`.  Now, I'm going to do a trick: I'm going to recurse on
     the *reversed* list.  To do this I'm going to define a helper.

  *)
  let final_state = 
    let rec h symbol_list = 
      match symbol_list with
      (* Case where list contains only one element *)
      | [hd] -> (transition dfa.start hd)              (* Corresponds to line 3 above *)
      | hd::tl -> (transition (h tl) hd)
      | _ -> failwith "empty list of symbols"          (* assume at least one symbol *)
    in
    h (List.rev symbols)                              (* I use the List library here. *)
  in

  (* Now I simply check to see if the final state is contained in the
     set of accepting states. *)
  if (contains final_state dfa.accepting) then 
    true 
  else 
    false

(* Now, let's reflect very carefully on how I did this.  First, I
   wrote out the recursion I wanted to happen.  Then I thought about
   how I could write a function that preserves that structure.  Occasionally,
   I'll find that I can't obviously do it.  For example, consider how
   the recursion would have worked if I had **not** reversed the
   list.  I would have gotten the wrong result!  

   This is a common pattern in functional programming: write an example
   equation you'd like to write, and then figure out how to get it to
   work.  It's really very similar in mechanics to the algebra you
   likely did in high school.  You sit down with the equations, and
   you figure out how to write a program that solves them.

   The part you need to practice at, and the part you get *better* at,
   is writing programs that are small and obviously correct.
   As an example, here's another way I could have written the last few
   lines of that function.  I could have defined a function:

   let rec search_from current_state symbol_list =
     match symbol_list with
       | [] -> current_state
       | sym::tl -> search_from (transition current_state sym) tl
   in
   let end_state = search_from dfa.start symbols in
   if (contains end_state dfa.accepting)
   then 
     true
   else 
    false

   Now I don't have to reverse the list, I accumulate the current
   state into the function.  

   Note that this is very similar to if I had written a function in Ruby that
   did something like:
   
   current_state = init
   for i in str { |x| ... update current_state }

   I've transformed what was (in Ruby) a program variable, and I've
   taken that to pass it along through the program.  This technique is
   sometimes called "threading the state through" the program.  And in
   fact, any program where I use some notion of state, I can
   systematically rewrite to thread the state through functions.
   
   Here's another example:
*)
      
(* In ruby:
   x = 0   
   [1,2,3].each { |y| x += y }
   
   But in OCaml:
*)

let sum lst = 
  let rec helper currentSum lst = match lst with
    | [] -> currentSum
    | hd::tl -> helper (currentSum + hd) tl
  in
  helper 0 lst

