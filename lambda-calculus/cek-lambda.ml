(* 
   CMSC 330, Summer 2015
   
   Lectures on small step semantics and the CEK machine.
 *)

module L = List
module S = String

type var = string    (* Variable names *)
type name = string   (* Constructor names *)

(* In this week's lectures, we're going to explore another way to
   define meaning to programming langauges.

   Just to give ourselves a refresher, let's recall the way we've
   defined semantics so far...
   
*)

(* -------------------------------------------------------------------------- *)
(* Review: lambda calculus, big step semantics                                *)
(* -------------------------------------------------------------------------- *)

module Lambda = struct
  
  (* We start with the lambda calculus. The lambda calculus defines
     three basic forms for terms: 
     
     t ::= 
       | x 
       | \x. t
       | t1 t2
     
  *)
  type term =
    | Var of var               (* Variables *)
    | Lam of string * term     (* Lambda abstractions *)
    | App of term * term       (* Applications *)

  (* And a utility function for helping us print terms. *)
  let rec string_of_term = function
    | Var x -> x
    | Lam (x,t) -> "(\\" ^ x ^ ". " ^ (string_of_term t) ^ ")"
    | App (t1,t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"

  (* We remember that there were a few different ways we could
     transform lambda terms.  For example, we had alpha-conversion (or
     alpha-renaming), which allows us to rename variables.  We had to
     be careful to make sure that when we performed alpha renaming, we
     didn't cause variable capture (if we did, we would change the
     meaning of the term).
     
     For example, in the following expression:
     
       \x. x ((\x. x y) x)   -- alpha-convert x to z --> \z. z ((\x. x y) z)

     Remember that we didn't convert that second x, because it was
     rebound.

     Then remember that we had beta-reduction (β-reduction) to "call"
     functions, by substituting their arguments.
     
     (\x. x x) (\y. y) -- beta-reduces to --> (\y. y) (\y. y)
                       -- beta-reduces to --> (\y. y)

     We had lots of things to say about the lambda calculus, which you
     can read in the `lambda.ml` file.
     
  *)
             
  (* The lambda calculus is turing complete, meaning it can encode any
     computation we might want to do: all using lambdas, applications,
     and variables.  

     But we typically want some extra things added to the lambda
     calculus, we we define extended terms.  These include things like
     numbers, booleans, if/then/else expressions, etc...

     To define these we extend the term representation:
     
  *)
  type eterm = 
    | EVar of var
    | ELam of string * eterm
    | EApp of term * eterm  
    | ENum of int                         (* Numbers *)
    | EBool of bool                       (* Booleans *)
    | EIfThenEls of eterm * eterm * eterm (* if/then/else *)
  (* etc... *)
                   
end

(* 

  After we talkeda about the lambda calculus, we talked about big-step
  semantics.  The lambda calculus just gives us a bunch of *rules*
  that we can use to transform lambda terms.  But it doesn't tell us
  *how* to use these rules.  E.g., consider the term:
   
    ((\x. x x) (\y. y)) (\y. y)

   What should we do with this term?  One sensible idea is that we
   should beta-reduce it as far as we possibly can:
   
        ((\x. x x) (\y. y)) (\y. y) 
    --> ((\y. y) (\y. y)) (\y. y) 
    --> (\y. y) (\y. y) 
    --> (\y. y) 

   Now we're done, there are no more reductions we can make.  This is
   similar to the way you play many board games: you continue making
   moves from the start until you run out of possible moves, at which
   point the game is done.  However, just like a board game, the
   lambda calculus only gives us moves we can make, it doesn't tell us
   how to make those moves.  For example:
   
   ((\x. x) (\y. y)) ((\x. x) (\y. y))
   ^---------------^ ^---------------^
           e1               e2

   Should we evaluate e1 first?  Or e2 first?  Church-Rosser tells us
   that the choice doesn't matter.  But if we wanted to write a
   computer program to reduce lambda terms, we would have to make a
   choice.  
   
   We can define **values**, as a syntactic subset of lambda terms:

     v ::= 
       | x
       | \x. t
       
   As an example, the following are values:
   
     - y
     - \x. x
     - \x. (\y. x)
     - \x. (\y. x) x 

   Note that in the last example, there was an application under the
   lambda.  This is okay, all that matters is that the top level part
   is a lambda.

   You can view values in programming languages as the places where
   computation "stops."  For example, if I have the term:
   
       \x. (\y. x) x

   I don't know how to reduce it until I know the value for x.  I
   could beta-reduce `(\y. x) x` to `x`, but usually we have the
   following rule:
   
       Rule: No reduction is allowed under binders (lambdas)
*)

(* -------------------------------------------------------------------------- *)
(* Small step semantics and the CEK machine                                   *)
(* -------------------------------------------------------------------------- *)

(* 

   In the previous section, we evaluated the computation all in "one
   big step."  What do I mean by this?  Well, let's think about how we
   evaluate this term:
   
   ((\f.\x. f (x+1)) (\x. x*2)) 3
   ^---------------^ ^-------^^   
   |      e1             e2   |
   ------------ e3 ------------

   To evaluate the term, we:
     1st) evaluate e1 applied to e2, resulting in a value v': 
          (\x. (\x. x*2) (x+1))
     2nd) evaluate the argument to a value:
          3 is already a value, so nothing to do there.
     3rd) Apply v' to 3:
          (\x. x*2) (3+1)
     4th) Evaluate the result:
          (\x. x*2) (3+1) --> (\x. x*2) 4 --> 4*2 --> 8

   To evaluate the program, we do it all in one fell swoop.

   It might not be obvious, but there is an alternative, and the way
 we'll get intuition for it is to think about how our processors work.
 A processor operates using a clock, a signal that pulses very
 quickly.  On each pulse (edge), the processor does the following: 
   
   - Fetches the current instruction
   - Decodes the instruction 
   - Executes the instruction

   What are instructions?  To learn more about it, you can study
   assembly programming, but there are a few basic forms:

   - Move instructions: transfer data between registers (temporary variables)
   - Load / Store instructions: move data to memory (RAM)
   - Control instructions: jump to another part of the program, 
      - Possibly conditionally (for if/then/else)
   - Arithmetic instructions: to (e.g.,) add/multiply/etc...

   Modern processors get much more complex than this, but the basic
   building blocks include these instructions on each clock tick.  To
   build large programs, we structure them as sequences of
   instructions. On each tick, a small amount of work is performed,
   doing the rest of the work later.  For example, we might write
   something like this (in a made up assembly language):
   
   move 0 r0               # Load 0 into register 0 
   move 1 r1 
   move 2 r2
   add r0 r1 r1            # Add r0 and r1, leaving result in r1
   mul r2 r1 r3            # Multiply r2 by r3, leaving result in r3

   This notion of computation proceeds in steps, where the state of
   the machine evolves on each step.  We will define a semantics for
   the lambda calculus that also proceeds in steps.

   Our machine will have three pieces:

                           -------------------------------|
                           v                              |
                 < C , E , K >                            |
                   |   ^-------------------------------|  | 
   - The control: the currently executing instruction  |  | 
                                                       |  |
   - The environment: that stores the local variables--|  |
                                                          |
   - The continuation: that tells us where to go next ----|

   You can think of the continuation as the stack.  When we want to
   call a procedure in assembly language, we need to save the
   registers on the stack. We also need to save a pointer to the next
   instruction, so the computer knows where to jump to after it
   finishes executing that procedure.  We'll define precisely what
   these continuations look like in a few minutes, but for now, think
   of them as representing the stack.
   
   The machine state is going to evolve in steps, just like a game.
   We start in an initial state that looks like this:

      Σ = < e , [] , Done > 

   and our machine is going to perform multiple steps to get to an end
   state:

     Σ --> Σ' --> ... --> Σf

   Where Σf represents some "final" configuration for the machine,
   where it's computed its result and that's ready to return to the
   user.

   This is just like a game board: you set up the initial state
   (similarly to how you place the pieces on their assigned places in
   the beginning of chess or checkers), then you make moves, until you
   get to the end.  In our case, there is going to be one **unique**
   next state: the step function is going to be deterministic.  Our
   goal is to cook up a series of steps so that stepping through the
   machine is going to mirror big step evaluation of terms (that we
   covered in the last section).

   Let's jump back to explain what the initial configuration means:

     - e, the program we want to evaluate.  This means we want the
       machine to process the entire program.

     - [] is the empty map.  We haven't assigned to any variables yet,
       so the environment should be empty.

     - Done is the empty continuation.  When we finish executing the
       whole program we should just return the result.
   
*)

(* 
   
   A note from the instructor:

   Try not to get stuck too much on this analogy to processors.  It's
   really just to illustrate how these machines are working by proxy
   of something that you may have seen before.  The real reason we're
   doing small step semantics is that -- in studying the machinery
   required to build them -- we'll get experience understanding how
   programming languages work.  And once we have that machinery, we'll
   be able to easily add on a bunch of features like exceptions,
   concurrency, etc... that are hard to add on to big step semantics.

 *)

(* -------------------------------------------------------------------------- *)
(* A note on environments and motivating closures                             *)
(* -------------------------------------------------------------------------- *)

(* 

   Our usual mechanism for performing computation has been to perform
   substitution.  E.g., to evaluate:
   
   (\x. x (\y. y)) (\z. z)
   
   We substitute `x` with `(\z. z)` in the first expression:
   
       x (\y. y) { x |-> (\z. z) } 
     = (\z. z) (\y. y)

   But this substitution operation doesn't really mesh with our small
   step philosophy.  Why?  Because these lambda terms could be
   arbitrarily large!  To substitute them, we have to walk over the
   whole lambda term, replacing all occurences of `x` with the
   necessary form, becing careful to avoid capture and substitute
   appropriately, etc..  In other words, if we have the following
   expression:
   
       (\x. e1) e2

   Performing the substitution { x |-> e2 } won't be a constant time
   operation.  It will be linear time, at least (because we might have
   to alpha rename to perform capture avoiding substitution, it might
   actually be quadratic in some cases).  But this is very different
   than the processors I described above.  Those processors perform
   one step in constant time on each clock cycle.  How can we recover
   this performance in our machine?

   The answer is that we use an environment that tells us how to look
   up variables.  For example, let's consider how the following term
   will be reduced:
   
   (\f. f f) (\y. y)

   In the big step semantics, we'd substitute `f` with `(\y. y)`.  But
   I just said we don't want to do that here, because we'd be walking
   down the term `(\f. f f)`.  Let's assume that instead, it were
   cartoonishly large, like: 
   
   (\f. (((f f) (f f)) ((f f) (f f))) ((f f) (f f)))

   There, we'd be copying f all over the place, performing 12
   substitutions in one step.  Instead, we're going to keep an
   **environment** that tells us the value for `f`.  You can think of
   the environment like the local variables: they tell us the values
   of variables at the current point in time.

   So instead of calling `(\f. f f) (\y. y)` by substituting, we'll
   instead update the environment to contain { f |-> (\y. y) }, and
   then we'll execute `f`.  But now, we have to execute:
   
     f f

   But how we know what the value is for `f`?  The answer is that we
   have to look it up in the environment.  So, to compute 
   
     f f
   
   We first: 
      1) Compute the function, by looking up f in the environment:
         [ f |-> (\y. y) ] ( f ) = (\y. y)
      2) Compute the argument, by looking up f in the environment:
         [ f |-> (\y. y) ] ( f ) = (\y. y)

      3) Call `(\y. y)` with `(\y. y)`.  How do we do this?  We extend
      the environment so it also contains { y |-> (\y. y) }.  Then we
      see:

        y  with environment [ y |-> (\y. y), f |-> (\y. y) ]

      4) Evaluate `y` by looking it up inside the environment,
      resulting in..
      
         (\y. y)

   So, we have modified beta-reduction so that instead of
   substituting, we simply extend the environment.
   
   Now, let's see what happens when we do the following example:
   
   (\f. f 1) ((\x. (\y. x)) 2)
   ^       ^ ^               ^
   |   e1  | |      e2       ^
   
   To evalute this expression, we have to do the following:
   1) Evaluate e1 to a value, but it already is.
   2) Evaluate e2 to a value, 
     2') Evaluate the function to a value v1, but it already is
     2'') Evaluate 2 to a value, but it already is
     2''') Add {x |-> 2} to the environment and evaluate v1
     The result is `(\y. x)`
     Note that we **don't** get `(\y. 2)`!  Remember, we **aren't**
     substituting.  We only look up a variable from the environment
     when we evaluate that variable.
     >>>> This line will be important later on

   3) To evaluate the result of e2 applied to e1, we update our
   environment from the empty environment to [ f |-> (\y. x) ], then
   we evaluate `f 1`.  Question: why isn't our environment this:
       [ f |-> (\y. x) , x |-> 2 ]

   The reason is that after evaluating something, we throw away its
   environment.  Think about it like this, if you had the following
   function in C:
       int a() { int x = 0; return x; }
       int b() { int x = 1; int y = a(); return x}

   You would expect the result to be 1.  You **wouldn't** expect the
   result to be 0.  The x being modified in `b` is a local variable,
   that belongs to `b`, **not** to `a`.  It's a similar idea here.
   
   So now, we evaluate:
       f 1 with environment [ f -> (\y. x) ]

   And what do we do?  We first lookup f to get `(\y. x)`, and then we
   call it by assigning [ y -> 1 ] and evaluating `x`, so now we
   evaluate:
   
       x [ f -> (\y. x), y -> 1 ]

   **But wait**!  How do we know what x is!?  We can't.  What should x
   be?  The answer, if we think about it carefully, is that x should
   be the environment where we evaluated e2 to a value.  In other
   words, we should have remembered what `x` was when we evaluated the
   value where the above line (starting with `>>>>`).

   We can solve this conundrum by pairing a lambda expression with an
   environment, creating a closure.  This is a pair:
   
       (\x. t), E

   Of a lambda term, and the environment that resolves the free
   variables.
*)


(* -------------------------------------------------------------------------- *)
(* Implementing the CEK machine                                               *)
(* -------------------------------------------------------------------------- *)

module CekMachine = struct
  (* Open the Lambda module so we can use the syntax. *)
  open Lambda
  (* 
     Let's start to define the CEK machine.  To begin with, the
     control is just going to be program terms:
   *)
  type control = term 
    
  (* Now that we have the C, let's define environments and
     continuations.  Environments are going to map variables to
     values.  But it turns out that in our machine, we can't just have
     values, we have to actually have **closures**.  So instead of
     mapping variables to values, the environment is going to map
     variables to a `machine_value`.  A machine value is kind of the
     internal representation of a value.  For now, the only values in
     our languages are going to be closures.

     Let's ask ourself, why aren't they going to be values?  In other
     words, why can't the environment map variables to variables?
     
     { x |-> y }         <<< Why **can't** this happen?

     The answer is that variables are just going to be looked up in
     the environment.  So, whenever we have to look up a variable x,
     or y, we'll use the current environment to look that up.

     So, we define machine values and environments:
  *)
  type machine_value =
    | Clo of term * environment
  and environment = 
    (var * machine_value) list

  (* 
     Next, we're going to define continuations.  

     Our computation in the small step machine is going to look as
     follows: to handle an application,
     
         t1 t2 
     
     How do we evaluate an application? 
       1) Evaluate t1 to a value, (\x. t')
       2) Evaluate t2 to a value, v2
       3) Evaluate the application of `(\x. t')` to v2

     A continuation tells us where to go **next**.  There are three
     possible cases for continuations when we're evaluating an
     expression:

       - Done.  There's nothing left to do, we're done with the
         computation.

       - Evaluate the argument.  This is when we're "focused in" on
   evaluating t1, and we have yet to evaluate t2.

       - Call the function (\x. t).  This is when we want to evaluate
         `t1 t2`, and we've evaluated `t1` to `(\x. t)`, and we're
         "focused in" in `t2`.  After we finish evaluating `t2`, we
         need to actually call `t1`.  How are we going to do that?
         We're going to make `x` point at the value we get from
         working on evaluating `t2`.  Then we're going to "focus in"
         on `t`.

     You see, the control serves as the "focus" of the machine.  It's
     what the machine is currently looking at to perform its work.
     When we think about it, the machine makes its move in three main
     ways: 
     
       - Changing the focus (control)
       - Changing the environment 
       - Changing what to do next (the continuation)
     
     All that being said, the continuation is going to look like this:
  *)

  type continuation =
    (* Nothing left to do after this *)
    | Done              
    (* Currently evaluating an e1 (in `e1 e2`), next evaluate `e2` *)
    | EvalArg of term * environment * continuation
    (* We've evaluated e1 to (\x. t) and are working on e2, next call *)
    | Call    of term * environment * continuation
                 
  (* Note that we've carefully laid out the continuation to include
     the environment.  This is very nuanced, we have to be careful
     about what environment is used in various places so we don't make
     a mistake. *)

  (* Now we can defin the machine state *) 
  type machine =
    control * environment * continuation
                 
  (* Here are a few utility functions to update the environment and
     (e.g.,) print states and such... *)
    
  let add var value environment = (var,value)::environment
  let rec lookup var = function
    | [] -> failwith ("variable " ^ var ^ " undefined")
    | (k,(v:machine_value))::tl -> if (k = var) then v else lookup var tl
    
  (* Pretty printing... *)
  let rec string_of_environment env =
    (L.fold_left (fun acc (k,v) -> acc ^ k ^ " |-> " ^ (string_of_machine_value v))
       "[ " env) ^ " ]"
  and string_of_machine_value = function
    | Clo (t,environment) -> "{ " ^ (string_of_term t) ^ " w/ environment "
                             ^ string_of_environment environment ^ " }"
  let rec string_of_continuation = function
    | Done -> "[]"
    | EvalArg(t,e,k) -> "Arg[" ^ string_of_term t ^ "," ^ string_of_environment e
                          ^ "," ^ (string_of_continuation k) ^ "]"
    | Call(t,e,k) -> "Call[" ^ string_of_term t ^ "," ^ string_of_environment e
                     ^ "," ^ (string_of_continuation k) ^ "]"
                     
  let string_of_machine (c,e,k) = "< " ^ string_of_term c ^ ", "
                                  ^ string_of_environment e ^ ", " 
                                  ^ string_of_continuation k ^ " >"


  (* ------------------------------------------------------------------------ *)
  (* Defining the step function                                               *)
  (* ------------------------------------------------------------------------ *)
  
  (* The machine is going to evolve in steps, where each step looks at
     the control (and continuation) and decides what to do.  The step
     function has the following type:
     
     step : state -> state
     
     It proceeds in cases:
  *)
  let step state = match state with
    
    (* The first case is simple enough.  Let's say that the control is
       `x`, meaning the machine is "focused in" on figuring out what
       the variable `x` is.  How do we look up what `x` is?  Well, we
       use the current *environment to tell us what it means.

       The environment holds closures, so we match the result of
       looking up the variable with the `Clo` constructor to get the
       lambda expression.  Remember that closures come packaged with
       an environment.  Say that the environment contains a closure
       for some variable `f`, `((\x. t), e')`.  That means `f` points
       at a function, but that function's variables need to be looked
       up in the environment `e`.  So now what do we do with the
       machine's focus?  Where do we put it?  Well, we need to focus
       in on t.  And we also need to change the environment.  What do
       we change the environment to?  We need to change it to `e'`,
       the environment given to us by the closure.
    *)
    | (Var x, e, k) ->
      (match (lookup x e) with
       | Clo (lambda,e') -> (lambda,e',k))
      
  (* 
       Let's think about what would happen if we *didn't* swap that
       environment.  Well, that would correspond to the case where we
       had this happen:
       
       (\f. f (\x. x)) ((\x. (\y. x)) (\z. z))

       When we try to apply that, we evaluate the argument, and we get
       a closure:
       
           (\y. x) , [x |-> ((\z. z),[])]
              t1                t2   ^^
                                     e2
                     ^-------------------^
                               e1

       Which means that we have the function t1, which needs to be
       executed in the environment e1.  The environment `e1`
       subsequently says that `x` is mapped to a closure whose term is
       `(\z.z)`, and when executing *that* expression, the environment
       should be the empty environment.

       When we start to execute `(\f. f (\x. x))` in the above line,
       we need to use the environment `e1` for `f`, because if we
       **didn't**, the machine would have no idea what `x` referred
       to.
       
       Alright, that handles the case for variables!
    *)
    (* Next, let's think, how do we evaluate an application?  Let's
       say our machine state is something like this: 
       
       < t1 t2, E, K >

       Where should our focus go first?  Well, the first thing we need
       to do is to execute e1.  So let's do that:
       
       < t1, E, K >    <<<<  WRONG

       But that's not quite right.  Because if we did that, after we
       finished executing e1, the machine would jump back to executing
       the continuation K.  So where do we need to do after we finish
       executing t1?

       < t1 , E , EvalArg(t2,E,K) >

       We actually need to evaluate the argument t2.  And when we
       evaluate t2, we need to do so using the **current**
       environment. We'll find out that otherwise, we don't know what
       environment to use.  The reason we need to save the current
       environment is similar to the reason we need to save local
       variables before calling a function: if we continued to use
       that function's variables upon its return, we would get the
       wrong results.

       All that being said, the definition of the `App` case is
       actually pretty simple!  *)
    | (App (t1,t2), e, k) ->
      (t1, e, EvalArg(t2,e,k))
      (* There are two cases left.  

         What happens when I'm evaluating an application `e1 e2`, and
         I've *finished* evaluating `e1` to a value `(\x. t)`.  Then
         my continuation will be EvalArg(t2,e,k).  Now what do I do?
         Well, I need to focus in on executing `t2`, and I need to use
         environment `e`, because I said I would get into trouble
         otherwise.  

         Then I need to use something for my continuation. What is
         that?  Well, after I finish evaluating the argument, I need
         to actually *call* the function, how do I represent that?
         With the `Call` constructor.  I call `(\x. t)` with the
         result.  And after that, I execute the continuation `k`.
         
         Written in code...
      *)
    | (Lam (x,t), e, EvalArg(t',e',k)) ->
      (t',e',Call(Lam(x,t),e,k))
      (* And now the last case.  How do I actually **call** the
         function?  Well, if I've evaluated `t1` to (\x. t'), and I've
         evaluated its argument to a lambda (remember, the only values
         are lambdas), then I'm here:
         
           t1       t2  
           ||       || 
         (\x. t) (\y. t')
              ^

         Now to call t1, I need to focus in on `t`, when I'm focused
         in on `t`, I need to **remember** that `x` is `(\y. t')`.
         But what if `(\y. t')` has variables it needs to look up?
         Well, those should come from the current environment, so I
         **actually** need to assign `x` to a closure.
         
         This is how I write that in code:
      *)
    | (Lam (x,t), e, Call(Lam(x',t'),e',k)) ->
      let extended_env = add x' (Clo(Lam (x,t),e)) e' in
      (t',extended_env,k)
      (*

         Think carefully about the place that I've put various terms
         and environments here.  Ask yourself, why do I extend e'
         rather than e?  (Answer: because I need to execute the lambda
         within the environment e'.)  

         After I'm done with all of that, I go back and execute the
       continuation `k`.  *)
      
    (* There's actually one case left: if none of those cases holds, I
       might be done, in which case I'm just going to hand back the
       state: *)
    | (_,_,Done) -> state
    (* And then I'm going to define a wildcard case to flag an error
       in any other configuration.  We'll talk more about this next
       time *)
    | _ -> failwith "no step defined." 

  
  (* Before I can start executing terms, I need to define how I start
     to play the game. What's the initial configuration?  I talked
     about this earlier, but the starting configuration is simply the
     configuration where we take the term we want to evaluate as the
     focus, then start with the empty environment, and the `Done`
     continuation.  *)
  let inject t = (t,[],Done)
  
  (* Here's an example term.. *)
  let example = App(Lam("x",Var "x"),Lam("y",Var "y"))
  
end

open CekMachine 

(* Homework (not graded): figure out and get an intuition for how the
   CEK machine is working.  One way to start is to define a bunch of
   example terms and do this with them:

   step (inject example)
   step (step (inject example))
   step (step (step (inject example)))
   ....

   until you get done.

   Figure out why various rules do various things, ask me questions
   when you get stuck. 

 *)

