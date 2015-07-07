# CMSC 330 - Project 3 

# Errata

- 07/07/15 -- Small typos in call rule
- 07/07/15 -- **important** error in "fix" rule, fixed now

In this project, we will extend the CEK machine to handle a core
subset of OCaml.  In the lambda calculus, the terms were:

    t ::= 
        | x             Variables 
        | \x. t         Lambda abstractions
        | t1 t2         Applications

In our language, TypelessCaml, we'll enhance this to include many
other features as well: 
    
    t ::= 
        | x                           Variables 
        | \x. t                       Lambda abstractions
        | t1 t2                       Applications
        | n                           Integral literals
        | b                           Boolean literals
        | let x = t1 in t2            Let binding
        | op t1 ... tn                Builtin operations
        | if t1 then t2 else t3       If/Then/Else
        | fix f in ...                Recursion
        | Ctr (t1, ..., tn)           Constructors
        | match t with | C1(x1,..,xn) -> t1 | C2(x1,..,xn) -> t1 | ...
        ^^ Match patterns

# Components

The **control** will be just a term

The **machine values** (which I'll call mvalues) will be:
- Closures of lambda terms and their associated environments `[ \x. t, E ]`
- Integers
- Booleans 
- Variants of constructor names and argument lists (also values) `Ctr(v1,...,vn)`
  
The **environment** will map variables to machine values, and will be
a list of (variable, mvalue) pairs.

The **continuation** will have several possibilities:

- `Done`: Do nothing after this

- `EArg(t, E, K)`: Evaluate a function's argument, in the context of
  environment E

- `ECall(\x. t, E, K)`: Call the function `\x. t` with environment E and
  do K afterwards

- `DecideTF(t1,t2,E,K)`: Next decide whether the current thing is true
  or false.  If it's true, start executing t1, if it's false start
  executing t2, do it in the environment of E with continuation K.

- `EVariant(name,evaluated_args,next_args,E,K)`: In the expression
    
    Ctr (t1,....,tn)
  
  We are evaluating some tk (where 0 < k <= n), and have already
  evaluated `t1...t(k-1)`, whose results are sitting in `evaluated_args`.
  Next is `t(k+1)...tn`, sitting in `next_args`, evaluate each of those in
  the context of E, and then do K. After that, take the resulting
  mvalues and form a variant from them.

- `EBArg(bi,evaluated_args,next_args,E,K)`: In the expression

         + t1 t2 ... tn

  Currently evaluating tk (where 0 < k <= n), and have already
  evaluated `t1...t(k-1)`, whose results are sitting in `evaluated_args`.
  Next is `t(k+1)...tn`, sitting in `next_args`, evaluate each of those 
  in the context of E, and then do K.  `bi` represents the specific operator,  
  in this case it's plus.


We'll explain these again later when we show the machine rules.

# Injection

How does the machine begin?  If we want to execute term e, the inject
function creates a machine state with e, the empty environment, and
the `Done` continuation:

    inject(e) = < e, [] , Done>

# Step function

The rules for each of the cases in the step function follow.  When
reading these rules, be careful to note which expressions move into
and out of the control, where the environments appear and move, and
how the continuations evolve:

## Evaluating a variable

### When it refers to a closure

Then we need to swap the current environment to the environment the
closure provids for us:

    if E(x) = [ (\x. t ) , E' ], then
    < x , E , K > --> < (\x. t ), E', K >

### When it refers to a literal / variant

Then we simply put that thing at the current control sring:
    
    if E(x) = {n|b|v(mv1,...,mvn)}
    < x , E , K > --> < {n|b|v(mv1,...,mvn)} , E , K > 

## Evaluating an application

We step to evauate `t1` and evaluate `t2` later:

    < t1 t2 , E , K > --> < t1 , E , EArg(t2,E,K) >

## Evaluating an argument

    < (\x. t) , E , EArg(t',E',K) > --> ( t', E', Call((\x. t) , E, K))

## Calling a function

### Calling with a function as an argument

If we are calling with a lambda term, we need to make a closure and
then *step into* the body:

    < (\x. t) , E , Call((\y. t'),E',K) > --> < t', { y |-> [ (\x. t), E ] } :: E', K >

Carefully note how the environments get swapped around.

### Calling w/ something else as an argument

Then no closure is necessary 

    < v , E , Call((\y. t'),E',K) > --> < t', { y |-> v } :: E', K >

## Evaluating a let binding

Involves the same machinery as an application:

    <let x = t1 in t2, E, K > --> <t1, E, Call(\x. t2,E,K)>

If this is confusing to you, remember that we could write:

    let x = t1 in t2 === (\x. t2) t1

And if we did that we would have:

    <(\x. t2) t1, E, K> --> <(\x. t2), E, EArg(t1,E,K)>
                        --> <t1, E, Call((\x. t2),E,K)>

Since the reduction is always going to take that form (since
`(\x. t2)` doesn't need to be evaluated any further), we just take a
shortcut and rewrite it to the second one.

## Evaluating an if

### Evaluating the guard

To evaluate

    if t1 then t2 else t3

We need to evaluate `t1`, then remember to go back and evaluate either
`t2` or `t3`, which we indicate with a `DecideTF` continuation:
    
    <if t1 then t2 else t3, E, K> --> <t1,E,DecideTF(t2,t3,E,K)>

### Deciding and evaluating the guard

After finishing evaluating the guard, we actually execute the branch

    <true,E,DecideTF(t2,t3,E',K)> --> <t2,E',K>

or...

    <false,E,DecideTF(t2,t3,E',K)> --> <t3,E',K>

Note that if the guard evaluated to anything *else*, we'd have a type
error, and we wouldn't define a next step.  You don't have to worry
about this in the project, since I defined that the programs would
always be well formed.

## Evaluating a built in 

To evaluate a built in, we need to evaluate each of its arguments in
sequence, and then perform the operation.  We start by seeing a
builtin expression and focusing on the first thing: 

    <op t1 t2 ... tn, E, K> --> <t1, E, EBArg(op,[],[t2,...,tn],E,K) >

`EBarg` represents that the next thing we have to do is evaluate t2.
The place where the empty list is, is the set of things we've
evaluated so far.  To go to the next one, we have another rule:

        <mv, E, EBArg(op,l,[tk,...,tn],E',K)> 
    --> <tk, E', EBArg(op,l@mv,[t(k+1),...,tn],E,K)

And then a rule to end the computation:

    if [|op l|] = v
    <mv,E,EBarg(op,l,[],E',K)> --> <v,E',K>

The notation `[|op l|]` means take the list of arguments to the
operator and apply it.  For example, if `l` is `1,2,3`, and `op` is
`+`, the result `v` should be `6`.

# Recursion with `fix`

Recursion works by "unrolling" the recursive definition.  In other
words, if we define a recursive function f in OCaml:

    let rec f = fun x -> if x = 0 then 1 else x*(f (x-1))

The way OCaml handles `f n` is to evaluate the body of `f`.  When
evaluating that body, f will be assigned to itself.

So for example, `f 1` is computed like so:

    f 1 
    = 
    (fun x -> if (x = 0) then 1 else x * (f (x-1))) 1
    = 
    if (1 = 0) then 1 else 1 * (f (1-1))
    = 
    1 * (f (1-1))
    = 
    1 * ((fun x -> if (x = 0) then 1 else x * (f (x-1))) 0)
    = 
    1 * (if (0 = 0) then 1 else 0 * (f (0-1)))
    = 
    1 * 1

Convince yourself why this works.  Once you are convinced, the
implementation of `let rec` is actually quite simple.  We call `let
rec` `fix` because it creates fixed points of functions.

In our syntax, `let rec f x = t` will be represented as

    fix f in (\x. t)

Its implementation is simple:

    <fix f in (\x. t), E, K> --> <t, {f |-> [fix f in (\x. t),E]}::E, K >

**Important errata**: In a previous version of this document, the "fix f in" in the conclusion of the above rule was left out.  This will cause errors for your implementation.

## End of the rules

And that should be all the rules you need!

Implementing matches and variants is just a little more complicated,
and will be extra credit for this project.  The reason relates to a
technicality when you have variants containing closures.  I'll write
up the guide and let people attempt extra credit.

# Running the computation

To actually *run* the computation, we take the reflexive transitive
closure of the step function.  This means, we evaluate the step
function to produce a sequence of states until we can't step any
longer. We'll detect the final state using the `final_state` function.
And at that point we'll have our answer:

    inject (e) --> .... -> (c, E, Done) 
    where final_state (c, E, Done) = true
    and the answer is c






