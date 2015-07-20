(* 
   CMSC 330 -- Summer 2015 
   Lectures on the OCaml module system
 *)

(* So far, we've been writing OCaml code that is comprised of single
   files. But large projects are typically comprised of many
   components that need to be designed, implemented, and validated
   independently.

   In object oriented languages, we typically structure code using
   classes and interfaces.  Each language is slightly different, but
   the overarching concept is modularity: classes offer a way to group
   code into logical components.

   As we design larger and larger software, we begin to realize that
   modularity and encapsulation are a key aspect of designing robust
   code.  In a large system, we will naturally find that code needs to
   change to accommodate new features.  If our system is not organized
   in a way that allows this to happen, the code quality can easily
   diminish over time.

   In OCaml (and its predecessor, Standard ML) we group code using
   modules.

*)

(* -------------------------------------------------------------------------- *)
(* Module basics                                                              *)
(* -------------------------------------------------------------------------- *)

(* Let's think back to our implementation of association maps.  We
   could have implemented them using association lists: *)
type ('a, 'b) assoc_map_list = ('a * 'b) list 
let empty_map = []
let add_entry key value l = (key,value)::l
let rec lookup key = function
  | [] -> failwith "no matching key"
  | (hd,v)::tl -> if key = hd then v else lookup key tl

(* When a programmer wants to use this code, they need to copy and
   paste it into their file.  We should be anxious about code smell
   whenever we are copy and pasting, there is almost always a better
   way.  It pollutes the codebase: the implementation of association
   lists is in the same place as the code that uses it (confusing the
   reader).  But it also leads to potential error.  When multiple
   pieces of code use association lists, what do they do.

   In the toplevel, you might think the answer is that they could
   write `#use` and the file name.  This is a solution very similar to
   `#include` in C, but doesn't work for compiled software and doesn't
   have the advantages of real modules we'll soon see.
*)

(* So as an alternative, let's group the association list in a
   module. 
   
   modules have the syntax:
*)
module LMap = struct 
  (* The things between the `struct` and the `end` are the module's
     components.  This is similar to the beginning and ending curly
     braces for Java classes. *)
  
  (* Inside modules we can write code as usual: *)
  type ('a, 'b) assoc_map_list = ('a * 'b) list 
  let empty_map = []
  let add_entry key value l = (key,value)::l
  let rec lookup key = function
    | [] -> failwith "no matching key"
    | (hd,v)::tl -> if key = hd then v else lookup key tl
end

(* 
   Now, if we type this into OCaml, OCaml will tell us:

    module Tree :
      sig
        type ('a, 'b) assoc_map_list = ('a * 'b) list
        val empty_map : 'a list
        val add_entry : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
        val lookup : 'a -> ('a * 'b) list -> 'b
      end

   Typing in the module gave us a *signature*.  The signature
   (beteween the `sig` and `end`) is the specification for the module:
   it includes the types of the components, along with their names,
   but not the implementation. Think of the signature as the module's
   interface.

   When the programmer uses the `Tree` module, they will program
   against its signature.  Just like OCaml terms have types, OCaml
   modules have signatures.  And just as types are checked, signatures
   are checked too.  We can force a module to have a certain signature
   by using a notation we'll talk about later. If we leave it off,
   OCaml will infer the signature for us, just like with terms.
   
 *)

(* To use things from the `Tree` module, we prefix functions inside
   the module with the module's name:  *)

let a = LMap.add_entry "h" 1 LMap.empty_map 

(* This can sometimes be frustratingly verbose, and so we sometimes
   alias the module with a simpler name: *)

module M = LMap

let a' = M.add_entry "h" 1 M.empty_map 

(* -------------------------------------------------------------------------- *)
(* Signatures and Specifications                                              *)
(* -------------------------------------------------------------------------- *)

(* In the above module, we implemented association maps as lists.  But
   as we've seen, we also could have choosen to implement association
   maps as trees.  So let's make another module that implements
   assocation maps as trees:
*)
module TMap = struct 
  type ('a, 'b) assoc_tree = 
    | Empty
    | Leaf of 'a * 'b * ('a, 'b) assoc_tree * ('a, 'b) assoc_tree
                
  let empty_map = Empty
  let rec add_entry key value t = match t with 
    | Empty -> Leaf (key,value,Empty,Empty)
    | Leaf (k,v,t1,t2) ->
      if key = k then t
      else 
        (if key < k then
           Leaf (key,v,add_entry key value t1, t2)
         else 
           Leaf (key,v,t1,add_entry key value t2))
        
  let rec lookup key = function
    | Empty -> failwith "no matching key"
    | Leaf (k,v,t1,t2) ->
      if k = key then v else (lookup key (if key < k then t1 else t2))
end

(* Now we have two implementations of association maps. 

   Let's say we have some code that uses an `LMap`.  We might like to
   be able to switch easily between an `LMap` and a `TMap`: they're
   both doing the same thing: the programmer isn't using anything
   specific about their implementation.

   But what if the programmer *does* use something about their
   implementation.  Let's think about the following use of LMap:

*)
let x = LMap.lookup "hello" [("hello","world")]

(* val x : string = "world" 

   But now, this won't work for TMaps:
   
   # let x = TMap.lookup "hello" [("hello","world")]
      Error: This variant expression is expected to have type
        (string, 'a) TMap.assoc_tree
      The constructor :: does not belong to type TMap.assoc_tree

   What went wrong here?  TMap lookup relies on being passed a tree.
   To fix this, we need *encapsulation*.  The implementation of maps
   should be able to do whatever it wants for the type.  It should be
   internal to `TMap` or `LMap`, but the programmer shouldn't be able
   to rely on it.  A similar concept arises in object oriented
   programming: you shouldn't rely on objects' private member
   variables.  We make variables private because they hold details
   that would break the abstraction boundary between the
   implementation and the interface.

   To fix this problem, we're going to make an interface common to all
   assocation maps.  Earlier we said that OCaml would infer a module
   specfication if we didn't manually write one.  But sometimes this
   is bad, because we want to force a more abstract implementation of
   the module.  OCaml can't know a priori what we want the interface
   to be: that's our choice as the designer.  So let's design a
   signature for what we think assocation maps should look like:
*)

(* Note that modules use `module M = struct ... end` and signatures
   use `module type M = sig ... end`
 *)

module type ASSOC_MAP = sig 

  (* Here we're saying that the module must have *some* type named t
     (with two parameters).  But we aren't saying what it is. It could
     be anything, it could be assocation list, it could be a tree,
     whatever.  All we require is that it's named `t`. 

     Because we haven't specified what the type is, the programmer is
   **not allowed** to know.  We'll come back to this later.
  *)
  type ('a, 'b) t
      
  (* Here, we're going to stipulate that there is a value named
     `empty_map`, which has that type. *)
  val empty_map : ('a, 'b) t
      
  (* And so on for the other components... *)
  val add_entry : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val lookup : 'a -> ('a,'b) t -> 'b
end

(* Now we have a specification, let's start writing modules that have
   that type.  To force a module to adhere to a specification, we use the syntax 
   
       module Name : NAME = struct ... end

   Where `Name` is the module's name, and `NAME` is the name of some
   signature we have already defined.  We can also specify the
   signature right there by writing `sig ... end` instead of `NAME`
   (just like we can write `23` instead of `x`).  But we usually
   define them separately for readibility.
   
   Let's try this with our list module:
*)
module LMap2 : ASSOC_MAP = struct 
  type ('a, 'b) t = ('a * 'b) list 
  let empty_map = []
  let add_entry key value l = (key,value)::l
  let rec lookup key = function
    | [] -> failwith "no matching key"
    | (hd,v)::tl -> if key = hd then v else lookup key tl
end

(* Note that we had to change the type `assoc_map_list` to `t`.
   Otherwise OCaml would complain to us with the following error:
   
    File "trees.ml", line 222, characters 27-282:
    Error: Signature mismatch:
           ...
           The type `t' is required but not provided
           File "trees.ml", line 198, characters 7-17: Expected declaration   

   This error happens because the module implementation `LMap2` does
   not meet its specification.  And OCaml explicitly tells us why it
   doesn't, just like with a type error.
*)

(* Let's try our bad code again: 
   
   let x = LMap2.lookup "hello" [("hello","world")]

   OCaml yells at us:

    Error: This expression has type 'a list
           but an expression was expected of type (string, 'b) LMap2.t

   Why?  Inside LMap2, it's obvious the implementation uses a list, so
   this should typecheck.  But the answer is that we've **hidden** the
   definition of the type `t` outside of the module.  When we're
   outside of `LMap2`'s implemetation, the programmer is only allowed
   to know that there is a type `t`.  To use that type `t`, they have
   to use the functions provided in the module to interact with it.

   This is encapsulation in action.  Instead, we're forced to write the code like this:
   
*)

let x = LMap2.add_entry "hello" "world" LMap2.empty_map
let y = LMap2.lookup "hello" x

(* This is great, because now we can define TMap2 the same way.  It
 * almost* works, but the type is named the wrong thing.  We really
   want a module that is the same as TMap, but has a type `t` that is
   equal to `assoc_tree` for TMap: *)
module TMap2 : ASSOC_MAP = struct 

  (* You might think that we have to repeat the entire definition of
     the module here.  But we actually don't.  We can use the `include`
     keyword, which takes the components of the module `TMap`, and
     includes them in this module. *)
  include TMap
      
  (* And now we just define a type `t` that is equal to `assoc_tree`. *)
  type ('a, 'b) t = ('a, 'b) assoc_tree
end

(* Now we can use the module just like LMap2. *)
let x = TMap2.add_entry "hello" "world" TMap2.empty_map
let y = TMap2.lookup "hello" x

(* Now we can swap from tree maps to list maps at will.  
   
   The typical way this is done is something like:
*)
module M = LMap2

(* code that uses M .... *)

(* Now, if we later decide we want a different module, we can simply
   change `LMap2` to `TMap2`.
*)

(* -------------------------------------------------------------------------- *)
(* Parameterized Modules and Functors                                         *)
(* -------------------------------------------------------------------------- *)

(* In our implementation of TMap2, we used the `<` operator to
   implement comparison.  This is alright, because the `<` operator is
   defined for all types.  But the `<` operator really only makes
   sense for things like integers.  A better design would be for the
   programmer give us an implementation of `<` that they wanted us to
   use to compare keys.

   But how could we do that.  One way is that we could have the
   programmer write a separate map implementation for each tree map:
   
   module IntKeyTreeMap : ASSOC_MAP = ... 
   module StringKeyTreeMap : ASSOC_MAP = ...
   
   etc...

   But a better implementation would be to define tree based
   association maps agnostic of the comparison operation.  To do this
   we can use a *functor*, which is a module that accepts a module as
   a parameter:
   
*)

module type ORDERED_KEY = sig
  type t
    
  (* `compare a b` will return: 
       - 0, when `a` and `b` are equal
       - <0, when `a` is greater than `b`
       - >0, when `a` is less than `b`
     
     E.g., for ints it could be `b-a`
  *) 
  val compare : t -> t -> int
end

module TreeMap (Key : ORDERED_KEY) = struct
  (* Notice how the previous ('a, 'b) becomes simply 'a. *)
  type 'a t = 
    | Empty
    | Leaf of Key.t * 'a * 'a t * 'a t
                
  let empty_map = Empty
  let rec add_entry key value t = match t with 
    | Empty -> Leaf (key,value,Empty,Empty)
    | Leaf (k,v,t1,t2) ->
      if (Key.compare key k = 0) then t
      else 
        (if (Key.compare key k < 0) then
           Leaf (key,v,add_entry key value t1, t2)
         else 
           Leaf (key,v,t1,add_entry key value t2))
        
  let rec lookup key = function
    | Empty -> failwith "no matching key"
    | Leaf (k,v,t1,t2) ->
      if (Key.compare k key = 0) then v else
        (lookup key (if (Key.compare key k < 0) then t1 else t2))
end

(* Now, note that we can't simply use TreeMap.empty_map: 
   
       # TreeMap.empty_map;;
       Error: The module TreeMap is a functor, not a structure

   To actually use TreeMap, we need to create a concrete instance of
   it.  Let's create one that uses integer pairs (x,y) as the keys:
*)
module M = TreeMap(struct
    type t = int * int
    let compare (x1,y1) (x2,y2) =
      if x1 >= x2 then y1 - y2
      else - (y2 - y1)
  end)

(* And then, TreeMap(IntPairKey) *)

let m = M.add_entry (3,5) "hello" M.empty_map
let x = M.lookup (3,5) m

(* Now, I can create a custom comparator to be used for individual map
   instances. *)

(* -------------------------------------------------------------------------- *)
(* Using the `with type` notation                                             *)
(* -------------------------------------------------------------------------- *)

(* In the last module I wrote, I made a little mistake: I didn't force
   it to implement the `ASSOC_MAP` signature.  Let's do that:

   module TreeMap2 (Key : ORDERED_KEY) : ASSOC_MAP = TreeMap

       Error: Signature mismatch:
       Modules do not match:
       functor (Key : ORDERED_KEY) ->
         sig
         type 'a t =
         'a TreeMap(Key).t =
             Empty
           | Leaf of Key.t * 'a * 'a t * 'a t
         val empty_map : 'a t
         val add_entry : Key.t -> 'a -> 'a t -> 'a t
         val lookup : Key.t -> 'a t -> 'a
       end
       is not included in
       ASSOC_MAP

   The reason is this: the ASSOC_MAP signature has two type parameters
   for the map, but TreeMap only has one.  To fix this problem, we'll
   change the ASSOC_MAP signature a little bit to include the key as a
   type within the structure:
*)
module type ASSOC_MAP = sig 
  type key 
  type 'value t
  val empty_map : 'value t
  val add_entry : key -> 'value -> 'value t -> 'value t
  val lookup : key -> 'value t -> 'value
end

(* Now we should be able to do it by including the previous
   implementation of `TreeMap` *)
module TreeMap2 (Key : ORDERED_KEY) : ASSOC_MAP = struct 
  (* In this module, we have now named a key type, and specified it
     must be the type `t` from `Key` *)
  type key = Key.t
               
  (* And now we just want to include the previous implementation *)
  include TreeMap(Key)
end

(* And an example key datatype. *)
module IntPairKey : ORDERED_KEY = struct
  type t = int * int
  let compare (x1,y1) (x2,y2) =
    if x1 >= x2 then y1 - y2
    else - (y2 - y1)
end

(* Let's make an example tree map with our ordered key, and then add
   things to it: *)
module TM = TreeMap2(IntPairKey)

(* If we try to do..

   # TM.add_entry (1,2) "hello" TM.empty_map;;
   Error: This expression has type 'a * 'b
     but an expression was expected of type
     TM.key = TreeMap2(IntPairKey).key

   We get an error immediately!  Why?  We know that the type of the
   key defined in IntPairKey was an int pair.  Here, OCaml is hiding
   something from us: it's hiding the association that the type of
   `key` in `TreeMap2(S : ORDERED_KEY)` (for a given structure `S`) is
   equal to the type `S.t`.

   To get past this, we need to employ a little bit of a hack.  We
   need to use an annotation, usually known as a `with type`
   annotation.  These annotations allow us to assert type equalities
   to OCaml's type checker, and give the extra hint that allows us to
   get us to our goal.  To do this, we restructure `TreeMap2` like so:
*)

module TreeMap3 (Key : ORDERED_KEY) : (ASSOC_MAP with type key = Key.t) = struct 
  (* In this module, we have now named a key type, and specified it
     must be the type `t` from `Key` *)
  type key = Key.t
               
  (* And now we just want to include the previous implementation *)
  include TreeMap(Key)
end

(* Notice that the only thing that changes here is the signature we
   put on the `TreeMap3` functor.  We're telling OCaml, "this module
   has signature ASSOC_MAP, but let the type checker know that the
   type of key inside ASSOC_MAP is equal to (the same type as) the
   type `Key.t` from the `Key` module on which the functor is
   parameterized."

   Now we can do like so...
*)
module TM = TreeMap3(IntPairKey)

(* And now we get... 

    # TM.add_entry (1,2) "hello" TM.empty_map;;
    Error: This expression has type 'a * 'b
       but an expression was expected of type TM.key = IntPairKey.t

   So we're still not quite done.  Unfortunately, now OCaml is telling
   us that it doesn't know the type of `IntPairKey.t`.  This is to be
   expected: we've "hidden" that type by using the module.  The
   easiest way to fix this is to add another `with type` annotation to
   the `IntPairKey` module:

*)
module IntPairKey : (ORDERED_KEY with type t = int * int) = struct
  type t = int * int
  let compare (x1,y1) (x2,y2) =
    if x1 >= x2 then y1 - y2
    else - (y2 - y1)
end

module TM = TreeMap3(IntPairKey);;

(* And now... *)
TM.add_entry (1,2) "hello" TM.empty_map

(* - : string TM.t = <abstr> *)
