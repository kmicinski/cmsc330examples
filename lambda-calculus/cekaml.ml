(* -------------------------------------------------------------------------- *)
(* CMSC 330 -- Project 3 -- Small Step Interpreter for CEKaml                 *)
(* -------------------------------------------------------------------------- *)

module L = List
module S = String

(* -------------------------------------------------------------------------- *)
(* CEKaml       abstract syntax                                               *)
(* -------------------------------------------------------------------------- *)

module Syntax = struct 
  type var = string    (* Variable names *)
  type name = string   (* Constructor names *)
    
  (* Built in operators. *)
  type builtin =
    | Plus
    | Times
    | And
    | Not
    | Or 
      
  (* The term representation *)
  type term =
    | Var of var                          (* Variables                          *)
    | Let of var * term * term            (* Let binding let x = ... in         *)
    | Lam of var * term                   (* (\x. e)                            *)
    | App of term * term                  (* e1 e2                              *)
    | NumLit of int                       (* literal integers                   *)
    | BoolLit of bool                     (* literal booleans                   *)
    | Builtin of builtin * term list      (* built in operators e1 + e2         *)
    | Ifthenels of term * term * term     (* if e1 then e2 else e3              *)
    | Fix of var * term                   (* fixpoints: let rec var = ...       *)
    | Variant of name * term list         (* constructors: C(e1,...,en)         *)
    (* Match patterns. 
       match t with 
         | C1(v11,...,v1k) -> b1
         | ...             -> ...
         | Cn(vn1,...,vnk) -> bn
       represented as 
       Match(t,[("C1",["v11",...,"v1k"],b1);
               ...
               ("C1",["v11",...,"v1k"],b1)])
    *)
    | Match of term * ((name * var list * term) list) 
  and
    (* A match pattern is a:
     - Constructor name
     - Variable list 
     - Term

     When matched, the term will be executed with the variables
     substituted into the environment.  *)
    match_pattern = (name * var list * term)
  
  let rec string_of_builtin = function
    | Plus -> "+"
    | Times -> "*"
    | And -> "&&" 
    | Not -> "!"
    | Or -> "||" 

  (* Turn a term into a string. *)
  let string_of_term t =
    let rec string_of_vlist vlist =
      List.fold_left
        (fun acc hd -> if acc = "" then hd else (hd ^ "," ^ acc)) "" (L.rev (vlist))
    in
    let rec h t = match t with
      | Var x -> x
      | Let (x,t1,t2) -> "let " ^ x ^ " = " ^ (h t1) ^ " in\n" ^ (h t2)
      | Lam (x,t') -> "fun " ^ x ^ " -> (" ^ (h t') ^ ")" 
      | App (t1,t2) -> "(" ^ (h t1) ^ " " ^ (h t2) ^ ")"
      | NumLit i -> string_of_int i 
      | BoolLit b -> string_of_bool b
      | Builtin (s,tlist) -> "(" ^ (string_of_builtin s) ^ " " ^
                             (L.fold_left (fun acc hd -> "(" ^ (h hd) ^ ") " ^ acc) "" (L.rev tlist))
      | Ifthenels (t1,t2,t3) -> "if " ^ (h t1) ^ " then " ^ (h t2) ^ " else " ^ (h t3)
      | Fix (f,t') -> "(fix " ^ f ^ " in " ^ (h t') ^ ")"
      | Variant (n,tl) -> n ^ "( " ^
                          (L.fold_left (fun acc hd -> "(" ^ (h hd) ^ "), " ^ acc) "" (L.rev tl))
      | Match (t, mptl) -> "match " ^ (h t) ^ " with \n" ^
                           (L.fold_left
                              (fun acc (n,vl,t) -> "| " ^ n ^ "(" ^ (string_of_vlist vl) ^ ") -> " ^ (h t))
                              ""
                              (L.rev mptl))
    in
    h t
end 

open Syntax 

(* -------------------------------------------------------------------------- *)
(* CEK-Style Small Step Interpreter                                           *)
(* -------------------------------------------------------------------------- *)

module Interpreter = struct
  (* 
     Do NOT modify these types / definitions, but read them to make
     sure you understand what they mean.
   *)

  (* The control string (current instruction) is simply a term *)
  type control = term 
    
  (* 

     Machine representation of Denotable values.  This is the domain D
     from the lecture notes.
   *)
  type closure = term * environment (* A closure: internal representation of a 
                                       function, with an environtment we will 
                                       use to execute it. *)
  and mvalue =
    | Closure of closure
    | Int of int
    | Boolean of bool
    | MVariant of name * mvalue list (* A variant is a constructor name and a
                                       list of values *)
                   
  (* Environments map variables to machine values. *)
  and environment = (var * mvalue) list
  (* 
     What to do next.  There are a few possibilities:
     
     - Done: no work to do next.

     - EArg(t2,E,K): currently evaluating a function (t1) of the
       application t1 t2.  **Next** evaluate t2, in environment E, and
       then do K.

     - Call(t1,E,K): currently evaluating an argument (t2) of the
       application t1 t2.  **Next** call the function by putting the
       control at the term inside of t (where t1 = (\x. t)), and
       setting the environment to E.  After we do this, do K.

     - DecideTF(t1,t2,E,K): currently evaluating a guard (e1) of an if
       statement `if e1 then e2 else e3`.  Next decide whether the
       guard is true or false, and if so execute t1 (or t2) in the
       environment E, doing K after that.
     
     - EvalPrimArg(evaluated_args,next_args,E,K): In the expression
         + t1 t2 ... tn

       Currently evaluating tk (where 0 < k <= n), and have already
       evaluated t1...t(k-1), whose results are sitting in
       evaluated_args.  Next is t(k+1)...tn, sitting in next_args,
       evaluate each of those in the context of E, and then do K.
     
   *)
  and continuation =
    | Done
    | EArg of term * environment * continuation
    | Call of term * environment * continuation
    | DecideTF of term * term * environment * continuation
    | EBArg of builtin * mvalue list * term list * continuation
                     
  type state = control * environment * continuation

  (* ------------------------------------------------------------------------ *)
  (* Environment                                                              *)
  (* ------------------------------------------------------------------------ *)
  let empty_environment = []
  let update_environment var value environment = (var,value)::environment
  let lookup var environment = failwith "undefined"
      
  (* ------------------------------------------------------------------------ *)
  (* Pretty printing                                                          *)
  (* ------------------------------------------------------------------------ *)
  let rec string_of_environment env =
    (L.fold_left (fun acc (k,v) -> acc ^ k ^ " |-> " ^ (string_of_machine_value v))
       "[ " env) ^ " ]"
  and string_of_machine_value = function
    | Closure (t,environment) -> "{ " ^ (string_of_term t) ^ " w/ environment "
                             ^ string_of_environment environment ^ " }"
    | Int i -> string_of_int i 
    | Boolean b -> string_of_bool b
    | MVariant (n,vs) -> n ^ "(" ^ 
                         (List.fold_left
                            (fun acc hd ->
                               if (acc = "") then (string_of_machine_value hd)
                               else ((string_of_machine_value hd) ^ "," ^ acc))
                            "" (L.rev (vs)))

  let rec string_of_continuation = function
    | Done -> "Done"
    | EArg (t,e,c) -> "EArg(" ^ (string_of_term t) ^ "," ^ (string_of_environment e) ^ (string_of_continuation c)
    | Call (t,e,c) -> "Call(" ^ (string_of_term t) ^ "," ^ (string_of_environment e) ^ (string_of_continuation c)
    | DecideTF (t1,t2,e,c) -> "DecideTF(" ^ (string_of_term t1) ^ "," ^
                              (string_of_term t2) ^ (string_of_environment e)
                              ^ (string_of_continuation c)
    | EBArg (bi,mvl,tl,k) -> "EBArg(" ^ (string_of_builtin bi) ^ "(" ^
                             (List.fold_left
                                (fun acc hd ->
                                   if (acc = "") then (string_of_machine_value hd)
                                   else ((string_of_machine_value hd) ^ "," ^ acc))
                                "" (L.rev (mvl)))
                             ^ "),("^
                             (List.fold_left
                                (fun acc hd ->
                                   if (acc = "") then (string_of_term hd)
                                   else ((string_of_term hd) ^ "," ^ acc))
                                "" (L.rev (tl)))
                             ^ 
                             ")," ^ (string_of_continuation k) ^ ")"
                     
  let string_of_machine (c,e,k) = "< " ^ string_of_term c ^ ", "
                                  ^ string_of_environment e ^ ", " 
                                  ^ string_of_continuation k ^ " >"

  (* ------------------------------------------------------------------------ *)
  (* Utility functions                                                        *)
  (* ------------------------------------------------------------------------ *)
                  
  (* 
     YOUR code begins here.
   *)

  (* Create an initial state *)
  let inject state = failwith "undefined"
  
  (* Determine if a state is final *)
  let final_state s = failwith "undefined"

  (* {step s} takes a state and steps it to a new state. 

      NOTE: This is the main function to be implemented for this
      project.
     
  *)
  let step (s : state) : state = failwith "undefined"
      
  (* `compute e` takes `e` and repeatedly applies `step` until a final
     state is reached.  You *must* implement this function in a tail
     recursive way.  *)
  let compute (e : term) = failwith "undefined"
end











