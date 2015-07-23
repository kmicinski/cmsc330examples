(* 
   Core TyplessCaml

   This is an interpreter for a core call by value language modeled
   after OCaml, in a big step substitution style.  

   Kristopher Micinski
 *)
module L = List
module S = String

type var = string    (* Variable names *)
type name = string   (* Constructor names *)

(* The term representation *)
type term =
  | Var of var                          (* Variables                          *)
  | Let of var * term * term            (* Let binding let x = ... in         *)
  | Lam of var * term                   (* (\x. e)                            *)
  | App of term * term                  (* e1 e2                              *)
  | Num of int                          (* literal integers                   *)
  | Bool of bool                        (* literal booleans                   *)
  | Builtin of string * term list       (* built in operators e1 + e2         *)
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

(* Term substitution, avoiding capture.  Note that this is sort of
   broken right now: substitution *should* alpha convert to avoid name
   capture.  
   XXX: fix this, KMM 06/24/15

   For students: don't worry about this, just assume substitution
   works. You don't need to care about how it's implemented for now.
*)
module Substitution = struct
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
    | Builtin (_,tl) -> L.flatten (L.map free_variables tl)
    | Fix (f,t) -> remove (free_variables t) f
    | Variant (n,t) -> L.flatten (L.map free_variables t)
    | Ifthenels (e1,e2,e3) -> L.flatten (L.map free_variables [e1;e2;e3])
    | Match (t,nvltl) ->
      difference
        (L.flatten (L.map (fun (_,vl,t) -> (difference (free_variables t) vl)) nvltl))
        (free_variables t)
    | _ -> []

  let fresh t =
    let rec vars t = match t with
      | Var x -> [x]
      | Let (x,t1,t2) -> x::(vars t1)@(vars t2)
      | Lam (x,t) -> x::(vars t)
      | App (t1,t2) -> (vars t1)@(vars t2)
      | Builtin (_,tl) -> L.flatten (L.map vars tl)
      | Fix (f,t) -> f::(vars t)
      | Ifthenels (e1,e2,e3) -> L.flatten (L.map vars [e1;e2;e3])
      | Variant (n,tl) -> L.flatten (L.map vars tl)
      | _ -> failwith "bad"
    in
    let nums = L.fold_left (fun acc hd -> 
        try
          (int_of_string (S.sub hd 1 (S.length hd-1)))::acc
        with _ -> acc
      ) [] (vars t)
    in
    "f" ^ (string_of_int (L.fold_left max 0 nums))

  let rec improved_substitute x t e = 
    match e with
    | Var y -> if (x = y) then t else e
    | Lam(y,t') -> 
      if (x=y) then
        e
      else 
        if (not (L.mem y (free_variables e))) then
          Lam(y,improved_substitute x t t')
        else
          failwith "can't replace." 
    | App(t1,t2) -> App((improved_substitute x t t1), (improved_substitute x t t2))
    | Let(y,t1,t2) -> 
      if (x=y) then
        e
      else if (not (L.mem y ((free_variables e)))) then
        Let(y,improved_substitute x t t1, improved_substitute x t t2)
        else
          failwith "can't replace." 
    | Builtin (n,tl) -> Builtin(n,L.map (improved_substitute x t) tl)
    | Fix(f,t') -> 
      if (x=f) then
        e
      else 
        if (not (L.mem f (free_variables e))) then
          Fix(f,improved_substitute x t t')
        else
          failwith "can't replace." 
    | Variant (n,tl) -> Variant (n,L.map (improved_substitute x t) tl)
    | Match (t',cvltl) ->
      Match((improved_substitute x t t),
            (L.map (fun (ctr,vl,t'') ->
                 (if (L.mem x vl) then (ctr,vl,t'')
                  else (ctr,vl,improved_substitute x t t''))) cvltl))
    | Ifthenels (e1,e2,e3) -> Ifthenels ((improved_substitute x t e1),
                                         (improved_substitute x t e2),
                                         (improved_substitute x t e3))
    | _ -> e

  let substitute = improved_substitute
end 

open Substitution

(* Helper function in pattern matching. *)
let rec zip lst1 lst2 = match lst1,lst2 with
  | [],[] -> []
  | [],_ -> failwith "unequal lengths"
  | _, []-> failwith "unequal lengths"
  | (x::xs),(y::ys) -> (x,y) :: (zip xs ys)

(* -------------------------------------------------------------------------- *)
(* Main evaluation relation                                                   *)
(* -------------------------------------------------------------------------- *)

(* 
   The following function implements the big step evauation reation: 
   
   e ⇓ v
 *)
let rec eval term = match term with
  (* 
     For values, I don't have to do any work to evaluate them.  In math:
     
     ----------  :: Value
       v ⇓ v
   *)
  | Var x -> term
  | Lam (x,t1) -> term
  | App (t1, t2) -> 
    let (Lam (x,v1)) = eval t1 in
    let v2 = eval t2 in
    eval (substitute x v2 v1)
  | Num _ -> term
  | Bool _ -> term
  (* `let x = e1 in e2` is literally just syntactic sugar for 
         (\x. e2) e1
     So we evaluate it that way.
   *)
  | Let (x,e1,e2) ->
    eval (App(Lam(x,e2),e1))
  (* To evaluate builtin operators, evaluate their parameters, and
     then apply operations in the metalangauge (OCaml) to perform
     things such as addition.
     n
     e₁ ⇓ v₁  ... en ⇓ vn   〚 op 〛(v1, ..., vn)
     --------------------------------------------
     op (e₁,...,en) ⇓ 
   *)
  | Builtin (s,tl) ->
    let values = L.map eval tl in
    let arith op i l =
      let rec h = function
      | [] -> i
      | (Num hd)::tl -> op hd (h tl) in
      Num (h l) in
    let boolop op i l =
      let rec h = function
      | [] -> i
      | (Bool hd)::tl -> op hd (h tl) in
      Bool (h l) in
    (match s with
     (* Here is the definition of the various builtin operators.  This
        corresponds to (in math) the 〚 op 〛syntax.  That's just math
        that means an operator that works on `Num n1 + Num n2`.
        Normally, I couldn't have + work on terms, so I make a helper
        function that allows + to work inside of terms.  
        
        As an example `Num 2 〚 + 〛Num 3` evaluates to `Num 5`.
     *)
     | "-" -> arith (-) 0 (values)
     | "+" -> arith (+) 0 (values)
     | "*" -> arith (fun x y -> x*y) 1 values
     | "/" -> arith (/) 1 (L.rev values)
     | "&&" -> boolop (&&) true values
     | "||" -> boolop (||) false values
     | "=" -> let ([Num a;Num b]) = values in
       if (a = b) then Bool true
       else Bool false
     | _   -> failwith "undefined primitive")
  (* 
     To evaluate fixpoints we "unroll the loop" one time.
     
     e.g., `fix f (\x -> f (x-1))`                   
           gets reduced to... 
           `(\x -> ((fix f (\x -> f (x-1))) (x-1)))`
     
     e { f ↦ (fix f e1) } ⇓ v
     --------------------------- :: Fix
     fix f e ⇓ v
     
   *)
  | Fix (x,e) -> substitute x term e
  (* To evaluate a variant, we simply evaluate its consituent components:
     
          e₁ ⇓ v₁  ...  en ⇓ vn
     ----------------------------- :: Variant
     C (e₁,...,en) ⇓ C (v₁,...,vn)
  *)
  | Variant (n,el) -> Variant (n,L.map eval el)
  (* There are two cases for if/then/else: when the guard is true, and
     when it's false.
     
     e₁ ⇓ true     e₂ ⇓ v
     -------------------------- :: IfTrue
     if e₁ then e₂ else e₃ ⇓ v

     e₁ ⇓ false     e₃ ⇓ v
     -------------------------- :: IfFalse
     if e₁ then e₂ else e₃ ⇓ v
  *)
  | Ifthenels (e1,e2,e3) ->
    (match (eval e1) with
     | Bool true ->
       eval e2
     | Bool false ->
       eval e3
     | _ -> failwith "tried to compare against a non boolean")
  (* 
     To evaluate
     match e with
         | C1(v11,...,v1k) -> b1
         | ...             -> ...
         | Cn(vn1,...,vnk) -> bn

     We first evaluate e to v, then figure out which constructor to
   execute, then interpret it after substituting. 
     
     e ⇓ Cm(v1,...,vj)    0 < m <= n   bm { vm1 ↦ v1, ..., vmk ↦ vj } ⇓ v
     --------------------------------------------------------------------- :: Match
     match e with | C1(..)->.. | ... | Cn(...)->.. ⇓ v
  
     It's not a **complicated** rule, just large.
  *)
  | Match(t,pl) ->
    let Variant (name,vals) = eval t in
    (* Find the case that matches. *)
    let [(_,varlist,matchbody)] = L.filter (fun (n,_,_) -> n = name) pl in
    (* Substitute each of the values from the match variable list with
       the concrete values inside the variant *)
    let rec subst_helper acc = function
      | [] -> acc
      | ((vname,value)::tl) -> subst_helper (substitute vname value acc) tl
    in
    eval (subst_helper matchbody (zip varlist vals))

(* Example programs. *)
let l = Lam("x",Lam("y",Builtin("*", [Var "x"; Var "y"])))
let l1 = App(l,Num 2)
let l2 = App(l1,Num 3)

let rec fac_ocaml =
  fun x ->
    if (x = 0) then 1
    else x * fac_ocaml (x-1)

let rec sum_ocaml =
  fun l ->
    match l with
    | hd::tl -> hd + (sum_ocaml tl)
    | []     -> 0
      
(* A function to cmpute the factorial *)
let fac =
  Fix("fac",
      (Lam("x",
           Ifthenels(Builtin("=", [Var "x"; Num 0]),
                     (Num 1), 
                     (Builtin("*", [Var "x";
                                    (App(Var "fac",
                                         (Builtin("-", [Var "x"; Num 1]))))]))))))

let example_list =
  Variant("Cons",([Num 1;Variant("Cons",([Num 3;Variant("Nil",[])]))]))

(* A function which sums a list. *)
let sum =
  Fix("sum",
      Lam("l",
          Match(Var "l",
                [("Cons", ["hd";"tl"], (Builtin("+",[Var "hd";App(Var "sum", Var "tl")])));
                 ("Nil", [], (Num 0))])))
     
                      
                                          
