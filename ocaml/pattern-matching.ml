(* Pattern matching:
   
    match e with 
       p1 -> e1
     | p2 -> e2
     | p3 -> e3
*)

match 1+2 with
    3 -> true
  | i -> false;;


let is_odd x =
    match x mod 2 with
    0 -> false
  | 1 -> true
  | _ -> raise (Invalid_argument "is_odd");;    (* why do we need this? *)

(* Now let's work with lists *)
let x = [1;2];;
match x with 
    []  -> print_string "x is an empty list\n"
  | _   -> print_string "x is anything but an empty list\n";;


(* A function with pattern matching; this will get familiar *)
let is_empty ls = 
    match ls with
    [] -> true
  | (h::t) -> false;;

is_empty [];;
is_empty [1;2];;
is_empty [1];;
is_empty [ [] ];;


(* The matching patterns can be really powerful! *)
let is_vowel = function
    ('a' | 'e' | 'i' | 'o' | 'u') -> true
  | _ -> false;;

let is_upper = function
    'A' .. 'Z' -> true
    | _ -> false;;



(* The matching patterns are BINDING *)
let hd ls = 
    match ls with 
    (h::t) -> h;;

hd [1;2;3];;
hd [1];;
hd(hd [ [4;3]; [2;1] ]);;
(* hd [];; *)


(* Practice: Implement tl *)
(* Practice: construct a list ls such that hd(tl(hd ls)) returns 330 *)


(* Coding with wildcards *)

let is_empty ls =
    match ls with
    [] -> true
  | (_::_) -> false


let hd ls = 
    match ls with
    (h::_) -> h

(* More examples *)
let f ls =
    match ls with (h1::(h2::_)) -> h1 + h2;;

f [2;4;8];;


let g ls = 
    match ls with [h1; h2] -> h1 + h2;;
g [1;2];;
(* g [1;2;3];; *)


(* Abbreviated pattern matching 
 *    "let f p = e" 
 *       is the same as 
 *    "let f x = match x with p -> e"
 *)

let hd (h::_) = h
let f(x::y::_) = x + y
let g [x; y] = x + y


(* You probably won't do things quite like the following, but... *)
let addFirsts ((x::_) :: (y::_) :: _) = x + y;;

addFirsts [ [1;2;3]; [4;5]; [7;8;9] ];;
(* Will the following work? *)
(* addFirsts [ [1;2;3]; [4;5]; [7;8;9]; [10;11;12] ];; *)
