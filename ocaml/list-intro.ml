(* A first taste of lists in OCaml *)


[1;2;3];;        (* primitive data type *)

[ [1;2]; [3;4] ] (* nested lists *)

[];;             (* empty list *)

(* What are the types of the above lists? *)



(* Constructing lists *)

3 :: [];;
2 :: (3 :: []);;
1::2::3::[];;

let x = [1;2;3];;
let y = 4 :: x;;

let y = x :: 4;;   (* does not work; "A :: B" means that
                        B should be a list containing
                        whatever type A is *)

(* construct a 'z' such that the following works: *)
let y = x :: z;;


(* More list type practice *)

[[[]; []; [1.3;2.4]]];;
