(* 'let' constructs *)
let x = 47;;


(* Often used for _local_ variables *)
let pi = 3.14 in            (*   {                       *)
    pi *. 3.0 *. 3.0;;      (*       float pi = 3.14;    *)
                            (*       pi * 3.0 * 3.0;     *)
                            (*   }                       *)
print_float pi;;            (*   printf("%f", pi);       *)


let x = 47;; (* overrides earlier definition *)
let x = 13 in x + 5;; (* overrides earlier one; uses it first *)
x;;


(* "let x = expression1 in expression2" -- what gets run when? *)
let x = print_string "ran e1\n" in print_string "ran e2\n";;


(* More examples *)
y;;

let y = 1 in y + 1;;

let y = y in y + 1;;

let y = 4 in let y = y + 1 in y;;   (* how do you parse this? *)
