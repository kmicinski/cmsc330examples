(* Conditionals *)

let print_grade score =
    if score >= 90 then 
        print_string "You got an A\n"
    else if score >= 80 then
        print_string "You got an B\n"
    else if score >= 70 then
        print_string "You got an C\n"
    else
        print_string "Let's all practice OCaml\n";;

print_grade 100;;


let is_the_answer y =
    let answer = 42 in
    y = answer;;            (* how do you grok this? *)

is_the_answer 17;;
is_the_answer 42;;


(* Comparing other types *)

let eq (x,y) = x = y;;      (* = is polymorphic, too *)

eq(3, 3);;

let x = "hi";;
let y = x;;
eq(x,y);;

eq("hi", "hi");;



(* == *)

let eqeq(x,y) = x == y;;

(* What is an _experiment_ we could run to figure out = vs. == ? *)

eqeq(3, 3);;

let x = "hi";;
let y = x;;
eqeq(x, y);;

eqeq("hi", "hi");;
