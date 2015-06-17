(* Polymorphic types *)

let hd (h::_) = h;;

(* We know hd works on any kind of list, i.e., it's polymorphic: *)
hd [1; 2; 3]
hd ["now"; "they're"; "strings"]
hd [("and", 1); ("now", 2); ("tuples", 3)]

(* So what is hd's type? *)



(* Some polymorphic functions *)
let tl (_::t) = t

let swap (x,y) = (y,x)

let tls(_::xs, _::ys) = (xs, ys)

let eq(x,y) = x = y     (* how do you parse this? *)
