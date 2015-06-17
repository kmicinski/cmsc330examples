(* Type annotations *)

let (x : int) = 3           (* "x has type int" *)
let z = (x : int) + 5



(* _Very_ useful for debugging *)

let area_of_int (x:int) : float =
    (float_of_int x) *. 3.14 *. 3.14;;

area_of_int(3);;


