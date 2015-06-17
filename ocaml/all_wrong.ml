print_int "This function expected an int";;

1 + true;;

1 + 0.5;;

[1; 2; "lists must be homogeneous"];;

[1, 2, 3];;  (* probably not what you expect; 
                this list has _one_ element *)

[1;2] :: 3   (* expects a list on the right of :: *)


let hd ls = match ls with (h::_) -> h;;
hd [];;
