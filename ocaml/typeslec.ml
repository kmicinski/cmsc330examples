let fold (update : 'a -> 'b -> 'a) initial l = 
  let rec h acc l = match l with
    | [] -> acc
    | hd::tl -> h (update acc hd) tl
  in
  h initial l

let l = [13; 12; 16]
let initial = 0
let update x y = x + y

let sum_all_the_values = fold update initial;;

let invert x = -x;;

let filter (f : 'a -> bool) (l : 'a list) =
  let l' =
    fold (fun acc next_element -> 
        if (f next_element) then
          next_element :: acc
        else 
          acc)
      []
      l
  in
  List.rev l'




    


