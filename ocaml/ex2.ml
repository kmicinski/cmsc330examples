let calcPercent (grade,max) = 
  ((float_of_int grade) /. (float_of_int max)) *. 100.0

let calcPercent' grade max = 
  ((float_of_int grade) /. (float_of_int max)) *. 100.0

let transform (f : ('a*'b) -> 'c) : 'a -> 'b -> 'c = 
  let h a b = 
    f (a,b)
  in 
  h



