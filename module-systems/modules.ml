let x = 23

module ListMap = struct
  type ('a, 'b) list_map = ('a * 'b) list
      
  let empty_map = []

  let add key value list = (key,value)::list
                           
  let rec lookup key = function
    | [] -> failwith "no key"
    | (hd,v)::tl -> if hd = key then v else lookup key tl 
end

module TreeMap = struct
  type ('a, 'b) tree_map =
    | Empty
    | Node of 'a * 'b * ('a, 'b) tree_map * ('a, 'b) tree_map

  let empty_map = Empty
    
  let rec add key value tm = match tm with
    | Empty -> Node (key,value,Empty,Empty)
    | Node (k,v,t1,t2) ->
      (if k = key then
         Node(k,value,t1,t2)
       else 
         (if key < k then
            Node(k,v,add key value t1, t2)
          else 
            Node(k,v,t1,add key value t2)))
      
  let rec lookup key tm = match tm with
    | Empty -> failwith "no key"
    | Node (k,v,t1,t2) -> 
      (if k = key then
         v
       else 
         (if key < k then
            lookup key t1
          else 
            lookup key t2))
end  

module type ASSOC_MAP = sig 
  type ('a, 'b) t
  val empty_map : ('a, 'b) t
  val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val lookup : 'a -> ('a, 'b) t -> 'b
end

module ListMap : ASSOC_MAP = struct
  type ('a, 'b) t = ('a * 'b) list
      
  let empty_map = []

  let add key value list = (key,value)::list
                           
  let rec lookup key = function
    | [] -> failwith "no key"
    | (hd,v)::tl -> if hd = key then v else lookup key tl 
end

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module IntOrder : ORDERED = struct
  type t = int
  let compare x y = y - x
end

module PairOrder : ORDERED = struct
  type t = int * int
  let compare (x1,y1) (x2,y2) =
    if (x1 > x2) then y1 - y2
    else y2 - y1
end

module type ASSOC_MAP = sig 
  type key
  type 'a t
  val empty_map : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val lookup : key -> 'a t -> 'a
end

module TreeMap (Key : ORDERED) : ASSOC_MAP with type key = Key.t = struct
  type key = Key.t
               
  type 'a t =
    | Empty
    | Node of key * 'a * 'a t * 'a t

  let empty_map = Empty
    
  let rec add key value tm = match tm with
    | Empty -> Node (key,value,Empty,Empty)
    | Node (k,v,t1,t2) ->
      (if (Key.compare k key = 0) then
         Node(k,value,t1,t2)
       else 
         (if (Key.compare key k < 0) then
            Node(k,v,add key value t1, t2)
          else 
            Node(k,v,t1,add key value t2)))
      
  let rec lookup key tm = match tm with
    | Empty -> failwith "no key"
    | Node (k,v,t1,t2) -> 
      (if (Key.compare k key = 0) then
         v
       else 
         (if (Key.compare key k < 0) then
            lookup key t1
          else 
            lookup key t2))
end  



