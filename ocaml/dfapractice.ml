type state = int

type symbol = char

type transition = state * symbol * state

(* 
 * Set of states,
 * Alphabet,
 * Initial state, 
 * Transition list,
 * Accepting states
 *)
type dfa = state list * symbol list * state * transition list * state list

let d : dfa = ([0],['1'],0,[(0,'1',0)],[])

let states (d:dfa) = match d with
  | (s,_,_,_,_) -> s

(* What happens if I leave off dfa annotation? Mess up an element *)
let transitions ((_,_,_,t,_):dfa) = t

(*
let states ((s,_,_,_,_):dfa) = s
*)

(* Using the record notation.  
   https://realworldocaml.org/v1/en/html/records.html
 *) 
type dfa = 
  { 
    states : state list;
    sigma : symbol list;
    start : state;
    transitions : transition list;
    accepting : state list;
  }

(* Dereference with .field, e.g., dfa.states *)
let states dfa = dfa.states

let addTransition (t:transition) dfa = { dfa with transitions = t::dfa.transitions }

(* Helper function. *)
let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let rec contains e l = 
  match l with
  | [] -> false
  | hd::tl -> if hd = e then true else contains e tl

let checkAccepts str dfa = 
  let symbols = explode str in
  let next_state state symbol = 
    let rec find_state l =
      match l with
      | (s1,sym,s2)::tl ->
        if (s1 = state && sym = symbol) then s2
        else find_state tl
      | [] -> failwith "no transition defined"
    in
    find_state dfa.transitions
      a





let checkAccepts str dfa = 
  let symbols = explode str in
  (* If I'm at state {state}, where do I go on {symbol}? *)
  let next_state state symbol = 
    let rec find_state l = 
      match l with
      | (s1,sym,s2)::tl ->
        if (s1 = state && symbol = sym) then s2
        else
          find_state tl
      | _ -> failwith "no next state"
    in
    find_state dfa.transitions
  in
  (* Start the search at current_state, consuming symbol list *)
  let rec search_from current_state symbol_list =
    match symbol_list with
    | [] -> current_state
    | sym::tl -> search_from (next_state current_state sym) tl
  in
  let end_state = search_from dfa.start symbols in
  if (contains end_state dfa.accepting)
  then 
    true
  else 
    false

let d : dfa = 
  { states = [0;1];
    sigma = ['0';'1'];
    start = 0;
    transitions =
      [(0,'0',0);
       (0,'1',1);
       (1,'0',0);
       (1,'1',1)];
    accepting = [1]
  }


