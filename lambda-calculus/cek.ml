module L = List

module CekMachine = struct
  type var = string
    
  type term =
    | Var of var               (* Variables *)
    | Lam of string * term     (* Lambda abstractions *)
    | App of term * term       (* Applications *)

  let rec string_of_term = function
    | Var x -> x
    | Lam (x,t) -> "(\\" ^ x ^ ". " ^ (string_of_term t) ^ ")"
    | App (t1,t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"
                     
  type control = term 
  type machine_value =
    | Clo of term * environment
  and environment = 
    (var * machine_value) list
  and continuation =
    | Done
    | EvalArg of term * environment * continuation
    | Call    of term * environment * continuation
                 
  let add var value environment = (var,value)::environment
  let rec lookup var = function
    | [] -> failwith ("variable " ^ var ^ " undefined")
    | (k,(v:machine_value))::tl -> if (k = var) then v else lookup var tl
          
  type machine =
    control * environment * continuation
    
  (* Pretty printing... *)
  let rec string_of_environment env =
    (L.fold_left (fun acc (k,v) -> acc ^ k ^ " |-> " ^ (string_of_machine_value v))
       "[ " env) ^ " ]"
  and string_of_machine_value = function
    | Clo (t,environment) -> "{ " ^ (string_of_term t) ^ " w/ environment "
                             ^ string_of_environment environment ^ " }"
  let rec string_of_continuation = function
    | Done -> "[]"
    | EvalArg(t,e,k) -> "Arg[" ^ string_of_term t ^ "," ^ string_of_environment e
                          ^ "," ^ (string_of_continuation k) ^ "]"
    | Call(t,e,k) -> "Call[" ^ string_of_term t ^ "," ^ string_of_environment e
                     ^ "," ^ (string_of_continuation k) ^ "]"
                     
  let string_of_machine (c,e,k) = "< " ^ string_of_term c ^ ", "
                                  ^ string_of_environment e ^ ", " 
                                  ^ string_of_continuation k ^ " >"
                                  

  let step state = match state with
    (* How do we evaluate a variable? *)
    | (Var x, e, k) ->
      (match (lookup x e) with
       | Clo (lambda,e') -> (lambda,e',k))
    (* How do we evaluate an application? *)
    | (App (t1,t2), e, k) ->
      (t1, e, EvalArg(t2,e,k))
    (* What do we do when we have a lambda and need to evaluate the argument? *)
    | (Lam (x,t), e, EvalArg(t',e',k)) ->
      (t',e',Call(Lam(x,t),e,k))
    (* What do we do when we need to call the function we just computed? *)
    | (Lam (x,t), e, Call(Lam(x',t'),e',k)) ->
      let extended_env = add x' (Clo(Lam (x,t),e)) e' in
      (t',extended_env,k)
    (* What do we do when there are no steps left? *)
    | (_,_,Done) -> state
    (* Fail on all other cases. *)
    | _ -> failwith "no step defined..."
             
  let inject t = (t,[],Done)

end


open CekMachine
let example = App(Lam("x",Var "x"),Lam("y",Var "y"))



