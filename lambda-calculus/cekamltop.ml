(* CMSC 330 -- Summer 2015 
   Toplevel for CEKaml
 *)

open Cekamlsol
open Lexing

let pp = Printf.printf
let pe = Printf.fprintf stderr
let ps = print_string
let psn x = print_string x; print_newline ()

let main _ =
  psn "CEKaml v 0.1 toplevel";
  let rec loop environment =
    ps ">> ";
    let input = Lexing.from_string (read_line ()) in
    try
      let parsed_term = Parser.prog Lexer.main input in
      try 
        let (c,e',_) =
          Interpreter.compute_plus_environment (parsed_term,environment,Done) in
        loop e'
      with
      | _ -> psn "Error reducing term!\n"; loop environment
    with
    | Lexer.SyntaxError msg -> pe "%s%!\n" msg; loop environment
    | Parser.Error -> pe "%s\n" "Parsing error..."; loop environment
  in
  loop [];;

main ();
