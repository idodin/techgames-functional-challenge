(*PRELUDE*)
exception NotImplemented

exception NotFound

let rec unfold (f: 'seed -> ('a * 'seed)) (stop : 'b -> bool) (b : 'seed) : 'a list =
  if stop b then []
  else
  	let x, b' = f b in
    x :: (unfold f stop b')
;;

type 'a tree = Node of 'a * ('a tree) list


(*List and Tree Examples*)
let exampleList = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;
let exampleTree = Node (1, [ Node (2, [])
						   						 ; Node (3, [ Node (4, [])
	                       			  			; Node (5, [])
	                   				  				; Node (6, [ Node (7, [])
	                   				  			 						 ; Node (8, [])
	                   				  			 						 ; Node (9, [])
	                   				  									 ; Node (10, [])
	                   				  			 						 ])
	                   				  				])
	                      	]);;

(*
UNFOLD EXAMPLE 1: 
nats generates a list of natural numbers up to (and including) an inputted maximum integer
*)
let nats max = 
	unfold 
		(fun b -> b, b + 1)
		(fun x -> x > max)
		0
;;
(*
UNFOLD EXAMPLE 2: 
nats generates a list of even numbers up to (and including) an inputted maximum integer
*)
let evens max =	
	unfold 
		(fun b -> (if (b mod 2 = 0) then (b) else (b+1)), b + 2) 
		(fun x -> x > max) 
		0
;;