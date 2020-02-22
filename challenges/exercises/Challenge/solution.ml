let solution _ = "This is the solution"

let identity x = x
;;

let rec append (l1 : 'a list) (l2 : 'a list) : 'a list = l1 @ l2
;;

let rec reverse (l : 'a list) : 'a list = List.rev l
;;

let rec findNthElement ((l : 'a list), (n : int)) : 'a option = 
	if n >= 0 then
		match l with
		| [] -> None
		| h::t -> if n = 0 then Some h else findNthElement(t, n-1)
	else None
;;

let rec replicateSomeElements ((p : 'a -> bool), (l : 'a list), (k : int)) : 'a list = 
	let rec f n e = 
			if n <= 0 then []
			else e::(f (n-1) e)
	in
	List.fold_left 
		(fun a x -> if p x then a@(f k x) else a@[x])
		[]
		l
;;

let rec enumerateKCombinations ((l : 'a list), (k : int)) : 'a list list =
	if k <= 0 then [[]]
	else 
		begin
			match l with
		     | [] -> []
		     | h :: t ->
		        let with_h = List.map (fun l -> h :: l) (enumerateKCombinations (t, k-1)) in
		        let without_h = enumerateKCombinations (t, k) in
		        with_h @ without_h
		end
;;

let rec countNodes (t : 'a tree) : int = 
	let Node (value, children) = t in
	match children with
		| [] -> 1
		| _ -> List.fold_left (fun acc t' -> acc + countNodes t') 1 children
;;

exception NotFound
let findNode ((p : 'a -> bool), (t : 'a tree)) : 'a option =
	let rec find_e (p : 'a -> bool) (t : 'a tree) : 'a = match t with
	|Node (a, l)-> 
	  if (p a) then a 
	  else match l with 
	  	|[] -> raise NotFound
	  	|Node (a, linside)::l -> try find_e p (Node (a,l)) with NotFound -> find_e p (Node (a,linside))
	in
	try ( Some (find_e p t) ) with NotFound -> None
;;

let fib max = 
	unfold 
		(fun (a,b) -> a, (b, a+b)) 
		(fun (a,b) -> a > max) 
		(1,1)
;;

let pascal max = 
	unfold
		(fun r -> 
			let next = List.map2 (+) ([0] @ r) (r @ [0]) in
			(r, next))
		(fun l -> List.length l >= max)
		[1]
;;