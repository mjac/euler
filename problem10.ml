open Big_int;;
open Set;;

module IntSet = Set.Make(struct type t = int let compare = compare end);;

let primeMem n s =
	if IntSet.for_all (fun c -> n mod c <> 0) !s
		then (s := IntSet.add n !s; true)
	else false;;


let primeCounter n =
	let 
		s = ref IntSet.empty in
	let rec checker pSum p =
			if p > n then pSum else 
			if primeMem p s then
				checker (add_big_int pSum (big_int_of_int p)) (p+1)
			else checker pSum (p+1)
	in
		checker zero_big_int 2;;

print_endline (string_of_big_int (primeCounter 2000000));;


(*(IntSet.exists (( = ) (big_int_of_int n)) !s)*)
(*
let prime n =
	let rec modCheck c =
		if c >= n then true
		else if n mod c = 0 then false
		else modCheck (c+1)
	in
		modCheck 2;;
		
*)