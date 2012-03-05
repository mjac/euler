
let rec
	let my_hash = Hashtbl.create 2000000;;




(* Interesting but no... *)
let rec depth m n d = 
	if n > m then d
	else
		if ((n-1) mod 3 = 0) && n <> 1
		then depth m (max (depth m (n*2) (d+1)) (depth m ((n-1)/3) (d+1))) (d+1)
		else (depth m (n*2) (d+1));;


print_int (depth 10 1 0);;

(* Use memoization *)
(*# let m = Hashtbl.create 1;;
val m : ('_a, '_b) Hashtbl.t = 
# Hashtbl.add m [1; 2; 3] "abc";
  Hashtbl.add m [1; 2; 4] "abd";
  Hashtbl.add m [2; 3; 4] "bcd";;
- : unit = ()
# Hashtbl.find m [1; 2; 4];;
- : string = "abd"
*)