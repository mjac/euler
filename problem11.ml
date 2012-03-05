open Big_int;;

let rec sumDig n t =
	let bigint10 = big_int_of_int 10 in
	let (q,r) = quomod_big_int n bigint10 in
	let tNew = t + int_of_big_int r in
		if gt_big_int q zero_big_int
		then sumDig q tNew
		else tNew;;

let num = (power_int_positive_int 2 1000);;

(*print_endline (string_of_big_int num);;*)
print_int (sumDig num 0);;
