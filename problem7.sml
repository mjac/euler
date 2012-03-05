fun prime n =
	let fun modCheck c =
		if (c >= n) then true
		else if (n mod c = 0) then false
		else modCheck (c+1)
	in
		modCheck 2
	end;


fun primeFinder n =
	let fun checker c p =
		if (prime p) then (if (n = c) then p else checker (c+1) (p+1))
		else checker c (p+1)
	in
		checker 1 2
	end;

primeFinder 1;
