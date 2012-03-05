
(*http://www.nuprl.org/Algorithms/03cucs-intsqrt.pdf *)

fun sqrt n = if n=0 then 0
else let val r = sqrt (n div 4)
val s = (2*r+1);
in
if n < s*s then (s-1)
else s
end;

exception Fail;
fun triplet 0 _ = raise Fail
  | triplet a b = 
	let
		val c2 = a*a+b*b
		val c = sqrt(c2)
	in
		if c*c = c2 andalso a+b+c = 1000 then (a*b*c, a, b, c)
		else if b = a then triplet (a-1) 1 else triplet a (b+1)
	end;
	
triplet 1000 1;