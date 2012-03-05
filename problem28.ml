fun spiralCount n =
	let	fun spiralProc c i sum =
		if i > n then sum
		else spiralProc (c+4*(i-1)) (i+2) (4*c+10*i-10+sum)
	in 
		spiralProc 1 3 1
	end;
	
	
	(*
	I have solved this question in a way similar to eulers. I have considered diagonals seperately as series.
Upper right dg. is n2 lower left is n2-1 so lower left to upper right is n2 + (n-1)/2. Other diagonal is more interesting. starting with
3 = 1^2 + 1 + 1
7 = 2^2 + 2 + 1
13 = 3^2 + 3 + 1
so on so forth, thus n2 + n + 1 since we started from 3, this sum-of-series will have 1000 terms.

S = Sum(n*n + (n-1)/2; 1 to 1001 incl.) + Sum(n*n+n+1; 1 to 1000).

and sum(n*n; 1 to k) = k(k+1)(2k+1)/6
I think pencil & paper is good. :)*)
	
(*
a=i+c-1

b=a+c-1
...


sum of i= odd numbers from 4*c+10*i-10


c+4i-4 is next

4*c+10i-14

fun spiralCount 1 sum = sum + 1
  | spiralCount d sum = spiralCount (d-1)

fun spiralDiag n = spiralCount ((n-1) div 2) 0;
*)

spiralCount 5;
(*
1
8
16
24


1
+2
3 5 7 9
+4
13 17 21 25  
+8

*)
