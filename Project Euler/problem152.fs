module Euler.Problems.Problem152

(*
There are several ways to write the number 1/2 as a sum of inverse squares using distinct integers.

For instance, the numbers {2,3,4,5,7,12,15,20,28,35} can be used:

In fact, only using integers between 2 and 45 inclusive, there are exactly three ways to do it, the remaining two being: {2,3,4,6,7,9,10,20,28,35,36,45} and {2,3,4,6,7,9,12,15,28,30,35,36,45}.

How many ways are there to write the number 1/2 as a sum of inverse squares using distinct integers between 2 and 80 inclusive?
*)

let nLimit = 80
//List.map (fun a -> 1/(float(a*a))) [2..nLimit]

// 2*uvw = u+v+w   1/2>=(u+v+w)/uvw  2<=x  1/x<=2

// 1/2 = 1/a+..+1/b -> 1/2 = a+b/ab  ca+ab+bc/abc 