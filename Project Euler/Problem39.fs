module Euler.Problems.Problem39

(* x^2 + y^2 = z^2 *)
(* x+y+z=n, z>y>x, y>n/3 *)
let zs n = [n/3..n*71/100]
let ys z n = [1..(n-z)/2]
let solutions n = zs n |>
    List.map (fun z -> ys z n |> List.map (fun y -> (n-z-y, y, z)) |>
    List.filter (fun (a,b,c) -> a*a+b*b=c*c)) |> List.concat
    
let maxSols n = [3..n] |> 
    List.map (fun p -> (p, solutions p |> List.length)) |>
    List.fold (fun (m, msol) (p, psol) -> if msol > psol then (m, msol) else (p, psol)) (0, 0)

(*
Faster to generate triples.

Similar to mine

As a starter, based on the relation
a^2+b^2 = c^2 (1)
If both a and b are even, c will also be even and P (the perimeter) will be even.
If both a and b are odd, c will be even and P will be even.
If one is even and the other is odd, c will be odd and P will again be even.
Therefore, only even values of P need to be checked.

Based on the other equation
a+b+c = P (2)
and using the value of c=P-a-b to replace it in equation (1) above, the following is obtained:
a^2+b^2 = (P-a-b)^2 = P^2+a^2+b^2-2*P*a-2*P*b+2*a*b (3)

Simplifying and transposing gives
b = P*(P-2*a)/2(P-a) (4)

For all even values of P, try all values of a with this equation until it exceeds or equals the computed value of b, counting the number of times that the division yields a whole number.

Following is my "brute force" code in assembler, finding the answer within the "blink of an eye" on a P4-1500.

start:
mov numa,3

@@:
mov ecx,perim
sub ecx,numa
mov eax,ecx
shl ecx,1
sub eax,numa
mul perim
div ecx
or edx,edx
.if ZERO?
inc numof
.else
.if eax <= numa
jmp @F
.endif
.endif
inc numa
jmp @B

@@:
mov eax,numof
.if eax > bestnum
mov bestnum,eax
push perim
pop bestp
.endif
mov eax,perim
add eax,2
cmp eax,999
ja endcalc
mov perim,eax

mov numof,0
jmp start

endcalc:
;display result (bestp)

Raymond 












lars,
The solution sounds very interesting. I am curious now.

The forum for problem 75 contains a infinite transform based solution entry from sng which gives all primitives. An associated entry can be found on mathsworld website.
Bishwa.

25 Jan 2005 12:34 pm 
Lars Ulveland   (C/C++)  Lars Ulveland is from Sweden
Ok, the method I use is really just an implementation of
http://mathworld.wolfram.com/PythagoreanTriple.html.

I have put this method to much tougher tests than these, in programming contests, and it has never let me down.

The recursive function calls itself with each of the three transforms and does whatever it needs to to with the triple itself, adding the perimeter size to an array or whatever is nescessary. Those of you who are inclined towards extreme optimization can problably write a faster implementation.

I like program using recursion when the problem is of a recursive nature. The difference between the pythagorean problems here has very little to do with generating them.
*)