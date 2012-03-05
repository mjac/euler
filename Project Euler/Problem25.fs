module Euler.Problems.Problem25

let rec fibTerm f n i j =
    if f i
        then n,i
        else fibTerm f (n+1) (i+j) i

let fibMatch f = fibTerm f 1 1I 0I

let fibDigit d =
    let dFloat = float(d-1) in
    let c,n = fibMatch (fun b -> bigint.Log10(b) >= dFloat) in
    c

(*
Fibonacci terms converge to (n)*Phi=(n+1), where Phi is the
Golden Ratio (1+sqrt5)/2.
I reasoned that there is an nth term that is smaller than 10^999
with the corresponding nth+1 term bigger than 10^999.
So, using the binary splitting method for searching, I used
the MS calculator and found Phi^4780<10^999 and Phi^4781>10^999.
Since the two initial terms of the series have the same value
by definition, you have to add one to the exponents found.
No code necessary.
Rudy. 
*)