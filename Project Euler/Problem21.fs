module Euler.Problems.Problem21

let isqrt n = int(sqrt(float n))

let sumProperDivisors n = [1..n-1] |> List.filter (fun x -> n % x = 0) |> List.sum

let amicable n limit = let x = sumProperDivisors n in x <> n && x < limit && sumProperDivisors x = n

let amicablePairSum n = [1..n-1] |> List.filter (fun x -> amicable x (n-1)) |> List.sum

(*
My code runs @ 77Hz (13ms). The hardest thing was probably to understand the question, turned out I was almost right, and after a few mods, the right answer popped up.

Dim I As Long, J As Long
Dim Factors(1 To 9999) As Long
Dim Ans As Long
Dim Found As Boolean

For I = 1 To 9999
For J = I * 2 To 9999 Step I
Factors(J) = Factors(J) + I
Next
Next

For I = 2 To 9999
If Factors(I) < 10000 and factors(I) > I Then
If Factors(Factors(I)) = I Then Ans = Ans + I + Factors(I)
End If
Next
*)