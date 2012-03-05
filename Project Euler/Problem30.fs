module Euler.Problems.Problem30

open Euler.Digit

let range a () =
    let powSum n l = List.map (fun x -> pown x n) l |> List.sum
    let sumTest n = n >= 10 && n = (powSum 5 (digits n []))
    in [1..((pown 9 a) * 2000000)] |> List.filter (fun x -> sumTest x) |> List.sum

// Can cache powers producing a large constant speed up...

(*
Surprisingly there are only three numbers that can be written as the 
sum of fourth powers of their digits:

    1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
    8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
    9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

As 1 = 1^(4) is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of 
fifth powers of their digits
*)
(*
I set 5*9^5 as the upper bound by inductive inference from the 
pattern in the example. It worked anyway.
*)