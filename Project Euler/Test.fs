module Euler.Test

open Euler.Problems

//List.fold (fun acc b -> List.iter (fun a -> Set.add (pown a b) acc |> ignore) [2..100]; acc) Set.empty [2..100] |> printfn "%A"

//List.fold (fun acc x -> (List.map (fun y -> x,y) [2..100]) @ acc) [] [2..100] |> Set.ofList |> printfn "%A"

// no need for state, just ignore from the bottom!

let aMax = 100
let bMax = 100

let normalise a b = if b = 1 then a,b else let m = b in pown a m, b / m

let maximise a b = 
    let rec proc n m =
        let a2 = pown a m
        let b2rem = b % m 
        if a2 > aMax
            then pown a n, b/n
            else proc (if b2rem = 0 then m else n) (m+1)
    in proc 1 1

maximise 3 6 |> printfn "%A"
maximise 9 3 |> printfn "%A"
maximise 27 2 |> printfn "%A"

exception LogZero

// Decompose a^b into (x^y)^b if x is the only prime factor, otherwise return none.
let decompose a b =
    let rec minFac n = if n > a/2 then 0 else (if a % n = 0 then n else minFac (n+1))
    let rec ilog m n c = match n with | 0 -> c | 1 -> c | x -> if n%m = 0 then ilog m (n / m) (c+1) else 0
    let x = minFac 2
    in if x = 0
        then Some(a,b)
        else let y = ilog x a 0 in if y=0 then Some(a,b) else Some(x, y * b)
        
decompose 8 10 |> printfn "%A"

List.fold (fun (acc:Set<int * int>) x -> (List.fold (fun acc2 y -> match decompose x y with | None -> acc2 | Some(x) -> acc2.Add x) acc [2..100])) Set.empty [2..100] |> Set.iter (fun x -> x |> printfn "%A")

(* INCORRECT DECOMPOSITION .... because 100-> 2*2*5*5 instead of 10*10 
for each factor, determine if it is a candidate for x 
do not use minFac*)

// decompose a into c^n
(*let maxPower a b =
    let rec proc leftover n c =
        if leftover % n = 0
            then 2
            else proc leftover (n+1) c
    in proc a 2 0*)
    // None Some etc
// nice but not correct... have to reduce down instead

//if b = 1 then a,b (* Cannot be simplified more *) (* maximise instead *)


//group values then identify number of unique groups
//disjoint set data structure

// Set.partition


(*
remove redundancy in notation
decompose in prime factors


add a^b eliminate

2^4
4^2

3^4
9^2
81^1

3^6
9^3 - (3^2)^3
3^6
27^2 - (3^3)^2

2^48
4^24
(4^2)^12

a^b
(a^2)^(b/2)



*)

//List.map (fun a -> List.map (fun b -> a,b) [2..100]) [2..100] |> List.map (fun (a,b) -> pown a b)

//Problem31.changeCount 200 [200;100;50;20;10;5;2;1] |> printfn "%A"
//Problem135.countSolutions 1 (100-1) 10 |> printfn "%A"

//Problem21.amicable 220 9999 |> printfn "%b"
//Problem21.test () |> printfn "%A"

System.Console.ReadLine() |>ignore

(*
I used some advanced (for me) number theory to solve this one.
Best problem ever! Taught me a lot. Used mathematica,
and a book on Champernowne's Constant. :)
*)