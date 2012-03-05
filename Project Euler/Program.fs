module Euler.Run

open Euler.Verify
open Euler.Problems

exception MissingSolution

let test n = match n with
    | 2 -> (fun () -> Problem2.sumFibLimited 4000000), 4613732
    | 7 -> (fun () -> Problem7.primeFinder 10001), 104743
    //| 15 -> (fun () -> Problem15.routeCount 20 20), 137846528820L
    | 17 -> (fun () -> Problem17.charAmount 1000), 21124
    | 19 -> (fun () -> Problem19.calcSundays ()), 171
    | 21 -> (fun () -> Problem21.amicablePairSum 10000), 31626
    //| 24 -> (fun () -> Problem24.searchSpace (1000000 - 1) (Set.ofList Problem24.symbols) [], 2783915460L)
    | 25 -> (fun () -> Problem25.fibDigit 1000), 4782
    | 26 -> (fun () -> Problem26.longestCycle 1000), 983
    | 27 -> (fun () -> let a,b,c = Problem27.maxProduct 1000 in a*b), -59231
    | 30 -> (fun () -> 2), 443839
    | 31 -> (fun () -> Problem31.changeCount 200 [200;100;50;20;10;5;2;1]), 73682
    | 39 -> (fun () -> let a,b = Problem39.maxSols 1000 in a), 840
    | _ -> raise MissingSolution

printf "Enter a problem: "

let i = int(System.Console.ReadLine())

try 
    let a,b = test i in
    let x = printfn "Running problem %i" i in
    checkAnswer a b
with
    | :? MissingSolution -> printfn "Missing problem %i" i

System.Console.ReadLine()