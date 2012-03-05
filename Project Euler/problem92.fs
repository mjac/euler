module problem92

open Euler.Digit

let digit2Sum n = List.map (fun a -> a*a) (digits n []) |> List.sum;

let digit2MaxSum n = 
    let x::xs = digits n [] in
    let xLen = List.length(x::xs) in
    if (List.filter (fun y -> y = 9) (x::xs)).Length = xLen then 81*xLen else (x-1)*(x-1)+81*(xLen-1);

(*
let digitSum n = digits n [] |> List.sum;

let digitMaxSum n = 
    let x::xs = digits n [] in
    let xLen = List.length(x::xs) in
    if (List.filter (fun y -> y = 9) (x::xs)).Length = xLen then 9*xLen else x-1+9*(xLen-1);
*)

//let digitSum2 n = let x = digitSum n in x * x;

let rec chain n (mem:array<int>) =
    let t = if mem.[n] = 0 then mem.[n] <- chain (digit2Sum n) mem
    in mem.[n]

let amount = 10000000-1;

let maxSum = digit2MaxSum(amount);
let mem = Array.create (max (maxSum*maxSum + 1) 90) 0;
mem.[1] <- 1
mem.[89] <- 89

[1..amount] |> List.filter (fun x -> chain (digit2Sum x) mem = 89) |> List.length |> printfn "%i";

System.Console.ReadLine() |> ignore