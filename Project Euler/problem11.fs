module problem11

open System.IO
open System.Text
 
let fileLines fileName =
    seq { use reader = new StreamReader(File.OpenRead(fileName))
          while not reader.EndOfStream do
              yield reader.ReadLine() }

let query fileName =
    fileLines fileName |>
        Seq.map (fun line -> (line.Split(' ') |> Seq.fold (fun acc num -> int(num)::acc) [])) |>
        Seq.fold (fun xs x -> x::xs) [];

let product4 zs = let rec inner xs rs = match xs with a::b::c::d::ys -> inner (b::c::d::ys) ((a*b*c*d)::rs) | _ -> rs in inner zs []
let max4 zs = List.max (product4 zs)
let max42d zs = zs |> Seq.cast<int> |> Seq.toList |> max4

let rearrange (f:int->int->int) (g:int->int->int) (zs:int [,]) = let rs:int [,] = Array2D.create 60 60 0 in let _ = Array2D.iteri (fun x y v -> rs.SetValue(v, f x y, g x y)) zs in rs

let initial = array2D(query "input11.txt")

initial |> max42d |> printfn "%i" //printfn "%A"
initial |> rearrange (fun x y -> y) (fun x y -> x) |> max42d |> printfn "%i" //printfn "%A"
initial |> rearrange (fun x y -> x+y) (fun x y -> x+y+x) |> max42d |> printfn "%i" //printfn "%A"
initial |> rearrange (fun x y -> x+y+x) (fun x y -> x+y) |> max42d |> printfn "%i" //printfn "%A"

System.Console.ReadLine() |> ignore

(* shorter f#

let arr =
  [|[|08;02;22;97;38;15;00;40;00;75;04;05;07;78;52;12;50;77;91;08|];
...
[|01;70;54;71;83;51;54;69;16;92;33;48;61;43;52;01;89;19;67;48|]|]
 
 let lst = [for i in 0..19 for j in 0..16 -> [for k in 0..3 -> arr.[i].[j + k] ] ] @     // rows
           [for i in 0..19 for j in 0..16 -> [for k in 0..3 -> arr.[j + k].[i] ] ] @     // columns
           [for i in 0..16 for j in 0..16 -> [for k in 0..3 -> arr.[i + k].[j + k] ] ] @ // diagonal (top,left - bottom,right)
           [for i in 3..19 for j in 0..16 -> [for k in 0..3 -> arr.[i - k].[j + k] ] ]   // diagonal (top,right - bottom,left)
 
 lst |> List.map(fun x -> x |> List.fold1_left( * ) ) |> List.fold1_left(fun a x -> Math.Max(a,x)) |> print_any
 *)