module problem18

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

let rec condense ys result =
    match ys with
    | x1::x2::xs -> condense (x2::xs) ((max x1 x2)::result)
    | _ -> List.rev result;


let rec merge xs ms =
    match xs with
    | x::xs -> merge xs (List.map2 (+) x (condense ms []))
    | [] -> ms.Head;

let result input = merge input (0::(List.map (fun a -> 0) input.Head));

printf "%i" (result (query "input18.txt"));

System.Console.ReadLine() |> ignore
// correct