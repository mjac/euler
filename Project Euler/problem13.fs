module problem13

open System.IO
open System.Text

let fileLines fileName =
    seq { use reader = new StreamReader(File.OpenRead(fileName))
          while not reader.EndOfStream do
              yield reader.ReadLine() }

let query fileName =
    fileLines fileName |>
    Seq.map (fun line -> bigint.Parse line) |>
    Seq.fold (fun a b -> bigint.Add(a, b)) bigint.Zero

(query "input13.txt").ToString().Substring(0, 10) |> printf "%s"

System.Console.ReadLine() |> ignore
// correct