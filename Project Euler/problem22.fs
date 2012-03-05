module problem22

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

    //read in names

System.Console.ReadLine() |> ignore
// correct