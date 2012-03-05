module problem112

let rec digits n res =
    match n with
        | 0 -> res
        | x -> digits (n/10) ((x%10)::res)

let pLimit = 99

let monotonic f pairs = Seq.forall (fun (a,b) -> f a b) pairs

let bouncy n =
    if n < 100 then false
    else let pairs = Seq.pairwise (Seq.ofList (digits n []))
         not(monotonic (<=) pairs || monotonic (>=) pairs)

let rec propBounce b n =
    //printfn "%i %i" b n
    if ((100*b)/n = pLimit) then n
    else propBounce (if bouncy (n+1) then b+1 else b) (n+1)


propBounce 0 1 |> printfn "%i"


System.Console.ReadLine()
