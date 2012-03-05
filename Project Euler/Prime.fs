module Euler.Prime

let rec upFrom i =
    seq { 
        yield i
        yield! upFrom (i+1)
    }

let rec primes =
    seq { 
        yield 2
        yield!
            upFrom 3 |>
            Seq.filter (fun p -> primes |> Seq.takeWhile (fun j -> j*j <= p) |> Seq.forall (fun j -> p % j <> 0))
    } |> Seq.cache

let rec primeList n = Seq.takeWhile (fun d -> d <= n) primes
let isPrime n = Seq.pick (fun d -> if d >= n then Some(d) else None) primes = n


//let primeSieve n = if n <= memMax then mem else 

//let isPrime p = if n <= memMax then mem.[n] else let s = primeSieve p in mem.[p]