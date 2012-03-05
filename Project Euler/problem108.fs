module problem108

let primeFactors n =
    let rec reduce n c (t::ts) = match n with
        | 0 -> []
        | 1 -> (t::ts)
        | x -> if x % c = 0 then reduce (x/c) c ((t+1)::ts)
               else reduce n (c+1) (if t = 0 then (t::ts) else (0::t::ts)) in reduce n 2 [0]

let divisors n = primeFactors n |> List.fold (fun a m -> a * (m+1)) 1

let rec divMatch c divCount = if divisors c > divCount then c else divMatch (c+1) divCount

divMatch 1 100 |> printfn "%i"

System.Console.ReadLine() |> ignore
(*
4(5+20)=5*20     4(6+12)=6*12   4(8+8)=8*8

n(x+y) = xy
nx+ny=xy


Re-write it as (x-n)*(y-n)=n^2 (keep in mind that x, y are not allowed to
be zero), and take it from there.

Cheers, ZVK(Slavek).

unique solutions to (x-n)*(y-n)=n^2
x>0 y>0  x,y>n

n=4 n^2=16 divisors=1,2,4,8,16; +4 = 5,6,8,12,20   note 8 twice as square

(div(n^2) + 1)/2 > 1000

1999 < div(n^2)

since n^2 includes factor once not twice

1000 < div(n)


divisors n!!!!!
...

1000=2^3*5^3

2^2*3^2
*)
