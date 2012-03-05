module Euler.Problems.Problem135

(*
maximum for n is M=n^2-n/2-1

find minimum n for M(n) >= d

M-d >= 0

find where M-d = 0

a=1 b=-1/2 c = -1-d

(1/2+sqrt(1/4+4(1-d)))/2

(1+sqrt(1+16(1+d))/4

n^2-(n/2)^2-1=d
3/4 n^2 = d+1



n^2-(n/2)^2-1 = d


3/4n^2 = d+1
n^2 = 4/3(d+1)

(a+2*d)^2-(a+d)^2-a^2=N
a^2+4d^2+4da - a^2-2da-d^2-a^2=N
3d^2-a^2+2da=N
(3d-a)(d+a)=N

3d>a (d+a>0 d>0 a>0) <- search space!

4/3(a+2d)^2=N
*)
let maxSqr n = int(sqrt(float ((4*(1+n))/3)))

let countSolutions start finish amt =
    let maxSqrVal = maxSqr finish
    let s = Array.create (finish + 1) 0
    [start+2..maxSqrVal] |>
    List.collect (fun x -> List.collect (fun y -> [x,y]) [x/2+1..x-1]) |>
    List.map (fun (x,y) -> x,y,2*y-x) |>
    List.map (fun (x,y,z) -> x,y,z,x*x-y*y-z*z) 

    (* |>
    List.filter (fun x -> x >= start && x <= finish) |>
    List.iter (fun x -> s.[x] <- s.[x] + 1)
    s
    (*
    Array.filter (fun x -> x = amt) s |> Array.length*)

    //Array.fold (fun acc x -> if x = amt then acc + 1 else acc) 0 s


    (* |>
    List.filter (fun x -> x = amt) |>
    List.sum*)


let workout n =
    let mem = Array.create (n+1) 0
    2
    *)