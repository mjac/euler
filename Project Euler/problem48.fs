module problem48

printfn "Problem 48"

(*
dodge
let rec modularPow a n t X =
    if n = 0I then 1I
    elif n = 1I then t
    else if bigint.op_BitwiseAnd(n, 1I) = 0I then modularPow a (bigint.Divide(n, 2I)) (bigint.Remainder(bigint.Multiply(t, t), X)) X else modularPow a (n-1I) (bigint.Remainder(bigint.Multiply(a, t), X)) X
  *)  
    (*match n with
    | 0I -> 1I
    | 1I -> t
    | x -> if x &&& 1L = 0L then modularPow a (n/2L) ((t*t) % X) X else modularPow a (n-1L) ((a*t) % X) X;*)

(List.map (fun (n:int) -> bigint.Pow(bigint(n), n)) [1..1000] |> List.sum).ToString() |> printf "%s";
// |> List.fold (fun acc s -> (acc + s) % 10000000000I) 0I).ToString() |> printf "%s";

System.Console.ReadLine() |> ignore
(*Correct but boring*)

(*
class E048():
	T10 = 10000000000L		
	def prod(n as long):
		p = 1L
		for i in range(1,n+1):
			p *= n
			p = p % T10
		return p
	def f(n as long) as long:
		sum = 0L
		for i in range(1,n+1):
			if i%10 != 0:
				sum += prod(i)
				sum = sum % T10
		return sum 
	def Run():
		assert f(1000) == 9110846700L # 52.7 ms

similar to my first
*)