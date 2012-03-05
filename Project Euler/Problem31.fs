module Euler.Problems.Problem31

//[200,100,50,20,10,5,2,1]

(* Could be changed by using DP and counting up... *)

let rec changeCount n cs =
    if n < 0 then 0
    elif n = 0 then 1
    else match cs with | x::xs -> changeCount (n-x) (x::xs) + changeCount n xs | [] -> 0

(*
http://www.physicsforums.com/showthread.php?t=222580
1. The problem statement, all variables and given/known data

I need to find the number of ways to make change for a dollar using an even number of coins, using only pennies, nickels, dimes, quarters, and fifty cent pieces.

2. Relevant equations

I found the generating function for the ways of making change for a dollar, 1/(1-x)(1-x^5)(1-x^10)(1-x^25)(1-x^50).

3. The attempt at a solution

I was thinking of changing the x into an x/2 to ensure that each number of coins is even, but I know that is not right. I am not sure how to ensure that there are an even number of coins. I need to find a new generating function, but am not sure what it is. Any help is appreciated. Thanks 
*)

(*
Here's a simpler Java (1.5) version which uses dynamic programming:

[hide code]

public class problem31 {
	final static int TOTAL = 200;
 
	public static void main(String[] args) {
		int[] coins = {1, 2, 5, 10, 20, 50, 100, 200};
		int[] ways = new int[TOTAL + 1];
		ways[0] = 1;
 
		for(int coin: coins)
			for(int j = coin; j <= TOTAL; j++)
				ways[j] += ways[j - coin];
 
		System.out.println("Result: " + ways[TOTAL]);
	}
}
*)

(*
Length[IntegerPartitions[200, All, {1, 2, 5, 10, 20, 50, 100, 200}]]
*)