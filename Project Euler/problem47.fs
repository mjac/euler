module problem47

let primeFactors n =
    let rec reduce n c (t::ts) = match n with
        | 0 -> []
        | 1 -> (t::ts)
        | x -> if x % c = 0 then reduce (x/c) c ((t+1)::ts)
               else reduce n (c+1) (if t = 0 then (t::ts) else (0::t::ts)) in reduce n 2 [0]

let ndistinct n t = List.length(primeFactors n) = t

let rec fac a c t = match c with | 0 -> [(a+1)..(a+t)] | x -> if ndistinct a t then fac (a-1) (c-1) t else fac (a+t-c+1) t t

fac 1 4 4 |> printfn "%A"

System.Console.ReadLine() |> ignore

(*

Limit=1000000     # Search under 1 million for now
factors=[0]*Limit # number of prime factors.
count=0
for i in xrange(2,Limit):
    if factors[i]==0:
        # i is prime
        count =0
        val =i
        while val < Limit:
            factors[val] += 1
            val+=i
    elif factors[i] == 4:
        count +=1
        if count == 4:
            print i-3 # First number
            break
    else:
        count = 0

uses memory to improve performance very clever
calculates all factors from the bottom up

My solution goes the other way around: 
After filling an array with the primes below one thousend I set a boolean array up to 1000000 false. 
Every product of four primes and multiples like 2*2*2*3*3*5*7 below 1000000 are set true. 
Then I look for the first 4 trues in sequence. 

Takes 60 ms in Delphi on a P4 2.6 GHz.


top notch

#include <stdio.h>
#include <string.h>
 
#define MAX 1000000
#define NUM 4  /* number of consecutive int */
#define NOF 4  /* number of required distinct prime factors */
 
int main(void) {
  /* an array to hold count of distinct prime factors for each i */
  int num_factors[MAX];
  memset(num_factors, 0, MAX * sizeof(int));
 
  printf("Sieving...\n");
  for (int i = 2; i < MAX; i++) {
    if (num_factors[i] > 0) /* not a prime, skip */
      continue;
    for (int j = 1; i * j < MAX; j++)
      num_factors[i * j]++;
  }
 
  printf("Scanning...\n");
  int left = NUM;
  for (int i = 2; i < MAX; i++) {
    if (num_factors[i] == NOF) {
      if (left == 1) {
        printf("%d\n", i + 1 - NUM);
        return 0;
      } else {
        left--;
      }
    } else {  /* reset consecutive counter */
      left = NUM;
    }
  }
 
  return 0;
}
*)