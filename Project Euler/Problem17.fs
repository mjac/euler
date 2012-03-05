module Euler.Problems.Problem17

let digitStr = [""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"];
let teenStr = [""; ""; "twen"; "thir"; "four"; "fif"; "six"; "seven"; "eigh"; "nine"];
let tyStr = [""; ""; "twen"; "thir"; "for"; "fif"; "six"; "seven"; "eigh"; "nine"];
let pow3str = [""; "thousand"; "million"; "billion"];

let tens n =
    let tensIdx = n / 10
    let unitIdx = n % 10
    if n < 20 && n >= 10 then 
    match n with
        | 10 -> "ten"
        | 11 -> "eleven"
        | 12 -> "twelve"
        | x -> (List.nth teenStr unitIdx) + "teen" 
    elif n >= 20 then (List.nth tyStr tensIdx) + "ty" + (List.nth digitStr unitIdx) 
    else (List.nth digitStr unitIdx)

let andtens n b = (if b && n <> 0 then "and" else "") + tens n

let hundreds n b =
    let hundredsIdx = n / 100
    let tensIdx = n % 100
    (if hundredsIdx > 0 then (List.nth digitStr hundredsIdx) + "hundred" else "") +
    andtens tensIdx (b || hundredsIdx > 0)

let rec thousandChunks n i =
    let thousandsIdx = n / 1000
    let hundredsIdx = n % 1000
    (if thousandsIdx > 0 then thousandChunks thousandsIdx (i+1) else "") +
    hundreds hundredsIdx (thousandsIdx > 0) +
    (List.nth pow3str i)

let numberStr n = thousandChunks n 0

let charAmount n = [1..n] |> List.map (fun a -> (numberStr a).Length) |> List.sum