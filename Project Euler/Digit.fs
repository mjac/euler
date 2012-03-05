module Euler.Digit

let rec digits n res =
    match n with
        | 0 -> res
        | x -> digits (n/10) ((x%10)::res)

let digitCount n = digits n [] |> List.length