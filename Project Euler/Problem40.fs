module Euler.Problems.Problem40

open Euler.Digit

let fracDigits part =
    let rec fracDigitsNth n t =
        let d = digits n []
        let dLen = List.length d
        let tIdx = part - t - 1 in 
            if tIdx < dLen then List.nth d tIdx
            else fracDigitsNth (n+1) (t+dLen)
    in fracDigitsNth 1 0

let fracSeries n = [0..n] |>
    List.map (fun a -> fracDigits (pown 10 a)) |>
    List.fold (fun acc a -> acc*a) 1
 