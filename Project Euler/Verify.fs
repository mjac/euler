module Euler.Verify

let timeAnswer f =
    let a = System.DateTime.Now
    let w = f ()
    let b = System.DateTime.Now
    (w, (b - a).TotalMilliseconds)

let checkAnswer f v =
    let a,t = timeAnswer f
    let correct = a = v
    (if correct then printf "Correct %A=%A" else printf "Incorrect %A!=%A") a v
    printfn " in %.0f ms" t
