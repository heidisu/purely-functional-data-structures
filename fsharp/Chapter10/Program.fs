open Chapter10.CatenableList
open FSharpx.Collections

let xl = cons 'a' (cons 'b' (cons 'c' Chapter10.CatenableList.E))

let q = LazyList.cons (C ('a', LazyList.empty)) (LazyList.cons (C ('b', LazyList.empty)) (LazyList.cons (C('c', LazyList.empty)) (LazyList.cons (C('d', LazyList.empty)) LazyList.empty)))
let xc = C('x', q)
let t = tail xc

printfn "%A" xc
printfn "%A" t