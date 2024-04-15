open Chapter10.CatenableList
open FSharpx.Collections
open Chapter10.Trie

let xl = cons 'a' (cons 'b' (cons 'c' Chapter10.CatenableList.E))

let q = LazyList.cons (C ('a', LazyList.empty)) (LazyList.cons (C ('b', LazyList.empty)) (LazyList.cons (C('c', LazyList.empty)) (LazyList.cons (C('d', LazyList.empty)) LazyList.empty)))
let xc = C('x', q)
let t = tail xc

printfn "%A" xc
printfn "%A" t

let t0 = empty
let t1 = bind ("cat" |> Seq.toList) 1 t0
let t2 = bind ("dog" |> Seq.toList) 2 t1
let t3  = bind ("car" |> Seq.toList) 3 t2
let t4 = bind ("cart" |> Seq.toList) 4 t3

printfn "%A" t4

let l1 = lookup ("cat" |> Seq.toList) t4
printfn "%A" l1