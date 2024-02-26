module Chapter10.PolymorphicTest

type 'a Seq = Nil | Cons of 'a * ('a * 'a) Seq

(*
let rec size (s: 'a Seq): int =
    match s with
    | Nil -> 0
    | Cons (a, ps) -> 1 + 2 * size ps
*)

type 'a EP = Elem of 'a | Pair of 'a EP * 'a EP
type 'a Seq' = Nil' | Cons' of 'a EP * 'a Seq'

let rec size (s: 'a Seq'): int =
    match s with
    | Nil' -> 0
    | Cons' (x, ps) -> 1 + 2 * size ps