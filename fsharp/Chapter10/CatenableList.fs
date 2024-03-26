module Chapter10.CatenableList

open FSharpx.Collections

module LazyList = 
    let snoc a q =
        LazyList.append q (LazyList.cons a LazyList.empty)

type 'a Cat = E | C of 'a * ('a Cat LazyList)

let head = function
    | E -> failwith "Illegal operation"
    | C (x, _) -> x

let link c (ys: 'a Cat) = 
    match c with
    | C (x, q) -> C(x, LazyList.snoc ys q)

let (++) x y =
    match (x, y) with
    | (E, ys) -> ys
    | (xs, E) -> xs
    | (xs, ys) -> link xs ys

let cons x xs = C(x, LazyList.empty) ++ xs
let snoc xs x = xs ++ C(x, LazyList.empty)

let rec linkAll q = 
    let t = LazyList.head q
    let q' = LazyList.tail q
    if LazyList.isEmpty q' then t else link t (linkAll q')

let tail c = 
    match c with
    | E -> failwith "Illegal operation"
    | C(x, q) -> if LazyList.isEmpty q then E else linkAll q

// oppgave 10.6
let rec flatten (cl: 'a Cat list) = 
    match cl with
    | [] -> E
    | E :: xs -> flatten xs
    | x :: xs -> x ++ flatten xs // O(1) hvis lazy