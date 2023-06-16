module Oppgave5.BinomialHeap

type 'a Elem = 'a
type 'a Tree = Node of int * 'a Elem * 'a Tree list
type 'a Heap = 'a Tree list

let  empty = []
let isEmpty te = List.isEmpty te
let rank (Node (r, x, c)) = r

let root (Node (r, x, c)) = x
let link (h as Node (r, x1, c1)) (t2 as Node (_, x2, c2)) =
    if x1 <= x2 then Node (r+1, x1, t2 :: c1)
    else Node (r+1, x2, h :: c2)

let rec insTree t ts =
    match ts with
    | [] -> [t]
    | x :: xs ->
        if rank t < rank x then t :: ts else insTree (link t x) xs

let insert (x: 'a Elem) (ts: 'a Heap) = insTree (Node (0, x, [])) ts
                           
let rec merge (ts1: 'a Heap) (ts2: 'a Heap) =
    match ts1, ts2 with
    | ts1, [] -> ts1
    | [], ts2 -> ts2
    | x :: xs , y :: ys ->
        if rank x < rank y then x :: merge xs ts2
        else if rank y < rank x then y :: merge ts1 ys
        else insTree (link x y) (merge xs ys)

let rec removeMinTree ts =
    match ts with
    | [] -> failwith "empty"
    | [x] -> (x, [])
    | x :: xs ->
        let (y, ys) = removeMinTree xs
        if root x < root y then (x, xs) else (y, x :: ys)

let findMin ts = let (t, _) = removeMinTree ts in root t

let deleteMin ts =
    let Node (_, x, ts1), ts2 = removeMinTree ts
    merge (List.rev ts1) ts2