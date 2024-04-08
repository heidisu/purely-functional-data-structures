module LazyBinomialHeap

open FSharpx.Collections

type 'a Bheap = E | H of 'a * 'a Heap
and 'a Tree = Node of int * 'a Bheap * 'a Tree list
and 'a Heap = 'a Tree LazyList

let leq (H(x, _)) (H(y, _)) = x <= y

let empty = LazyList.empty
let isEmpty h = LazyList.isEmpty h

let rank (Node(r, x, c)) = r

let root (Node(r, x, c)) = x

let link t1 t2 = 
    match (t1, t2) with 
    | Node(r, x1, c1), Node (_, x2, c2) ->
        if leq x1 x2 then Node(r + 1, x1, t2 :: c1) else Node (r + 1, x2, t1 :: c2)

let rec insTree t ts = 
    match ts with
    | LazyList.Nil -> LazyList.cons t LazyList.empty
    | LazyList.Cons(x, xs) -> 
        if rank t < rank x then LazyList.cons t ts else insTree (link t x) xs

let rec mrg ts1 ts2 = 
    match ts1, ts2 with
    | t, LazyList.Nil -> t
    | LazyList.Nil, t -> t
    | LazyList.Cons(t1, ts1'), LazyList.Cons(t2, ts2') ->
        if rank t1 < rank t2 then LazyList.cons t1 (mrg ts1' ts2)
        else if rank t2 < rank t1 then LazyList.cons t2 (mrg ts1 ts2')
        else insTree (link t1 t2) (mrg ts1' ts2') 

let insert x ts = insTree (Node (0, x, [])) ts
let merge ts1 ts2 = mrg ts1 ts2

let rec removeMinTree h = 
    match h with
    | LazyList.Nil -> failwith "empty list"
    | LazyList.Cons(t, LazyList.Nil) -> (t, LazyList.empty)
    | LazyList.Cons(t, ts) ->
        let (t', ts') = removeMinTree ts
        if leq (root t) (root t') then (t, ts) else (t', LazyList.cons t ts')

let findMin ts = 
    let (t, _) = removeMinTree ts 
    root t

let deleteMin ts = 
    let (Node(_, x, ts1), ts2) = removeMinTree ts
    mrg (ts1 |> LazyList.ofList |> LazyList.rev) ts2