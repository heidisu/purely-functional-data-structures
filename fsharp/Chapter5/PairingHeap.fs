module Chapter5.PairingHeap

type 'a Elem = 'a

type 'a Heap = E | T of 'a Elem * 'a Heap List

let empty = E

let isEmpty = function
    | E -> true
    | _ -> false

let merge h1 h2 = 
    match h1, h2 with
    | (h, E) -> h
    | (E, h) -> h
    | (T(x, hs1), T(y, hs2)) ->
        if x <= y then T(x, h2 :: hs1) else T(y, h1 :: hs2)

let insert x h = merge (T(x, [])) h

let rec mergePairs = function
    | [] -> E
    | [h] -> h
    | h1 :: h2 :: hs -> merge (merge h1 h2) (mergePairs hs)

let findMin = function
    | E -> raise <| failwith "Cannot find min of empty"
    | T (x, hs) -> x

let deleteMin = function
    | E -> raise <| failwith "Cannot delete min of emtpy"
    | T (x, hs) -> mergePairs hs

type 'a BinTree = E' | T' of 'a Elem * 'a BinTree * 'a BinTree

let rec toBinaryAcc ph lst = 
    match ph, lst with
    | E, _ -> E'
    | T (x, []), [] -> T' (x, E', E')
    | T (x, []), y :: ys -> T' (x, E', toBinaryAcc y ys)
    | T (x, h :: hs), [] -> T' (x, toBinaryAcc h hs, E')
    | T (x, h :: hs), y :: ys -> T' (x, toBinaryAcc h hs, toBinaryAcc y ys)
 
let toBinary ph = toBinaryAcc ph []

(*
let toBinary' = function
    | E -> E'
    | T (x, []) -> T'(x, E', E')
    | T (x, y :: ys) -> T'(x, )

*)

let empty' = E'
let isEmtpy' = function
    | E' -> true
    | _ -> false

let merge' t1 t2 = 
    match t1, t2 with
    | t, E' -> t
    | E', t -> t
    | T' (x, a1, E'), T' (y, a2, E') ->
        if x <= y then T' (x, T' (y, a2, a1), E')
        else T' (y, T' (x, a1, a2), E')
    | _ -> raise <| failwith "not possile"
  
  
let  insert' x h = merge' (T' (x, E', E')) h

let rec mergePairs' = function
| E' -> E'
| T' (x, a, E')  as t ->  t
| T' (x, a, T' (y, b, c)) ->
    merge' (merge' (T' (x, a, E')) (T' (y, b, E'))) (mergePairs' c)

let findMin' = function
    | E' -> raise <| failwith "cannot find min of empty"
    | T' (x, a, E') -> x

let deleteMin' E' = function
    | E' -> raise <| failwith "cannot delete min of empty"
    | T' (x, a, E') -> mergePairs' a
