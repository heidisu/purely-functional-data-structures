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

(*
          fun aux (T (x, []), []) = T' (x, E', E')
        | aux (T (x, []), h2::hs2) = T' (x, E', aux (h2, hs2))
        | aux (T (x, h1::hs1), []) = T' (x, aux (h1, hs2), E')
        | aux (T (x, h1::hs1), h2::hs2) = T' (x, aux (h1, hs1), aux (h2, hs2))
*)

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
    