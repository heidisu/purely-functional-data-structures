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



