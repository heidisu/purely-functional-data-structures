module BootstrappedHeap

open LazyBinomialHeap    

let empty = E
let isEmpty = function
    | E -> true
    | _ -> false

let merge h1 h2 = 
    match (h1, h2) with
    | (E, h) -> h
    | (h, E) -> h
    | (H(x, p1), H(y, p2)) ->
        if x <= y then H(x, LazyBinomialHeap.insert h2 p1)
        else H(y, LazyBinomialHeap.insert h1 p2)

let insert x h = merge (H(x, LazyBinomialHeap.empty)) h

let findMin h = 
    match h with
    | E -> failwith "empty"
    | H(x, _) -> x

let deleteMin h = 
    match h with
    | E -> failwith "empty"
    | H(x, p) -> 
        if LazyBinomialHeap.isEmpty p then E
        else 
            let (H(y, p1)) = LazyBinomialHeap.findMin p
            let p2 = LazyBinomialHeap.deleteMin p
            H(y, LazyBinomialHeap.merge p1 p2)