module Chapter5.Deque

type 'a Deque = 'a list * 'a list

let empty: 'a Deque = ([], [])
let isEmpty (q: 'a Deque) =
    match q with
    | [], [] -> true
    | _ -> false

let cons x ((f, r): 'a Deque) =
    match r with
    | [] -> ([ x ], f)
    | r -> (x :: f, r)

let head ((f, r): 'a Deque) =
    match f with
    | [] -> List.head r // antar 1 <= #f V #r <= 1   #f = 0 => #r <= 1 => r = rev r
    | f -> List.head f 

let tail (f, r) =
    match f with
    | [] -> [], []
    | [ x ] ->
        let size = List.length r / 2
        (List.rev (List.skip size r), List.take size r)
    | f -> (List.tail f, r)

let last (f, r) =
    match r with
    | [] -> List.head f
    | _ -> List.head r

let snoc ((f, r): 'a Deque) x =
    match f with
    | [] -> (r, [x])
    | f -> (f, x :: r)

let init (f, r) =
    match r with
    | [] -> [], []
    | [ y ] ->
        let size = List.length f / 2
        (List.take size f, List.rev (List.skip size f))
    | r -> (f, List.tail r)