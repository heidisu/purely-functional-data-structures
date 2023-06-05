module Chapter5.Queue

type 'a Queue = 'a list * 'a list

let head: 'a Queue -> 'a = function
    | (x:: _, r) -> x
    | ([], _) -> failwith "No head in empty queue"

let tail = function
    | ([ _ ], r) -> (List.rev r, [])
    | (_ :: f, r) -> (f, r)
    | ([], _) -> failwith "No tail in empty list"
    
let snoc (f, r) x =
    match (f, r) with
    | [], _ -> [x], []
    | (f, r) -> (f, x :: r)

let empty: 'a Queue = ([], [])

let isEmpty ((f, _): 'a Queue) = f = []


