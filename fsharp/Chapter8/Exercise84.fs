module Chapter8.Exercise84

open Chapter8.Queue

type 'a Queue84 = 'a list * 'a Queue

let empty = ([], empty)
let isEmpty (l, q) = List.isEmpty l && isEmpty q

let cons (a: 'a) ((l, q): 'a Queue84): 'a Queue84 =
    (a :: l, q)
let head ((l, q): 'a Queue84): 'a =
    match l with
    | [] -> head q
    | x :: _ -> x
let tail ((l, q): 'a Queue84): 'a Queue84 =
    match l with
    | [] -> ([], tail q) 
    | _ :: xs -> (xs, q)
    
let snoc a (l, q) = (l, snoc a q)
let last (l, q) =
    if Queue.isEmpty q then List.last l else last q
    
let init (l, q) = (l, init q)


    