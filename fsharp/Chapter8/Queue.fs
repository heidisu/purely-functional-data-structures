module Chapter8.Queue

type 'a Queue = 'a list

let empty: 'a Queue = []
let isEmpty (a: 'a Queue): bool = List.isEmpty a

let cons (a: 'a) (q: 'a Queue): 'a Queue = a :: q
let head (q: 'a Queue): 'a = List.head q
let tail (q: 'a Queue): 'a Queue = List.tail q

let snoc (a: 'a) (q: 'a Queue): 'a Queue = q @ [a]
let last (q: 'a Queue): 'a = List.last q
let init (q: 'a Queue): 'a Queue = List.take (List.length q - 1) q