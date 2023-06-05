module Chapter5.Queue

type Queue<'a when 'a : equality> = 'a list * 'a list

val empty: 'a Queue
val isEmpty: 'a Queue -> bool

val snoc: 'a Queue -> 'a -> 'a Queue
val head: 'a Queue -> 'a
val tail: 'a Queue -> 'a Queue
