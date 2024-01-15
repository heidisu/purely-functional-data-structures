module Chapter8.Queue

type 'a Queue = 'a list

val empty: 'a Queue
val isEmpty: 'a Queue -> bool

val cons: 'a -> 'a Queue -> 'a Queue
val head: 'a Queue -> 'a
val tail: 'a Queue -> 'a Queue

val snoc: 'a -> 'a Queue -> 'a Queue
val last: 'a Queue -> 'a
val init: 'a Queue -> 'a Queue
