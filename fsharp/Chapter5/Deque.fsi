module Chapter5.Deque

type 'a Deque = 'a list * 'a list

val empty: 'a Deque
val isEmpty: 'a Deque -> bool

val cons: 'a -> 'a Deque -> 'a Deque
val head: 'a Deque-> 'a
val tail: 'a Deque -> 'a Deque

val snoc: 'a Deque -> 'a -> 'a Deque
val last: 'a Deque -> 'a
val init: 'a Deque -> 'a Deque