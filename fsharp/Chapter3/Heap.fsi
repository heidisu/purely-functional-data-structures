module Chapter3.Heap

type Elem<'a when 'a : comparison> = 'a

type Heap<'a when 'a : comparison>

exception Empty

module Heap =
    val empty: Heap<'a>
    val isEmpty: Heap<'a> -> bool
    val insert: Elem<'a> -> Heap<'a> -> Heap<'a>
    val merge: Heap<'a> -> Heap<'a> -> Heap<'a>
    