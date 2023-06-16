module Chapter5.SplayHeap

type 'a Elem = 'a

type 'a Tree =
    | E
    | T of 'a Tree * 'a Elem * 'a Tree

let rec bigger pivot = function
    | E -> E
    | T (a, x, b) ->
        if x <= pivot then bigger pivot b
        else
            match a with
            | E -> T (E, x, b)
            | T (a1, y, a2) ->
                if y <= pivot then T (bigger pivot a2, x, b)
                else T (bigger pivot a1, y, T(a2, x, b))

let rec smaller pivot = function
    | E -> E
    | T (a, x, b) ->
        if x > pivot then smaller pivot a
        else
            match b with
            | E -> T (a, x, E)
            | T (a1, y, a2) ->
                if y > pivot then T (a, x, smaller pivot a1)
                else T (T (a, x, a1), y, smaller pivot a2)
    
let insert x t = T (smaller x t, x, bigger x t)