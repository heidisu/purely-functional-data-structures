module Chapter5.SplayHeap

open System.Runtime.Intrinsics.X86

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

let rec partition pivot = function
    | E -> (E, E)
    | T(a, x, b) as t ->
        if x <= pivot then
            match b with
            | E -> (t, E)
            | T(b1, y, b2) ->
                if y <= pivot then
                    let small, big = partition pivot b2
                    (T(T(a, x, b1), y, small), big)
                else
                    let small, big = partition pivot b1
                    (T(a, x, small), T(big, y, b2))
        else
            match a with
            | E -> (E, t)
            | T(a1, y, a2) ->
                if y <= pivot then
                    let small, big = partition pivot a2
                    (T(a1, y, small), T(big, x, b))
                else
                    let small, big = partition pivot a1
                    (small, T(big, y, T(a2, x, b)))

let rec findMin = function
    | E -> failwith "Can't find min of empty tree"
    | T(E, x, b) -> x
    | T(a, x, b) -> findMin a

let rec deleteMin = function
    | E -> failwith "Can't delete min of empty tree"
    | T(E, x, b) -> b
    | T(T(E, x, b), y, c) -> T(b, y, c)
    | T(T(a, x, b), y, c) -> T(deleteMin a, x, T(b, y, c))