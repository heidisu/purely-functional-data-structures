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
        if x >= pivot then smaller pivot a
        else
            match b with
            | E -> T (a, x, E)
            | T (a1, y, a2) ->
                if y >= pivot then T (a, x, smaller pivot a1)
                else T (T (a, x, a1), y, smaller pivot a2)
    
// let insert x t = T (smaller x t, x, bigger x t)

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

let insert x t =
    let a, b = partition x t
    T (a, x, b)

let rec findMin = function
    | E -> failwith "Can't find min of empty tree"
    | T(E, x, b) -> x
    | T(a, x, b) -> findMin a

let rec deleteMin = function
    | E -> failwith "Can't delete min of empty tree"
    | T(E, x, b) -> b
    | T(T(E, x, b), y, c) -> T(b, y, c)
    | T(T(a, x, b), y, c) -> T(deleteMin a, x, T(b, y, c))
    
// Oppgave 5.7
let rec toList (tree: 'a Elem Tree) =
    printfn $"toList {tree}"
    match tree with
    | E -> List.empty
    | T (E, x, E) -> [x]
    | T (E, x, b) -> x :: toList b
    | T (a, x, E) -> toList a @ [x]
    | T (a, x, b) -> toList a @ [x] @ toList b


//let rec toList' (acc: 'a Elem list) (tree: 'a Elem Tree) =
  //   printfn $"toList {tree}"
   //  match tree with
   //  | E -> acc
   //  | T (a, x, b) -> toList (x :: toList acc a) b

let sorted (lst: 'a list) =
    lst
    |> List.fold (fun t elem -> insert elem t) E
    |> toList

// Sorted med sortert liste inn
// [1; 2; 3; 4]
// insert 1 E - partition 1 E - (E,  1,  E)
// insert 2 (E, 1, E) -> 1 <= 2 og  b = E -> partion = ((E, 1, E), 2, E)
// insert 3 ((E, 1, E), 2, E) -> 2 <= 3 og b = E -> partion = (((E, 1, E), 2, E), 3, E)
// etc...

// Sorted med sortert liste inn
// [4; 3; 2; 1]
// insert 4 E - partition 4 E - (E,  4,  E)
// insert 3 (E, 4, E) -> 4 > 3 og  a = E -> partion = (E, 3, (E, 4, E))
// insert 2 (E, 3, (E, 4, E)) -> 3 > 2 og a = E -> partion = (E, 2, (E, 3, (E, 4, E)))
// etc...

// fold insert -> O(n)
// toList -> O(n)
// = O(2n) = O(n)