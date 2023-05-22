module Chapter3.Heap

type Elem<'a when 'a: comparison> = 'a

type Heap<'a when 'a: comparison> =
    | E
    | T of int * Elem<'a> * Heap<'a> * Heap<'a>

exception Empty

module Heap =
    let empty = E

    let isEmpty =
        function
        | E -> true
        | _ -> false

    let rank =
        function
        | E -> 0
        | T (r, _, _, _) -> r

    let makeT x a b =
        if rank a >= rank b then
            T(rank b + 1, x, a, b)
        else
            T(rank a + 1, x, b, a)

    let rec merge h1 h2 =
        match (h1, h2) with
        | h1, E -> h1
        | E, h2 -> h2
        | T (_, x, a1, b1), T (_, y, a2, b2) ->
            if x <= y then
                makeT x a1 (merge b1 h2)
            else
                makeT y a2 (merge h1 b2)

    let insert x h = merge (T(1, x, E, E)) h

    let findMin =
        function
        | E -> raise Empty
        | T (_, x, _, _) -> x

    let deleteMin =
        function
        | E -> raise Empty
        | T (_, _, a, b) -> merge a b
        
    let rec exercise_3_2 x h =
        match h with
        | E -> T(1, x, E, E)
        | T(_, y, a, b) ->
            if x <= y then
                makeT x E h
            else
                makeT y a (exercise_3_2 x b)

    let fromList l =
        List.fold (fun s e -> merge (T(1, e, E, E)) s) E l
        