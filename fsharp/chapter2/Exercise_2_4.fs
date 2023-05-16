module Chapter2.Exercise_2_4

open Chapter2.Trees

exception SameValue

let rec insertThrow x candidate t = 
    match t with 
    | E -> if Some x = candidate then raise SameValue else T (E, x, E)
    | T (a, y, b) ->
        if x < y then T (insertThrow x candidate a, y, b)
        else T(a, y, insertThrow x (Some y) b)

let insert x t =
    try
        insertThrow x None t
    with SameValue -> t
