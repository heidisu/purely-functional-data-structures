module Chapter2.Exercise_2_3

open Chapter2.Trees

exception SameValue

let rec insertThrow x t= 
    match t with 
    | E -> T (E, x, E)
    | T (a, y, b) ->
        if x < y then T (insertThrow x a, y, b)
        else if x > y then T(a, y, insertThrow x b)
        else raise SameValue
let insert x t =
    try
        insertThrow x t
    with SameValue -> t