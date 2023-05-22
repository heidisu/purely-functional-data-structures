module Chapter2.Exercise_2_2

open Chapter2.Trees
open Trees.Tree
let rec twoWaySearch x candidate (t: 'a Tree) =
    match t with
    | E -> Some x = candidate
    | T (a, y, b) ->
        if x < y then twoWaySearch x candidate a
        else twoWaySearch x (Some y) b

let betterMember x (t: 'a Tree) = 
    twoWaySearch x None t