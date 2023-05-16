module Chapter2.Exercise_2_5

open Trees
let rec complete x d = 
    match d with
    | 0 -> E
    | n -> let subtree = complete x (n - 1)
                in T(subtree, x, subtree)

let create2 x m =
    T (complete x m, x, complete x (m + 1))

let rec create x m =
    match m with
    | 0 -> E
    | m ->
        let half = m / 2
        if m % 2 = 0 then
            let t = complete x half in T(t, x, t)
        else create2 x half