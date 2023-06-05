module Chapter2.Exercise_2_5

open Trees
let rec complete x d = 
    match d with
    | 0 -> E
    | n -> let subtree = complete x (n - 1)
                in T(subtree, x, subtree)

let create2 x m =
    let vt = complete x m
    let ht = T(vt, x, vt)
    T (vt, x, ht)

let rec create x m =
    match m with
    | 0 -> E
    | m ->
        let half = m / 2
        if m % 2 = 0 then
            let t = complete x half in T(t, x, t)
        else create2 x half