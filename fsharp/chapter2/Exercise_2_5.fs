module Chapter2.Exercise_2_5

open Trees
let rec complete x d = 
    match d with
    | 0 -> E
    | n -> let subtree = complete x (n - 1)
                in T(subtree, 'x', subtree)