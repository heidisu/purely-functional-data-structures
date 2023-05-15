module Chapter2.Exercise_2_1

let rec suffix (lst: 'a list) = 
    match lst with
    | [] -> [[]]
    | x :: xs -> lst :: suffix xs