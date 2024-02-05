module Chapter9.SkewBinaryNumbers

(*
skew binary numbers [Mye83, Oka95b], the weight Wi of the zth digit is
2^(i + 1) - 1 than 2^i as in ordinary binary numbers. 
Digits may be zero, one, or two
*)

type Nat = int list

let inc (ls: Nat) = 
    match ls with
    | x :: y :: rest -> 
        if x = y then (1 + x + y) :: rest else 1 :: ls
    | _ -> 1 :: ls

let dec ls = 
    match ls with
    | 1 :: ws -> ws
    | w :: ws -> 
        (w / 2) :: (w / 2) :: ws