module Chapter8.HoodMelvilleQueue_8_3

type 'a RotationState =
    | Idle
    | Reversing of int * 'a list * 'a list * 'a list * 'a list
    | Appending of int * 'a list * 'a list
    | Done of 'a list

let startRotation (f, r) = Reversing (0, f, [], r, [])

let exec: 'a RotationState ->  int * 'a RotationState = function
    | Reversing (ok, x ::f, f', y:: r, r') -> (1, Reversing (ok + 1, f, x :: f', r, y :: r'))
    | Reversing (ok, [], f', [y], r') -> (1, Appending (ok, f', y :: r'))
    | Appending (0, f', r') -> (0, Done r')
    | Appending (ok, x :: f', r') -> (1, Appending (ok - 1, f', x :: r'))
    | state -> (0, state)

let invalidate: 'a RotationState ->  'a RotationState = function
    | Reversing (ok, f, f', r, r') -> Reversing (ok - 1, f, f', r, r')
    | Appending (0, f', x :: r') -> Done r'
    | Appending (ok, f', r') -> Appending (ok - 1, f', r')
    | state -> state

let exec' (diff, f, state, r) =
    match exec state with
    | (_, Done newf) -> (diff, newf, Idle, r)
    | (d, ns) -> (diff + d, f, ns, r)

let check (q as (diff, f, state, r)) =
    if diff >= 0 then exec' (exec' q)
    else
        let ns = Reversing (0, f, [], r, [])
        exec' (exec' (0, f, ns, []))

let empty = (0, [], Idle, [])

let isEmpty (diff, f, state, r) =
    match f with
    | [] -> true
    | _ -> false

let snoc (diff, f, state, r) x = check (diff - 1, f, state, x :: r)

let head (diff, x :: f, state, r) = x

let tail (diff, x :: f, state, r) = check (diff - 1, f, invalidate state, r)