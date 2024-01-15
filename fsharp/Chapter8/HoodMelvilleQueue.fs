module Chapter8.HoodMelvilleQueue

type 'a RotationState =
    | Idle
    | Reversing of int * 'a list * 'a list * 'a list * 'a list
    | Appending of int * 'a list * 'a list
    | Done of 'a list

let startRotation (f, r) = Reversing (0, f, [], r, [])

let exec: 'a RotationState ->  'a RotationState = function
    | Reversing (ok, x ::f, f', y:: r, r') -> Reversing (ok + 1, f, x :: f', r, y :: r')
    | Reversing (ok, [], f', [y], r') -> Appending (ok, f', y :: r')
    | Appending (0, f', r') -> Done r'
    | Appending (ok, x :: f', r') -> Appending (ok - 1, f', x :: r')
    | state -> state

let invalidate: 'a RotationState ->  'a RotationState = function
    | Reversing (ok, f, f', r, r') -> Reversing (ok - 1, f, f', r, r')
    | Appending (0, f', x :: r') -> Done r'
    | Appending (ok, f', r') -> Appending (ok - 1, f', r')
    | state -> state

let exec2 (lenf, f, state, lenr, r) =
    let exec1 = exec state
    let exec2 = exec exec1
    printfn $"Exec1: %A{exec1}\tExec2: %A{exec2}"
    match exec2 with
    | Done nl -> (lenf, nl, Idle, lenr, r)
    | ns -> (lenf, f, ns, lenr, r)

let check (q as (lenf, f, state, lenr, r)) =
    if lenr <= lenf then exec2 q
    else
        let ns = Reversing (0, f, [], r, [])
        exec2 (lenf + lenr, f, ns, 0, [])

let empty = (0, [], Idle, 0, [])

let isEmpty (lenf, f, state, lenr, r) = (lenf = 0)

let snoc (lenf, f, state, lenr, r) x = check (lenf, f, state, lenr + 1, x :: r)

let head (lenf, x :: f, state, lenr, r) = x

let tail (lenf, x :: f, state, lenr, r) = check (lenf - 1, f, invalidate state, lenr, r)