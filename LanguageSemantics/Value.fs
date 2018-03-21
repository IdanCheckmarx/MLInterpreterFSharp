module Value

open Microsoft.FSharp.Collections

type value =
    | IntegerInterval of int * int
    | String of string
    | Closure of environment * string * AST.expression
    | Boolean of bool
    | Any
    | None

and environment = Map<string, value>

let Union val1 val2 =
    match val1, val2 with
    | _, None -> val1
    | None, _ -> val2
    | IntegerInterval(i1, i2), IntegerInterval(j1, j2) -> IntegerInterval(min i1 j1, max i2 j2)
    | String(str1), String(str2) -> if (str1 = str2) then val1 else Any
    | Boolean(b1), Boolean(b2) -> if (b1 = b2) then val1 else Any
    | _ -> Any

let AreOverlapped lowerbound1 upperbound1 lowerbound2 upperbound2 =
    let lowerbound = max lowerbound1 lowerbound2
    let upperbound = min upperbound1 upperbound2
    lowerbound <= upperbound1

let Intersect val1 val2 =
    match val1, val2 with
    | _, Any -> val1
    | Any, _ -> val2
    | IntegerInterval(i1, i2), IntegerInterval(j1, j2) when (AreOverlapped i1 i2 j1 j2) -> IntegerInterval(max i1 j1, min i2 j2)
    | String(str1), String(str2) when (str1 = str2) -> val1
    | Boolean(b1), Boolean(b2) when (b1 = b2) -> val1
    | _ -> None