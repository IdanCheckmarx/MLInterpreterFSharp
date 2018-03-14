﻿module Interpreter

module Return =

    type eval_result =
        | Success of Value.value
        | RuntimeError

    let bind2Integer f = fun x y ->
        match x, y with
        | Success (Value.Integer n1), Success (Value.Integer n2) -> f n1 n2
        | _ -> RuntimeError

    let Integer n = Success(Value.Integer n)

open AST

let plus = (fun x y -> Return.Integer (x + y))
let aaa = Return.bind2Integer plus


let operationOfOperator operator =
    match operator with
    | Plus -> fun x y -> Return.Integer (x + y)
    | Minus -> fun x y -> Return.Integer (x - y)
    | Multiply -> fun x y -> Return.Integer (x * y)
    | Divide -> fun x y -> if y = 0 then Return.RuntimeError else Return.Integer (x / y)
    |> Return.bind2Integer

let rec eval environment expression =
    match expression with
    | IntegerLiteral n -> Return.Integer n
    | BinaryExpression(leftExpression, operator, rightExpression) ->
        let leftValue = eval environment leftExpression
        let rightValue = eval environment rightExpression
        let operation = operationOfOperator operator
        operation leftValue rightValue
    | Function(param, body) -> Return.Success (Value.Closure(environment, param, body))
    | FunctionCall(f, argument) ->
        let fRValue = eval environment f
        let argumentRValue = eval environment argument
        match fRValue, argumentRValue with
        | Return.Success (Value.Closure(capturedEnvironment, param, body)), Return.Success argumentValue ->
            let newEnvironment = Map.add param argumentValue capturedEnvironment
            eval newEnvironment body
        | _ -> Return.RuntimeError
    | Identifier(identifier) ->
        match Map.tryFind identifier environment with
        | Some value -> Return.Success value
        | None -> Return.RuntimeError
