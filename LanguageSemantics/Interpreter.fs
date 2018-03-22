module Interpreter

module Return =

    let bind2Integer f = fun x y ->
        match x, y with
        | Value.IntegerInterval(n1, n2), Value.IntegerInterval(n3, n4) when n1 = n2 && n3 = n4 -> f n1 n3
        | Value.Any, _
        | _, Value.Any -> Value.Any
        | _ -> Value.None

    let Integer n = Value.IntegerInterval(n, n)
    let Boolean b = Value.Boolean b

open AST

let operationOfOperator operator =
    match operator with
    | Plus -> fun x y -> Return.Integer (x + y)
    | Minus -> fun x y -> Return.Integer (x - y)
    | Multiply -> fun x y -> Return.Integer (x * y)
    | Divide -> fun x y -> if y = 0 then Value.None else Return.Integer(x / y)
    | Equals -> fun x y -> Return.Boolean(x = y)
    | NotEquals -> fun x y -> Return.Boolean(x <> y)
    | GreaterThan -> fun x y -> Return.Boolean(x > y)
    | GreaterThanOrEquals -> fun x y -> Return.Boolean(x >= y)
    | LessThan -> fun x y -> Return.Boolean(x < y)
    | LessThanOrEquals -> fun x y -> Return.Boolean(x <= y)
    |> Return.bind2Integer

let rec eval environment expression =
    match expression with
    | IntegerLiteral n -> Return.Integer n
    | BinaryExpression(leftExpression, operator, rightExpression) ->
        let leftValue = eval environment leftExpression
        let rightValue = eval environment rightExpression
        let operation = operationOfOperator operator
        operation leftValue rightValue
    | Function(param, body) -> Value.Closure(environment, param, body)
    | FunctionCall(f, argument) ->
        let fRValue = eval environment f
        let argumentRValue = eval environment argument
        match fRValue, argumentRValue with
        | Value.Closure(capturedEnvironment, param, body), argumentValue ->
            let newEnvironment = Map.add param argumentValue capturedEnvironment
            eval newEnvironment body
        | _ -> Value.None
    | Identifier(identifier) ->
        match Map.tryFind identifier environment with
        | Some value -> value
        | None -> Value.Any
    | IfExpr(condition, trueExpr, falseExpr) ->
        let conditionVal = eval environment condition
        match conditionVal with
        | Value.Boolean(true) ->
            eval environment trueExpr
        | Value.Boolean(false) ->
            eval environment falseExpr
        | Value.Any ->
            let trueBranchVal = eval environment trueExpr
            let falseBranchVal = eval environment falseExpr
            Value.Union trueBranchVal falseBranchVal
        | _ -> Value.None