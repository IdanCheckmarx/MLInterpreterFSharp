module Interpreter

module Return =

    let bind2Integer f = fun x y ->
        match x, y with
        | Value.IntegerInterval(n1, n2), Value.IntegerInterval(n3, n4) when n1 = n2 && n3 = n4 -> f n1 n3   // currently only handles single-point intervals, may be extended to support other cases
        | Value.Any, _
        | _, Value.Any -> Value.Any
        | _ -> Value.None
        
open AST

let operationOfOperator operator =
    match operator with
    | Plus -> fun x y -> Value.IntegerInterval(x + y, x + y)
    | Minus -> fun x y -> Value.IntegerInterval(x - y, x - y)
    | Multiply -> fun x y -> Value.IntegerInterval(x * y, x * y)
    | Divide -> fun x y -> if y = 0 then Value.None else Value.IntegerInterval(x / y, x / y)
    | Equals -> fun x y -> Value.Boolean(x = y)
    | NotEquals -> fun x y -> Value.Boolean(x <> y)
    | GreaterThan -> fun x y -> Value.Boolean(x > y)
    | GreaterThanOrEquals -> fun x y -> Value.Boolean(x >= y)
    | LessThan -> fun x y -> Value.Boolean(x < y)
    | LessThanOrEquals -> fun x y -> Value.Boolean(x <= y)
    |> Return.bind2Integer

let rec eval environment expression =
    match expression with
    | IntegerLiteral n -> Value.IntegerInterval(n, n)   // an interval instead of a single value
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
        | None -> Value.Any // any abstract value instead of a RuntimeError
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