// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open AST
open Interpreter

let expressionOnePlusSeventeen =
    BinaryExpression (IntegerLiteral 1, Plus, IntegerLiteral 17)

let expressionTwoMultipliedBySeventeen =
    BinaryExpression (IntegerLiteral 2, Multiply, IntegerLiteral 17)


(* (fun x -> x + 5) 6 *)
let six = IntegerLiteral 6
let five = IntegerLiteral 5
let x = Identifier "x"
let binaryExpr = BinaryExpression(x, Plus, five)
let f = Function("x", binaryExpr)
let expr = FunctionCall(f, six)

// if (6 == 5) { 1 } else { 0 }
// if (6 == 6) { 1 } else { 0 }
let compareToSix x = BinaryExpression(six, Equals, x)
let trueExpr = IntegerLiteral 1
let falseExpr = IntegerLiteral 0
let ifExpr1 = IfExpr(compareToSix five, trueExpr, falseExpr)
let ifExpr2 = IfExpr(compareToSix six, trueExpr, falseExpr)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    
    let value = eval Map.empty expr
    let ifExpr1Value = eval Map.empty ifExpr1
    let ifExpr2Value = eval Map.empty ifExpr2

    0 // return an integer exit code
