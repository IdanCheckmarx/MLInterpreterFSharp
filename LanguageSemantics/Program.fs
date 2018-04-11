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

// if (5 == 6) { 1 } else { 0 }
// if (6 == 6) { 1 } else { 0 }
// if (a == 6) { 1 } else { 0 }
let compareToSix x = BinaryExpression(x, Equals, six)
let trueExpr = IntegerLiteral 1
let falseExpr = IntegerLiteral 0
let aIdentifier = Identifier "a"
let ifExpr1 = IfExpr(compareToSix five, trueExpr, falseExpr)
let ifExpr2 = IfExpr(compareToSix six, trueExpr, falseExpr)
let ifExpr3 = IfExpr(compareToSix aIdentifier, trueExpr, falseExpr)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    
    let value = eval Map.empty expr
    let ifExpr1Value = eval Map.empty ifExpr1   // returns [0, 0]
    let ifExpr2Value = eval Map.empty ifExpr2   // returns [1, 1]
    let ifExpr3Value = eval Map.empty ifExpr3   // returns [0, 1]
    
    printfn "\nExpected process results:\n"
    printfn "[0,0]"
    printfn "[1,1]"
    printfn "[0,1]"

    printfn "\nActual process results:\n"
    printfn "%s" (Value.ToString ifExpr1Value)
    printfn "%s" (Value.ToString ifExpr2Value)
    printfn "%s" (Value.ToString ifExpr3Value)

    0 // return an integer exit code
