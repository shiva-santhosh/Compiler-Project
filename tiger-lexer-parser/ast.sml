structure Ast  = struct 

datatype binary_op = Plus | Minus | Mul | Divison | Equal | NotEqual | GT | LT |  GTE | LTE | And | Or

datatype Expr = Var of string |
		Const of int |
		Quote of string |
		Assign of Expr*Expr |
                OP of (Expr*binary_op*Expr) |
		If of (Expr*Expr) |
		IfElse of (Expr*Expr*Expr) |
		While of (Expr*Expr) |
		For of (Expr*Expr*Expr)|
		para of (Expr) | 
		Dec of (Expr*Expr) |
		Let of (Expr list * Expr list)  |
		Array of (Expr*Expr) |
		Method of (Expr*Expr) |
		MethodCall of (Expr*Expr*Expr)|
		break

fun plus x y = OP (x, Plus, y) 
fun minus x y = OP (x, Minus, y)
fun mul x y = OP (x, Mul, y)
fun div1 x y = OP (x, Divison, y)
fun eq x y = OP (x, Equal, y) 
fun neq x y = OP (x, NotEqual, y)
fun gt x y = OP (x, GT, y)
fun lt x y = OP (x, LT, y)
fun gte x y = OP (x, GTE, y) 
fun lte x y = OP (x, LTE, y)
fun and1 x y = OP (x, And, y)
fun or x y = OP (x, Or, y)
fun assign x y = Assign (x, y)

fun binOpToString Plus  = " + "
  | binOpToString Minus = " - "
  | binOpToString Mul   = " * "
  | binOpToString Divison = " / " 
  | binOpToString Equal = " = " 
  | binOpToString NotEqual = " <> " 
  | binOpToString GT = " > " 
  | binOpToString LT = " < " 
  | binOpToString GTE = " >= " 
  | binOpToString LTE = " <= " 
  | binOpToString And = " & " 
  | binOpToString Or = " | "  ;

end   
