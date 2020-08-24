fun num_of_tabs 0 = "" |
	num_of_tabs 1 = "\t" |
	num_of_tabs x = "\t" ^ (num_of_tabs(x-1))

fun IdentExpr x exp = case exp of 
			Ast.OP(x1, (op1), y1) => ((IdentExpr x x1) ^ (Ast.binOpToString op1)  ^ ( IdentExpr x y1)) |
			(Ast.Const x) => (Int.toString x) |
			(Ast.Quote str) => (str) |
			(Ast.Method (exp1,exp2)) => (IdentExpr x exp1) ^ "." ^ (IdentExpr x exp2) |
			(Ast.MethodCall (exp1,exp2,exp3)) => (IdentExpr x exp1) ^ "." ^ (IdentExpr x exp2) ^ "(" ^ (IdentExpr x exp3) ^ ")"  |
			(Ast.Assign(exp1,exp2)) => ((IdentExpr x exp1) ^ (" := ")  ^ ( IdentExpr x exp2)) |
			(Ast.Var str) => (str) |
			(Ast.If (exp1,exp2)) => ( "if  "^(IdentExpr x exp1)^("  ")^"then\n"^(num_of_tabs (x+1))^(IdentExpr (x+1) exp2)) |
			(Ast.IfElse (exp1,exp2,exp3)) => ("if  " ^(IdentExpr x exp1)^("  ")^"then\n"^(num_of_tabs (x+1))^(IdentExpr (x+1) exp2)^"\n"^ num_of_tabs (x)^ "else\n"^(num_of_tabs (x+1)) 								 ^(IdentExpr (x+1) exp3)) |
			(Ast.While(exp1,exp2)) => ( "while  " ^ (IdentExpr x exp1) ^ "   do  \n" ^ num_of_tabs (x+1) ^ (IdentExpr (x+1) exp2) ) |
			(Ast.For(exp1,exp2,exp3)) => ( "for  " ^ (IdentExpr x exp1) ^ "  " ^  "to  " ^ (IdentExpr x exp2) ^ "  do\n" ^ (num_of_tabs (x+1)) ^ (IdentExpr (x+1) exp3) ) |
			(Ast.break) => "break\n" |
			(Ast.para exp1) => "( " ^ (IdentExpr x exp1) ^ " )" |
			(Ast.Dec (exp1,exp2)) => ("var " ^  (IdentExpr x exp1) ^ (" := ")  ^ ( IdentExpr x exp2) ) |
			(Ast.Let (exp1,exp2)) => ("let\n") ^ (IdentProgram (x+1)  exp1) ^ ("in\n") ^ (IdentProgram (x+1)  exp2) ^ "end"  |
			(Ast.Array (exp1,exp2)) => (IdentExpr x exp1) ^ "[" ^ (IdentExpr x exp2) ^ "]" 
  
 			
and IdentProgram x  l = case l of 
                        [] => ""  
                        |( x1::xs) => ((num_of_tabs x) ^ (IdentExpr x x1) ^ "\n" ^ ( IdentProgram x xs)) 
