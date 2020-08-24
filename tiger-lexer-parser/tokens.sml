
datatype keywords = Array | If | Then | Else | While | For | To | Let | In |
                       End | Of | Break | Nil | Function | Var | Type | Import 
                       | Primitive |Do 

datatype object_related_keyword = Class | Extends | Method | New 

datatype symbols =  Comma | Colon | SemiColon  | OpenBracket | ClosedBracket | OpenSquareBracket | ClosedSquareBracket |
		OpenFlowerBracket | ClosedFlowerBracket | FullStop | Sub | Add | Mul | Div | Equal | NotEqual | LessThan |LessThanEqual | GreaterThan | GreaterThanEqual | And | Or | Assign  

datatype whitespace = Whitespace ;

datatype end_of_line =  NandR | RandN | SlashR | SlashN  ;

datatype strings = Quote of string  

(* strings datatype should be extendend *) 

datatype identifiers = UnderScoreMain 

(*
datatype operator = Plus | Sub | Mul | Div | Equal | NotEqual | GreaterThan |
                        LessThan | GreaterThanEqual | LessThanEqual | And | Or
*)

datatype Tokens = Key of keywords | Obj of object_related_keyword | Sym of symbols
               | Eof of end_of_line | Comm of string | Id of identifiers |
                Int of int | EOF | Space of whitespace |
                VAR of string | Quote of string 
