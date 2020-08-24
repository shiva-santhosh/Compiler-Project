type lineNo            = int
type pos               = lineNo  
val  lineRef : pos ref = ref 0
fun updateLine n      = lineRef := !(lineRef) + n

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token

fun lineRange l r = "line " ^ l
fun error (e,l,r) = TextIO.output(TextIO.stdErr, lineRange l r ^ ":" ^ e ^ "\n")

fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)

fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode
val newlineCount = List.length o List.filter (fn x => x = #"\n") o String.explode

%%
%header (functor ExprLexFun(structure Tokens : Expr_TOKENS));

ws    = [\ \t];
digit = [0-9]+;
alpha = [A-Za-z] ;
quote = \"[^\"]*\";
id = {alpha}({alpha}|{digit}|_)*; 


%%
{ws}+         => ( lex() );
\n({ws}*\n)*  => ( lex());
[-]?{digit}+  => ( Tokens.CONST (toInt yytext, !lineRef, !lineRef) );
"if"	      => ( Tokens.IF  (!lineRef,!lineRef)) ;
"else"	      => ( Tokens.ELSE  (!lineRef,!lineRef)) ;
"then"	      => ( Tokens.THEN  (!lineRef,!lineRef)) ;
"while"	      => ( Tokens.WHILE  (!lineRef,!lineRef)) ;
"do"          => ( Tokens.DO  (!lineRef,!lineRef)) ;
"for"         => ( Tokens.FOR  (!lineRef,!lineRef)) ;
"to"          => ( Tokens.TO  (!lineRef,!lineRef)) ;
"break"	      => ( Tokens.BREAK  (!lineRef,!lineRef)) ;
"var"	      => ( Tokens.VAR  (!lineRef,!lineRef)) ;
"let"	      => ( Tokens.LET  (!lineRef,!lineRef)) ;
"in"	      => ( Tokens.IN  (!lineRef,!lineRef)) ;
"end"	      => ( Tokens.END  (!lineRef,!lineRef)) ;
"("	      => ( Tokens.OB  (!lineRef,!lineRef)) ;
")"	      => ( Tokens.CB  (!lineRef,!lineRef)) ;
"["           => ( Tokens.LBRAC  (!lineRef,!lineRef)) ;
"]"           => ( Tokens.RBRAC  (!lineRef,!lineRef)) ;
";"	      => ( Tokens.SEMICOLON  (!lineRef,!lineRef)) ;
":="	      => ( Tokens.ASSIGN  (!lineRef,!lineRef)) ;
"+"           => ( Tokens.PLUS  (!lineRef,!lineRef) );
"-"           => ( Tokens.MINUS  (!lineRef,!lineRef) );
"*"           => ( Tokens.MUL (!lineRef,!lineRef) );
"/"           => ( Tokens.DIV (!lineRef,!lineRef) );
">"           => ( Tokens.GT (!lineRef,!lineRef) );
">="           => ( Tokens.GTE (!lineRef,!lineRef) );
"<"           => ( Tokens.LT (!lineRef,!lineRef) );
"<="           => ( Tokens.LTE (!lineRef,!lineRef) );
"<>"           => ( Tokens.NOTEQUAL (!lineRef,!lineRef) );
"="           => ( Tokens.EQUAL (!lineRef,!lineRef) );
"&"           => ( Tokens.AND (!lineRef,!lineRef) );
"|"           => ( Tokens.OR (!lineRef,!lineRef) );
"."	      => ( Tokens.DOT (!lineRef,!lineRef) ) ;
{quote}	      => (Tokens.QUOTE (yytext, !lineRef,!lineRef));
{id} 	      => (Tokens.Var (yytext, !lineRef,  !lineRef)) ;
