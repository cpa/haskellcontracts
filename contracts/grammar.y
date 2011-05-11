{
module Main where
import Data.Char
import Haskell
}

%name haskell
%tokentype { Token }
%error { happyError }

%token
	any	{TokenAny}
        data	{TokenData}
	'='	{TokenEquals}
	':::'	{TokenSatisfies}
	':'	{TokenColon}
	'{'	{TokenCurlyO}
	'}'	{TokenCurlyC}
	'->' 	{TokenArrow}
	var 	{TokenVar $$}
	int 	{TokenInt $$}
	'|' 	{TokenPipe}
	';;' 	{TokenSep}
	case 	{TokenCase}
	of 	{TokenOf}
	'('	{TokenParenO}
	')'	{TokenParenC}
%%

ListGeneral : General ';;' ListGeneral {$1:$3}
	    | General {[$1]}
	    | {- empty -} {[]}

Comm : var Comm {}
     | var {}
     | {- empty -} {}

General : var Args '=' Expr {Def $ Let $1 $2 $4} 
	| var Args '=' case Expr of PatExpr {Def $ LetCase $1 $2 $5 $7}
        | var ':::' Contr { ContSat $ Satisfies $1 $3 }
        | data var Args '=' DataArgs { DataType $ Data $2 $5 }

PatExpr : '|' Pattern '->' Expr PatExpr {($2,$4):$5}
	| '|' Pattern '->' '('Expr')' PatExpr {($2,$5):$7}
	| '|' Pattern '->' Expr {[($2,$4)]}
	| '|' Pattern '->' '('Expr')' {[($2,$5)]}
	| {- empty -} {[]}

Pattern : var Pattern	{$1:$2}
	| var '('Pattern')'	{$1:$3}
	| var {[$1]}
	| '('var')' {[$2]}
	| {- empty -} {[]}

DataArgs : var int '|' DataArgs {($1,$2):$4}
	 | '('var int')' '|' DataArgs {($2,$3):$6}
	 | var int {[($1,$2)]}
	 | '('var int')' {[($2,$3)]}
	 | {- empty -} {[]}

Args : var Args {$1:$2}
     | {- empty -} {[]}

Expr : var { if isUpper $ head $1 then Con $1 else Var $1 }
     | '(' Expr Expr ')' { App $2 $3 }
     | Expr Expr { App $1 $2 }


Contr : '{' var ':' Expr '}' { Pred $2 $4 }
      | var ':' Contr '->' Contr { AppC $1 $3 $5 }
      | '(' var ':' Contr '->' Contr ')' { AppC $2 $4 $6 }

{
happyError :: [Token] -> a
happyError x = error $ "Parse error: " ++ show x

data Token = TokenCase
     	   | TokenExpr
	   | TokenOf
     	   | TokenData
	   | TokenInt Int
	   | TokenPipe
     	   | TokenAny
	   | TokenSep
     	   | TokenEquals
	   | TokenSatisfies
	   | TokenVar String
	   | TokenArrow
	   | TokenColon
	   | TokenParenO
	   | TokenParenC
	   | TokenCurlyO
	   | TokenCurlyC
	   deriving (Eq,Show)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexInt (c:cs)
lexer ('=':cs) = TokenEquals : lexer cs
lexer (':':':':':':cs) = TokenSatisfies : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('{':cs) = TokenCurlyO : lexer cs
lexer ('}':cs) = TokenCurlyC : lexer cs
lexer ('(':cs) = TokenParenO : lexer cs
lexer (')':cs) = TokenParenC : lexer cs
lexer (';':';':cs) = TokenSep : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs

lexInt cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs = case span isAlpha cs of
       ("any",rest) -> TokenAny : lexer rest
       ("data",rest) -> TokenData : lexer rest
       ("case",rest) -> TokenCase : lexer rest
       ("of",rest) -> TokenOf : lexer rest       		     		    	  
       (var,rest)   -> TokenVar var : lexer rest
main = getContents >>= print . haskell . lexer
}
