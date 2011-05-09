{
module Main where
import Data.Char
import Haskell
}

%name haskell
%tokentype { Token }
%error { happyError }

%token
	let {TokenLet}
	any {TokenAny}
        data {TokenData}
	'=' {TokenEquals}
	':::' {TokenSatisfies}
	':' {TokenColon}
	'{' {TokenCurlyO}
	'}' {TokenCurlyC}
	'->' {TokenArrow}
	var {TokenVar $$}
	int {TokenInt $$}
	'|' {TokenPipe}
	';;' {TokenSep}
%%

ListGeneral : General ';;' ListGeneral {$1:$3}
	    | General {[$1]}
	    | {- empty -} {[]}

General : let var Args '=' Expr {Def $ Let $2 $3 $5} 
        | var ':::' Contr { ContSat $ Satisfies $1 $3 }
        | data var Args '=' DataArgs { DataType $ Data $2 $5 }

DataArgs : var int '|' DataArgs {($1,$2):$4}
	 | var int {[($1,$2)]}
	 | {- empty -} {[]}

Args : var Args {$1:$2}
     | {- empty -} {[]}

Expr : var { if isUpper $ head $1 then Con $1 else Var $1 }
     | Expr Expr { App $1 $2 }


Contr : '{' var ':' Expr '}' { Pred $2 $4 }
      | var ':' Contr '->' Contr { AppC $1 $3 $5 }

{
happyError :: [Token] -> a
happyError x = error $ "Parse error: " ++ show x

data Token = TokenLet
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
lexer (';':';':cs) = TokenSep : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs

lexInt cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs = case span isAlpha cs of
       ("let",rest) -> TokenLet : lexer rest
       ("any",rest) -> TokenAny : lexer rest
       ("data",rest) -> TokenData : lexer rest
       (var,rest)   -> TokenVar var : lexer rest
main = getContents >>= print . haskell . lexer
}
