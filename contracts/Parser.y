{
module Parser where
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
	bad     {TokenBad}
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
 	','	{TokenComma}
        cf	{TokenCF}
	'||'    {TokenOr}
	'&&'	{TokenAnd}
%%

ListGeneral : General ';;' ListGeneral {$1:$3}
	    | General {[$1]}
	    | {- empty -} {[]}

Comm : var Comm {}
     | var {}
     | {- empty -} {}

General : var Args '=' Expr {Def $ Let (map toLower $1) $2 $4} 
	| var Args '=' case Expr of PatExpr {Def $ LetCase (map toLower $1) $2 $5 $7}
        | var ':::' Contr { ContSat $ Satisfies (map toLower $1) $3 }
        | data var Args '=' DataArgs { DataType $ Data (map toLower $2) $5 }

PatExpr : '|' Pattern '->' Expr PatExpr {($2,$4):$5}
	| '|' Pattern '->' '('Expr')' PatExpr {($2,$5):$7}
	| '|' Pattern '->' Expr {[($2,$4)]}
	| '|' Pattern '->' '('Expr')' {[($2,$5)]}
	| {- empty -} {[]}

Pattern : var Pattern	{(map toLower $1):$2}
	| var '(' Pattern ')'	{(map toLower $1):$3}
	| var {[map toLower $1]}
	| '('var')' {[map toLower $2]}
	| {- empty -} {[]}

DataArgs : var int '|' DataArgs {(map toLower $1,$2,okContract $2):$4}
	 | '('var int')' '|' DataArgs {(map toLower $2,$3, okContract $3):$6}
	 | var int  {[(map toLower $1,$2,okContract $2)]}
	 | '('var int')' {[(map toLower $2,$3,okContract $3)]}
	 | var int ':::' Contr '|' DataArgs {(map toLower $1,$2,$4):$6}
	 | '('var int')' ':::' Contr '|' DataArgs {(map toLower $2,$3,$6):$8}
	 | var int ':::' Contr {[(map toLower $1,$2,$4)]}
	 | '('var int')' ':::' Contr {[(map toLower $2,$3,$6)]}
	 | {- empty -} {[]}

Args : var Args {(map toLower $1):$2}
     | {- empty -} {[]}

Atom : var { if isUpper $ head $1 then Con ((map toLower $1)) else Var ((map toLower $1)) }
     | '(' Expr ')' { $2 }

Expr : '(' Expr Atom ')' { App $2 $ $3 }
     | cf '(' Expr ')' { CF $ $3 }
     | Expr Atom { App $1 $ $2 }
     | bad { BAD }
     | '(' bad Expr ')' { App BAD $3 }
     | bad Expr { App BAD $2 }
     | Atom { $1 }
     | var '(' commaArgs ')' { FullApp (map toLower $1) $ $3 }

commaArgs : Expr ',' commaArgs { $1:$3 }
	  | Expr { [$1] }
	  | {- empty -} { [] }

Contr : '{' var ':' Expr '}' { Pred (map toLower $2) $4 }
      | '(' Contr ')' { $2 }
      | Contr '&&' Contr { And $1 $3 }
      | Contr '||' Contr { Or  $1 $3 }
      | var ':' Contr '->' Contr { AppC (map toLower $1) $3 $5 }
      | '(' var ':' Contr '->' Contr ')' { AppC (map toLower $2) $4 $6 }

{

happyError :: [Token] -> a
happyError x = error $ "Parse error: " ++ show x

data Token = TokenCase
     	   | TokenExpr
	   | TokenOf
     	   | TokenData
	   | TokenInt Int
	   | TokenPipe
	   | TokenCF
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
	   | TokenBad
	   | TokenComma
	   | TokenOr
	   | TokenAnd
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
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('{':cs) = TokenCurlyO : lexer cs
lexer ('}':cs) = TokenCurlyC : lexer cs
lexer ('(':cs) = TokenParenO : lexer cs
lexer (')':cs) = TokenParenC : lexer cs
lexer (';':';':cs) = TokenSep : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs
lexer (',':cs) = TokenComma : lexer cs

lexInt cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs = case span (\x -> isAlpha x || x == '_') cs of
       ("any",rest) -> TokenAny : lexer rest
       ("bad",rest) -> TokenBad : lexer rest
       ("BAD",rest) -> TokenBad : lexer rest
       ("data",rest) -> TokenData : lexer rest
       ("case",rest) -> TokenCase : lexer rest
       ("of",rest) -> TokenOf : lexer rest       		     		    	  
       ("cf",rest) -> TokenCF : lexer rest
       ("CF",rest) -> TokenCF : lexer rest
       (var,rest)   -> TokenVar var : lexer rest
main = getContents >>= print . haskell . lexer
}
