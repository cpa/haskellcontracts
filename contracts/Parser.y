-- -*- haskell -*-
{
module Parser where
import Data.Char
import Haskell
}

-- The second parameter, ListGeneral, is the start symbol.
%name haskell ListGeneral
%tokentype { Token }
%error { happyError }

%token
--        any   {TokenAny}
        data  {TokenData}
        '='   {TokenEquals}
        bad   {TokenBad}
        ':::' {TokenSatisfies}
        ':'   {TokenColon}
        '{'   {TokenCurlyO}
        '}'   {TokenCurlyC}
        '->'  {TokenArrow}
        uvar  {TokenUVar $$}
        lvar  {TokenLVar $$}
        int   {TokenInt $$}
        '|'   {TokenPipe}
        ';;'  {TokenSep}
        case  {TokenCase}
        of    {TokenOf}
        '('   {TokenParenO}
        ')'   {TokenParenC}
--        ','   {TokenComma}
        cf    {TokenCF}
        '||'  {TokenOr}
        '&&'  {TokenAnd}
%%
ListGeneral : sep(General,';;') {$1}

-- Parameterized parsers based on section 6.4.1 of the Happy manual (I
-- can't copy paste from windows to emacs right now :P).
--
-- Get "Internal Happy error"s with these.
snd(p,q)  : p q              {$2}
--list(p)   : list1(p)         {$1}    | {[]}
--list1(p)  : p list(p)        {$1:$2}
list(p)   : p list(p)        {$1:$2} | {[]}
--list1(p)  : p list(p)        {$1:$2}
sep(p,q)  : sep1(p,q)        {$1}    | {[]}
sep1(p,q) : p list(snd(q,p)) {$1:$2}

--ListGeneral : sep(General,';;') {$1}

Vars : list(lvar) {$1}
-- Sanitized constructor (TPTP doesn't allow upper case constant).  It
-- also doesn't allow lower case variables to be quantified over, but
-- we will fix that when we introduce quantifications.
Con : uvar {"'" ++ $1 ++ "'"}
Name : Con {$1} | lvar {$1} -- constructor | constant.

General : lvar Vars '=' Expr                  { Def $ Let $1 $2 $4 } 
        | lvar Vars '=' case Expr of PatExprs { Def $ LetCase $1 $2 $5 $7 }
        | Name ':::' Contr                    { ContSat $ Satisfies $1 $3 }
-- XXX: do we actually support parameterized types?
        | data Con Vars '=' ConDecls          { DataType $ Data $2 $5 }

Pattern  : Con Vars              { $1:$2 }
PatExpr  : '|' Pattern '->' Expr { ($2,$4) }
PatExprs : list(PatExpr)         { $1 }

ConDecl  : Con int          { ($1,$2,okContract $2) }
ConDecls : sep(ConDecl,'|') { $1 }

Atom : Name         { Var $1 }
     | '(' Expr ')' { $2 }
Expr : Expr Atom    { App $1 $2 }
     | bad          { BAD }
     | Atom         { $1 }
-- XXX: there was a commented out FullApp rule.  Might be better to
-- only allow FullApp, until we add support for the "function pointer"
-- translation?
Contr : cf {CF}
{-
Contr : '{' lvar ':' Expr '}'     { Pred $2 $4 }
      | '(' Contr ')'             { $2 }
      | Contr '&&' Contr          { And $1 $3 }
      | Contr '||' Contr          { Or  $1 $3 }
      | lvar ':' Contr '->' Contr { AppC $1 $3 $5 }
      | Contr '->' Contr          { AppC "_" $1 $3 }
      | cf                        { CF }
-}
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
           | TokenUVar String -- Upper case var
           | TokenLVar String -- Lower case var
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
lexer ('-':'-':cs) = lexer $ dropWhile (/= '\n') cs
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexInt (c:cs)

lexInt cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar (c:cs) = token : lexer rest where
  (var,rest) = span (\x -> isAlpha x || x == '_' || isDigit x) (c:cs)
  token = case var of
    "data" -> TokenData
    "case" -> TokenCase
    "of"   -> TokenOf
--    "Any"  -> TokenAny
    "CF"   -> TokenCF
    "BAD"  -> TokenBad
    _      -> (if isUpper c then TokenUVar else TokenLVar) var

main = getContents >>= print . haskell . lexer
}
