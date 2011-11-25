-- -*- haskell -*-
{
module Parser where
import Data.Char
import Haskell
import Prelude hiding (lex)
}

-- The second parameter, ListGeneral, is the start symbol.
%name haskell ListGeneral
%tokentype { Token }
%error { happyError }

%token
--        any   {TokenAny}
        import{TokenImport}
        data  {TokenData}
        '='   {TokenEquals}
        ':::' {TokenSatisfies}
        ':'   {TokenColon}
        '{'   {TokenCurlyO}
        '}'   {TokenCurlyC}
        '->'  {TokenArrow}
        con   {TokenCon $$}
        var   {TokenVar $$}
        '|'   {TokenPipe}
        ';;'  {TokenDoubleSep}
        ';'   {TokenSingleSep}
        '.'   {TokenDot}
        case  {TokenCase}
        of    {TokenOf}
        '('   {TokenParenO}
        ')'   {TokenParenC}
--        ','   {TokenComma}
        cf    {TokenCF}
        '||'  {TokenOr}
        '&&'  {TokenAnd}
        '{-# CONTRACT'  {TokenContractPragmaO}
        '#-}' {TokenPragmaC}
%%

-- Parameterized parsers based on section 6.4.1 of the Happy manual (I
-- can't copy paste from windows to emacs right now :P).
fst(p,q)  : p q              {$1}
snd(p,q)  : p q              {$2}
list(p)   : p list(p)        {$1:$2} | {[]}
list1(p)  : p list(p)        {$1:$2}
sep(p,s)  : sep1(p,s)        {$1}    | {[]} -- p's separated by s's
sep1(p,s) : p list(snd(s,p)) {$1:$2}

-- Careful: I got "Internal Happy error", not a parse error, when I
-- erroneously used sep(General,';;') here.
--
-- XXX: that was probably a bug, ask Simon M.
ListGeneral : list(fst(General,';;')) {$1}

Vars : list(var) {$1}
Named : con {Con $1} | var {Var $1} -- constructor | constant.
Module : sep1(con,'.') { $1 }

General : var Vars '=' CaseExpr                { Def $ Let $1 $2 $4 }
        | '{-# CONTRACT' var ':::' Contr '#-}' { ContSat $ Satisfies $2 $4 }
-- XXX: do we actually support parameterized types?
        | data con Vars '=' ConDecls           { DataType $ Data $2 $5 }
        | import Module                        { Import $2 }

-- We format case expressions in a way that's easy to parse w/o
-- layout, but is also valid Haskell: e.g.
--
--   case e of {
--   ; p1 -> case e' of {
--           ; p' -> e'' 
--           }
--   ; p2 -> e''' 
--   };;
Pattern  : con Vars                           { ($1, $2) }
PatExpr  : Pattern '->' CaseExpr              { ($1, $3) }
CaseExpr : Expr                               { Base $1 }
         | case Expr of
          '{' list(snd(';', PatExpr)) '}'           { Case $2 $5 }

-- 'Any' here is the constructor contract.  Not currently used, but
-- some 'assert's compare terms for equality, which looks at this
-- component, so e.g. 'undefined' is not suitable.
ConDecl  : con list(Atom)   { ($1,length $2,Any) }
ConDecls : sep(ConDecl,'|') { $1 }

Atom : Named        { Named $1 }
     | '(' Expr ')' { $2 }
Expr : Expr Atom    { $1 :@: $2 }
     | Atom         { $1 }
-- Q: there was a commented out FullApp rule.  Might be better to
-- only allow FullApp, until we add support for the "function pointer"
-- translation?
-- A: NO! FullApp is the special case, not regular app!
ContrAtom : '{' var ':' Expr '}'      { Pred $2 $4 }
          | cf                        { CF }
          | '(' Contr ')'             { $2 }
Contr : ContrAtom '&&' ContrAtom      { And $1 $3 }
      | ContrAtom '||' ContrAtom      { Or  $1 $3 }
      | var ':' ContrAtom '->' Contr  { Arr (Just $1) $3 $5 }
       -- XXX, HACK: "" becomes a fresh name in cTrans.
      |         ContrAtom '->' Contr  { Arr Nothing $1 $3 }
      | ContrAtom                     { $1 }

{
happyError :: [Token] -> a
happyError x = error $ "Parse error: " ++ show x

data Token = TokenCase
           | TokenExpr
           | TokenOf
           | TokenData
           | TokenImport
           | TokenPipe
           | TokenCF
           | TokenAny
           | TokenDoubleSep
           | TokenSingleSep
           | TokenEquals
           | TokenSatisfies
           | TokenCon String -- Upper case var
           | TokenVar String -- Lower case var
           | TokenDot
           | TokenArrow
           | TokenColon
           | TokenParenO -- 'O' = 'open'
           | TokenParenC -- 'C' = 'close'
           | TokenCurlyO
           | TokenCurlyC
           | TokenComma
           | TokenOr
           | TokenAnd
           | TokenContractPragmaO
           | TokenPragmaC
           deriving (Eq,Show)

lex :: String -> [Token]
lex [] = []
-- Skip lines we don't care about.
--
-- XXX: this is pretty ad-hoc.  Maybe better to use CPP?  Use
-- '-fno-warn-unrecognised-pragmas' to avoid GHC's whining.
lex ('{':'-':'#':' ':'S':'K':'I':'P':' ':'#':'-':'}':cs) = lex . skip $ skip cs
lex ('=':cs) = TokenEquals : lex cs
lex (':':':':':':cs) = TokenSatisfies : lex cs
lex (':':cs) = TokenColon : lex cs
lex ('|':'|':cs) = TokenOr : lex cs
lex ('&':'&':cs) = TokenAnd : lex cs
lex ('-':'>':cs) = TokenArrow : lex cs
-- The POPL 09 syntax.
lex ('{':'-':'#':' ':'C':'O':'N':'T':'R':'A':'C':'T':cs)
  = TokenContractPragmaO : lex cs
lex ('#':'-':'}':cs) = TokenPragmaC : lex cs
lex ('{':cs) = TokenCurlyO : lex cs
lex ('}':cs) = TokenCurlyC : lex cs
lex ('(':cs) = TokenParenO : lex cs
lex (')':cs) = TokenParenC : lex cs
lex (';':';':cs) = TokenDoubleSep : lex cs
lex (';':cs) = TokenSingleSep : lex cs
lex ('|':cs) = TokenPipe : lex cs
lex (',':cs) = TokenComma : lex cs
lex ('.':cs) = TokenDot : lex cs
lex ('\'':_) = error "Single quotes (\"'\") are not allowed in source files :P"
-- Discard comments.
lex ('-':'-':cs) = lex $ skip cs
lex (c:cs) 
      | isSpace c = lex cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = error "We don't lex numbers currently!" -- lexInt (c:cs)
{-
lexInt cs = TokenInt (read num) : lex rest
      where (num,rest) = span isDigit cs
-}
lex cs = error $ "Don't know how to lex: "++(head . lines $ cs)

skip cs = let cs' = dropWhile (/= '\n') cs
          in if not (null cs')
             then tail cs' -- Drop the newline, if any.
             else ""

lexVar (c:cs) = token : lex rest where
  (var,rest) = span (\x -> isAlpha x || x == '_' || isDigit x) (c:cs)
  token = case var of
    "data"   -> TokenData
    "case"   -> TokenCase
    "of"     -> TokenOf
--    "Any"  -> TokenAny
    "CF"     -> TokenCF
    "import" -> TokenImport
    _        -> (if isUpper c then TokenCon else TokenVar) var

parse = haskell . lex

main = getContents >>= print . parse
}
