module Expr where

type Variable = String
type Constructor = String

data Expr = Var Variable
          | App Expr Expr
          | Con Constructor (Maybe Expr)
          | BAD | UNR
          deriving (Eq,Show)
                  
