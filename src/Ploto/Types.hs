module Ploto.Types where

data Core = VarDecl Symbol Core
          | FuncDecl Symbol [Symbol] Scope
          | FuncCall Symbol [Core]
          | Cond Core Core
          | Return Core
          | Prim PrimValue
          | Symbol Symbol
          | Scope Scope
          deriving Show

newtype Symbol = SymbolValue { unSymbolValue :: String }
               deriving Show
newtype Scope = ScopeValue { unScope :: [Core] }
              deriving Show

data PrimValue = PrimString String
               | PrimInteger Int
               | PrimNil
               deriving Show

newtype AST a = AST { unAST :: a }
                deriving Show
