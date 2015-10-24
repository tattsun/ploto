{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ploto.Types where

import qualified Data.Map as M
import Control.Monad.State

type PrimFunc = [PrimValue] -> CoreRuntimeT PrimValue

instance Show PrimFunc where
  show = const "Primitive Function"

data Core = VarDecl Symbol Core
          | FuncDecl Symbol [Symbol] Scope
          | PrimFunc PrimFunc
          | FuncCall Symbol [Core]
          | Cond Core Core
          | Prim PrimValue
          | Symbol Symbol
          | Scope Scope
          deriving Show

newtype Symbol = SymbolValue { unSymbolValue :: String }
               deriving (Show, Eq, Ord)
newtype Scope = ScopeValue { unScope :: [Core] }
              deriving Show

data PrimValue = PrimString String
               | PrimInteger Int
               | PrimNil
               deriving Show

newtype AST a = AST { unAST :: a }
                deriving Show

newtype FuncScope = FuncScope { unFuncScope :: M.Map Symbol Core }
newtype VarScope = VarScope { unVarScope :: M.Map Symbol PrimValue }
data CrCallStackFrame = CrCallStackFrame { crLocalVarScope :: VarScope
                                         }
data CoreRuntime = CoreRuntime { crCallStack :: [CrCallStackFrame]
                               , crGlobalFuncScope :: FuncScope
                               , crGlobalVarScope :: VarScope
                               }
newtype CoreRuntimeT a = CoreRuntimeT { unCoreRuntimeT :: StateT CoreRuntime IO a
                                      }
                         deriving (Functor, Applicative, Monad, MonadState CoreRuntime
                                  ,MonadIO)
