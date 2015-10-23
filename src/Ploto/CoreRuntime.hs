{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Ploto.CoreRuntime where

import Ploto.Types
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State

pushStackFrame :: CrCallStackFrame -> CoreRuntimeT ()
pushStackFrame sf = do
  s <- get
  put $ s { crCallStack = sf:(crCallStack s)
          }

popStackFrame :: CoreRuntimeT CrCallStackFrame
popStackFrame = do
  s <- get
  let sf = head (crCallStack s)
  put $ s { crCallStack = tail (crCallStack s)
          }
  return sf

resolveVar :: Symbol -> CoreRuntimeT (Maybe PrimValue)
resolveVar sym = do
  stacks <- crCallStack <$> get
  globalMap <- crGlobalVarScope <$> get
  let varOnStack = foldl (\acc sf -> if isNothing acc
                                     then M.lookup sym (unVarScope $ crLocalVarScope sf)
                                     else acc) Nothing stacks
  if isNothing varOnStack
    then return $ M.lookup sym (unVarScope $ globalMap)
    else return varOnStack

resolveFunc :: Symbol -> CoreRuntimeT (Maybe Core)
resolveFunc sym = do
  globalMap <- crGlobalFuncScope <$> get
  return $ M.lookup sym (unFuncScope $ globalMap)

registerVar :: Symbol -> PrimValue -> CoreRuntimeT ()
registerVar sym val = do
  stacks <- crCallStack <$> get
  globalMap <- crGlobalVarScope <$> get
  runtime <- get
  if null stacks
    then registerToGlobal runtime
    else registerToCurrentStack runtime
    where
      registerToGlobal runtime = do
        let globalMap = unVarScope . crGlobalVarScope $ runtime
            newMap = VarScope $ M.insert sym val globalMap
        put $ runtime { crGlobalVarScope = newMap }
      registerToCurrentStack runtime = do
        let currentStack = head . crCallStack  $ runtime
            newStack = currentStack { crLocalVarScope = VarScope $ M.insert sym val $
                                                        unVarScope
                                                        (crLocalVarScope currentStack)
                                    }
        put $ runtime { crCallStack = newStack:(tail $ crCallStack runtime)
                      }

registerFunc :: Symbol -> Core -> CoreRuntimeT ()
registerFunc sym func = do
  funcMap <- unFuncScope . crGlobalFuncScope <$> get
  let newFuncScope = FuncScope $ M.insert sym func funcMap
  runtime <- get
  put $ runtime { crGlobalFuncScope = newFuncScope
                }


newCoreRuntime = CoreRuntime { crCallStack = []
                             , crGlobalFuncScope = FuncScope $ M.empty
                             , crGlobalVarScope = VarScope $ M.empty
                             }

preprocess :: [Core] -> CoreRuntimeT ()
preprocess (x:xs) = preprocessOne x
preprocess [] = return ()

preprocessOne :: Core -> CoreRuntimeT ()
preprocessOne (VarDecl symbol (Prim value)) = registerVar symbol value
preprocessOne (VarDecl _ _) = error "Toplevel variables must be primitives."
preprocessOne decl@(FuncDecl symbol _ _) = registerFunc symbol decl
preprocessOne x = error $ concat ["unexpected toplevel expression: ", show x]


run :: [Core] -> CoreRuntimeT ()
run (x:xs) = undefined

exec :: [Core] -> IO ()
exec cores = flip evalStateT newCoreRuntime $ unCoreRuntimeT $ do
  preprocess cores
  run cores
