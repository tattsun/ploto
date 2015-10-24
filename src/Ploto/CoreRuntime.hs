{-# LANGUAGE OverloadedStrings #-}
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
  let varOnStack =
        if null stacks
        then Nothing
        else M.lookup sym (unVarScope $ crLocalVarScope (head stacks))
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
newStackFrame = CrCallStackFrame { crLocalVarScope = VarScope $ M.empty
                                 }

preprocess :: [Core] -> CoreRuntimeT ()
preprocess (x:xs) = do
  preprocessOne x
  preprocess xs
preprocess [] = return ()

preprocessOne :: Core -> CoreRuntimeT ()
preprocessOne (VarDecl symbol (Prim value)) = registerVar symbol value
preprocessOne (VarDecl _ _) = error "Toplevel variables must be primitives."
preprocessOne decl@(FuncDecl symbol _ _) = registerFunc symbol decl
preprocessOne x = error $ concat ["unexpected toplevel expression: ", show x]

symbolToPrimString :: Symbol -> PrimValue
symbolToPrimString sym = PrimString $ unSymbolValue sym

eval :: Core -> CoreRuntimeT PrimValue
eval (VarDecl sym core) = do
  value <- eval core
  registerVar sym value
  return value
eval (FuncCall sym args) = do
  evaledArgs <- mapM eval args
  func <- resolveFunc sym
  case func of
    Nothing -> error ("function not found: " ++ unSymbolValue sym)
    Just (PrimFunc primfunc) -> primfunc evaledArgs
    Just (FuncDecl _ argNames scope) -> do
      let argsWithName = zip argNames evaledArgs
      pushStackFrame $ newStackFrame { crLocalVarScope = VarScope $ M.fromList argsWithName
                                     }
      result <- eval $ Scope scope
      void popStackFrame
      return result
eval (Prim prim) = return prim
eval (Symbol sym) = do
  var <- resolveVar sym
  case var of
    Nothing -> error ("variable not found: " ++ unSymbolValue sym)
    Just prim -> return prim
eval (Scope (ScopeValue cores)) = do
  last <$> mapM eval cores
eval x = error ("unexpected object: " ++ show x)

exec :: [Core] -> IO ()
exec cores = flip evalStateT newCoreRuntime $ unCoreRuntimeT $ do
  mapM_ (\(name, primfunc) ->
           registerFunc (SymbolValue name) (PrimFunc primfunc)
        ) primFuncs

  preprocess cores

  eval (FuncCall (SymbolValue "main") [])
  return ()

----------------------------------------------------------------------

primFuncs :: [(String, PrimFunc)]
primFuncs = [("print", p_print)
            ,("println", p_println)
            ,("concat", p_concat)
            ,("add", p_add)
            ,("sub", p_sub)
            ,("mul", p_mul)
            ,("div", p_div)
            ]

p_print :: PrimFunc
p_print (x:[]) = p_print' x
  where
    p_print' p@(PrimString str) = do
      liftIO $ putStr str
      return p
    p_print' p@(PrimInteger int) = do
      liftIO $ putStr (show int)
      return p
p_print _ = error "argument error"


p_println :: PrimFunc
p_println (x:[]) = p_println' x
  where
    p_println' p@(PrimString str) = do
      liftIO $ putStrLn str
      return p
    p_println' p@(PrimInteger int) = do
      liftIO $ putStrLn (show int)
      return p
p_println _ = error "argument error"

p_concat :: PrimFunc
p_concat ((PrimString x):(PrimString y):[]) = return $ PrimString (x++y)
p_concat _ = error "argument error"

p_add :: PrimFunc
p_add ((PrimInteger x):(PrimInteger y):[]) = return $ PrimInteger (x+y)
p_add _ = error "argument error"

p_sub :: PrimFunc
p_sub ((PrimInteger x):(PrimInteger y):[]) = return $ PrimInteger (x-y)
p_sub _ = error "argument error"

p_mul :: PrimFunc
p_mul ((PrimInteger x):(PrimInteger y):[]) = return $ PrimInteger (x*y)
p_mul _ = error "argument error"

p_div :: PrimFunc
p_div ((PrimInteger x):(PrimInteger y):[]) = return $ PrimInteger (x `div` y)
p_div _ = error "argument error"
