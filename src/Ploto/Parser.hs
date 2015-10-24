{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Ploto.Parser where

import Text.Parsec
import Data.Functor.Identity
import Ploto.Types
import qualified Text.Parsec.Token as P
import Debug.Trace

parseToCore :: FilePath -> String -> Either ParseError [Core]
parseToCore = parse program

program :: (Stream s Identity Char) => Parsec s u [Core]
program = core `sepBy` wsSeparator

core :: (Stream s Identity Char) => Parsec s u Core
core = varDecl <|> funcDecl

expr :: (Stream s Identity Char) => Parsec s u Core
expr = try varDecl <|>
       try funcCall <|>
       try cond <|>
       try prim <|>
       do sym <- symbol; return (Symbol sym)

varDecl :: (Stream s Identity Char) => Parsec s u Core
varDecl = do
  _ <- string "var"
  skipMany1 space
  sym <- symbol
  skipMany space
  _ <- char '='
  skipMany space
  val <- expr
  return $ VarDecl sym val

funcDecl :: (Stream s Identity Char) => Parsec s u Core
funcDecl = do
  _ <- string "func"
  skipMany1 space
  name <- symbol
  _ <- char '('
  skipMany space
  arglist <- symbol `sepBy` commaSeparator
  skipMany space
  _ <- char ')'
  skipMany1 space
  s <- scope
  return $ FuncDecl name arglist s

funcCall :: (Stream s Identity Char) => Parsec s u Core
funcCall = do
  funcname <- symbol
  skipMany space
  _ <- char '('
  skipMany space
  exprs <- expr `sepBy` commaSeparator
  skipMany space
  _ <- char ')'
  return $ FuncCall funcname exprs

cond :: (Stream s Identity Char) => Parsec s u Core
cond = do
  _ <- string "if"
  skipMany space
  _ <- char '('
  skipMany space
  condition <- expr
  skipMany space
  _ <- char ')'
  skipMany1 space
  s <- scope
  skipMany1 space
  elsecond <- optionMaybe $ try elseCond
  return $ Cond condition s elsecond

elseCond :: (Stream s Identity Char) => Parsec s u Scope
elseCond = do
  _ <- string "else"
  skipMany space
  scope

prim :: (Stream s Identity Char) => Parsec s u Core
prim = try primString <|> try primTrue <|> try primFalse <|> primNumber

primString :: (Stream s Identity Char) => Parsec s u Core
primString = do
  _ <- char '"'
  str <- many $ noneOf "\""
  _ <- char '"'
  return $ Prim (PrimString str)

primNumber :: (Stream s Identity Char) => Parsec s u Core
primNumber = do
  num <- many1 digit
  return $ Prim (PrimInteger $ read num)

primTrue :: (Stream s Identity Char) => Parsec s u Core
primTrue = do
  _ <- string "true"
  return $ Prim PrimTrue

primFalse :: (Stream s Identity Char) => Parsec s u Core
primFalse = do
  _ <- string "false"
  return $ Prim PrimFalse


symbol :: (Stream s Identity Char) => Parsec s u Symbol
symbol = do
  sym <- many1 alphaNum
  return $ SymbolValue sym

scope :: (Stream s Identity Char) => Parsec s u Scope
scope = do
  _ <- char '{'
  skipMany space
  cores <- (try varDecl <|> try cond <|> funcCall)
           `sepEndBy` wsSeparator
  skipMany space
  _ <- char '}'
  return $ ScopeValue cores

wsSeparator :: (Stream s Identity Char) => Parsec s u ()
wsSeparator = skipMany1 (newline <|> space)

commaSeparator :: (Stream s Identity Char) => Parsec s u ()
commaSeparator = do
  skipMany space
  _ <- char ','
  skipMany space
  return ()
