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
cond = string "CONCONCONCONC" >> return (Prim PrimNil)

returnAst :: (Stream s Identity Char) => Parsec s u Core
returnAst = do
  _ <- string "return"
  skipMany1 space
  val <- expr
  return $ Return val

prim :: (Stream s Identity Char) => Parsec s u Core
prim = try primString <|>  primNumber

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

symbol :: (Stream s Identity Char) => Parsec s u Symbol
symbol = do
  sym <- many1 alphaNum
  return $ SymbolValue sym

scope :: (Stream s Identity Char) => Parsec s u Scope
scope = do
  _ <- char '{'
  skipMany space
  cores <- (try varDecl <|> try funcCall <|> try cond <|> returnAst)
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
