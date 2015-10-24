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

plotoDef :: (Stream s m Char) => P.GenLanguageDef s u m
plotoDef = P.LanguageDef { P.commentStart = "/*"
                         , P.commentEnd = "*/"
                         , P.commentLine = "//"
                         , P.nestedComments = True
                         , P.identStart = letter
                         , P.identLetter = alphaNum
                         , P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                         , P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                         , P.reservedNames = ["if"
                                             ,"else"
                                             ,"func"
                                             ,"var"
                                             ,"true"
                                             ,"false"
                                             ]
                         , P.reservedOpNames = ["="]
                         , P.caseSensitive = False
                         }

lexer :: (Stream s m Char) => P.GenTokenParser s u m
lexer = P.makeTokenParser plotoDef

program = core `sepBy` ws

core = try varDecl <|> funcDecl

varDecl :: (Stream s Identity Char) => Parsec s u Core
varDecl = do
  reserved "var"
  name <- symbol
  reservedOp "="
  value <- prim
  return $ VarDecl name (Prim value)

funcDecl :: (Stream s Identity Char) => Parsec s u Core
funcDecl = do
  reserved "func"
  name <- symbol
  args <- parens $ symbol `sepBy` comma
  scope <- ScopeValue <$> braces (many expr)
  return $ FuncDecl name args scope

expr :: (Stream s Identity Char) => Parsec s u Core
expr = choice [ try varDecl
              , try funcCall
              , try cond
              , try (Prim <$> prim)
              , Symbol <$> symbol
              ]

funcCall :: (Stream s Identity Char) => Parsec s u Core
funcCall = do
  name <- symbol
  args <- parens $ expr `sepBy` comma
  return $ FuncCall name args

cond :: (Stream s Identity Char) => Parsec s u Core
cond = do
  reserved "if"
  condition <- parens expr
  ok <- ScopeValue <$> braces (many expr)
  els <- optionMaybe $ ScopeValue <$> do
    reserved "else"
    braces (many expr)
  return $ Cond condition ok els

prim :: (Stream s Identity Char) => Parsec s u PrimValue
prim = try primString <|> try primInteger <|> try primTrue <|> primFalse

primString :: (Stream s Identity Char) => Parsec s u PrimValue
primString = PrimString <$> P.stringLiteral lexer

primInteger :: (Stream s Identity Char) => Parsec s u PrimValue
primInteger = PrimInteger . fromIntegral <$> P.integer lexer

primTrue :: (Stream s Identity Char) => Parsec s u PrimValue
primTrue = do
  reserved "true"
  return PrimTrue

primFalse :: (Stream s Identity Char) => Parsec s u PrimValue
primFalse = do
  reserved "false"
  return PrimFalse

reserved :: (Stream s Identity Char) => String -> Parsec s u ()
reserved = P.reserved lexer

reservedOp :: (Stream s Identity Char) => String -> Parsec s u ()
reservedOp = P.reservedOp lexer

symbol :: (Stream s Identity Char) => Parsec s u Symbol
symbol = do
  identifier <- P.identifier lexer
  return $ SymbolValue identifier

braces :: (Stream s Identity Char) => Parsec s u a -> Parsec s u a
braces = P.braces lexer

parens :: (Stream s Identity Char) => Parsec s u a -> Parsec s u a
parens = P.parens lexer

comma  :: (Stream s Identity Char) => Parsec s u String
comma = P.comma lexer

ws  :: (Stream s Identity Char) => Parsec s u ()
ws = P.whiteSpace lexer
