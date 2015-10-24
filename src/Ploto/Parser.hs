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

program = many core

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
{-
program :: (Stream s Identity Char) => Parsec s u [Core]
program = core `sepBy` wsSeparator

core :: (Stream s Identity Char) => Parsec s u Core
core = try varDecl <|> funcDecl

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
  skipMany ws
  _ <- char '('
  skipMany ws
  condition <- expr
  skipMany ws
  _ <- char ')'
  skipMany ws
  s <- scope
  skipMany ws
  elsecond <- optionMaybe $ try elseCond
  return $ Cond condition s elsecond

elseCond :: (Stream s Identity Char) => Parsec s u Scope
elseCond = do
  _ <- string "else"
  skipMany ws
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
  skipMany ws
  cores <- (try varDecl <|> try cond <|> funcCall)
           `sepEndBy` wsSeparator
  skipMany ws
  _ <- char '}'
  return $ ScopeValue cores

ws :: (Stream s Identity Char) => Parsec s u ()
ws = do
  _ <- try newline <|> space
  return ()

wsSeparator :: (Stream s Identity Char) => Parsec s u ()
wsSeparator = skipMany1 (newline <|> space)

commaSeparator :: (Stream s Identity Char) => Parsec s u ()
commaSeparator = do
  skipMany space
  _ <- char ','
  skipMany space
  return ()
-}
