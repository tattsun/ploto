module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runProgram
    else runOption args

  where
    runProgram = do
      str <- getContents
      let parsedResult = parseToCore "stdin" str
      case parsedResult of
        Left err -> error ("parse failed " ++ show err)
        Right core -> exec core
    runOption args = do
      case head args of
        "ast" -> do
          str <- getContents
          print $ parseToCore "stdin" str
        _ -> error "unknown option"
