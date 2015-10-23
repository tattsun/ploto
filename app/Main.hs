module Main where

import Lib

main :: IO ()
main = do
  str <- getContents
  print $ parseToCore "stdin" str
