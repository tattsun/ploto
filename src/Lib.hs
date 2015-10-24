module Lib
    ( parseToCore
    , exec
    ) where

import Ploto.Parser
import Ploto.CoreRuntime

someFunc :: IO ()
someFunc = putStrLn "someFunc"
