module GenerateJS where

import Servant
import Servant.JS

import Monte.MonteAPI

js = jsForAPI (Proxy :: Proxy TsuroApi) jquery

main :: IO ()
main = putStrLn "asdf"