module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/System/Authenticate/SmbClient.hs"]
