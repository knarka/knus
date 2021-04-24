module Knus.Main where

import System.Environment

import Knus.Parse (parseRaw)

main :: IO ()
main = do
    (a:_) <- getArgs
    putStrLn . show $ parseRaw a
