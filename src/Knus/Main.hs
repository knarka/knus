module Knus.Main where

import System.Environment

import Knus.Parse (parseRaw)
import Knus.Interp (interp)

interpInput :: String -> String
interpInput x = case (parseRaw x) of
    Right lang -> case (interp lang) of
        Right result -> show result
        Left err     -> "error: " ++ err
    Left err   -> show err

main :: IO ()
main = do
    (a:_) <- getArgs
    putStrLn $ interpInput a
