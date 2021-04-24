module Knus.Interp where

import Knus.Lang
import Knus.Ouverture

constolist :: Lang -> [Lang]
constolist Nil = []
constolist (Cons l r) = l : constolist r

apply :: String -> [Lang] -> Either String Lang
apply "+" xs = ou_add xs

interp :: Lang -> Either String String
interp (Ident x)             = Right x
interp Nil                   = Right "nil"
interp T                     = Right "t"
interp (KNum n)              = Right $ show n
interp (Cons (Ident f) args) = (apply f (constolist args)) >>= (\x -> (return $ show x))
interp (Cons _ _)            = Left "cannot apply to non-function"
interp (Quote x)             = Right $ '\'' : show x
