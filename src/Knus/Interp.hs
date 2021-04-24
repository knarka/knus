module Knus.Interp where

import Knus.Lang

interp :: Lang -> Either String String
interp (Ident x)    = Right x
interp Nil          = Right "nil"
interp T            = Right "t"
interp c@(Cons _ _) = Right $ showCons c
interp (Quote x)    = Right $ '\'' : show x
