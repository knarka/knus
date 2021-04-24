module Knus.Lang where

import Data.List (intersperse) -- for intersperse

data Lang = Ident String -- TODO: rename members
          | Nil
          | T
          | Cons Lang Lang
          | Quote Lang

prettyList :: Lang -> String
prettyList (Cons l Nil) = show l
prettyList (Cons l r)   = (show l) ++ " " ++ (prettyList r)

instance Show Lang where
    show (Ident x)    = x
    show (Nil)        = "nil"
    show (T)          = "t"
    show c@(Cons _ _) = '(' : (prettyList c) ++ ")"
    show (Quote x)    = '\'' : show x
