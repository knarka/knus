module Knus.Lang where

showCons1 :: Lang -> String
showCons1 (Cons l Nil) = show l
showCons1 (Cons l r)   = (show l) ++ " " ++ (showCons1 r)
showCons1 _            = undefined

showCons :: Lang -> String
showCons c@(Cons _ _) = '(' : (showCons1 c) ++ ")"
showCons _            = undefined

data Lang = Ident String -- TODO: rename members
          | Nil
          | T
          | KNum Integer
          | Cons Lang Lang
          | Quote Lang

instance Show Lang where
    show (Ident x)    = x
    show (Nil)        = "nil"
    show (T)          = "t"
    show (KNum x)     = show x
    show c@(Cons _ _) = showCons c
    show (Quote x)    = '\'' : show x
