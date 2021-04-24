module Knus.Desugar where

import Text.ParserCombinators.Parsec

import Knus.Lang

desugar :: Lang -> Lang
desugar (Ident x)   = Ident x
desugar Nil         = Nil
desugar T           = T
desugar (Cons l r)  = Cons l r
desugar (Quote Nil) = Nil
desugar (Quote x)   = Quote x
