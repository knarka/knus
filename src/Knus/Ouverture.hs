module Knus.Ouverture where

import Knus.Lang
import Knus.Parse (cons)

-- TODO: centralize error messages

ou_add :: [Lang] -> Either String Lang
ou_add []            = Right $ KNum 0
ou_add ((KNum x):xs) = case ou_add xs of
    Right (KNum y) -> Right $ KNum (x + y)
    Left err       -> Left err
ou_add _             = Left $ "type: can only apply '+' to 'num'"

ou_sub :: [Lang] -> Either String Lang
ou_sub []            = Left $ "arity: expect at least 1 for '-'"
ou_sub [(KNum x)]    = Right $ KNum (negate x)
ou_sub ((KNum x):xs) = case ou_add xs of
    Right (KNum y) -> Right $ KNum (x - y)
    Left err       -> Left $ "type: can only apply '-' to 'num'"
ou_sub _             = Left $ "type: can only apply '-' to 'num'"

ou_mult :: [Lang] -> Either String Lang
ou_mult []            = Right $ KNum 0
ou_mult ((KNum x):xs) = case ou_mult xs of
    Right (KNum y) -> Right $ KNum (x * y)
    Left err       -> Left err
ou_mult _             = Left $ "type: can only apply '*' to 'num'"

ou_type1 :: Lang -> String
ou_type1 (Ident _)   = "ident"
ou_type1 Nil         = "list"
ou_type1 T           = "t"
ou_type1 (KNum _)    = "num"
ou_type1 (KChar _)   = "char"
ou_type1 (Cons _ _)  = "cons"
ou_type1 (Quote _)   = "quote"

ou_type :: [Lang] -> Either String Lang
ou_type [x] = Right $ cons $ map (\x -> KChar x) (ou_type1 x)
ou_type _   = Left "arity: expect 1 for 'type'"
