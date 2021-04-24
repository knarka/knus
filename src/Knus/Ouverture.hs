module Knus.Ouverture where

import Knus.Lang

ou_add :: [Lang] -> Either String Lang
ou_add []            = Right $ KNum 0
ou_add ((KNum x):xs) = case ou_add xs of
    Right (KNum y) -> Right $ KNum (x + y)
    Left err       -> Left err
ou_add _             = Left $ "can only apply 'add' to type 'num'"
