module Knus.Parse where

import Text.ParserCombinators.Parsec

import Knus.Lang

--
-- helpers
--

cons :: [Lang] -> Lang
cons []     = Nil
cons (x:xs) = Cons x (cons xs)

sign :: (Num a) => Parser (a -> a)
sign = (char '-' >> return negate) <|> (return id)

nat :: Parser Integer
nat = do x <- (many1 digit)
         return $ (read x::Integer)

identChar :: Parser Char
identChar = letter <|> (oneOf "!?+*/%^")

escapedChar :: Parser String
escapedChar = do _ <- char '\\'
                 x <- anyChar
                 return $ '\\' : [x]

normalChar :: Parser String
normalChar = do x <- (satisfy (/= '"'))
                return [x]

--
-- atoms
-- 
ident :: Parser Lang
ident = do x <- identChar
           xs <- many (digit <|> identChar)
           return $ case (x:xs) of
              "t"   -> T
              "nil" -> Nil
              other -> Ident other

num :: Parser Lang -- TODO: floats
num = do s <- sign
         n <- nat
         return $ KNum (s n)

minus :: Parser Lang
minus = char '-' *> return (Ident "-")

kstring :: Parser Lang
kstring = do _ <- char '"'
             xs <- many (escapedChar <|> normalChar)
             _ <- char '"'
             return . Quote . cons $ map (\x -> KChar x) (concat xs)

atom :: Parser Lang
atom = ident <|> kstring <|> (try num) <|> minus

--
-- language features
--
quote :: Parser Lang
quote = do _ <- char '\''
           x <- lang
           return $ Quote x
list :: Parser Lang
list = do _ <- string "("
          xs <- many (spaces *> lang <* spaces)
          _ <- char ')'
          return $ cons xs

lang :: Parser Lang
lang = quote <|> (try list) <|> atom

program :: Parser Lang
program = do x <- (spaces *> lang)
             _ <- eof
             return x

parseRaw :: String -> Either ParseError Lang
parseRaw input = case parse program "" input of
    Right val -> Right val
    Left err  -> Left err
