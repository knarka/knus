module Knus.Parse where

import Text.ParserCombinators.Parsec

import Knus.Lang

--spaces1 :: Stream s m Char => ParsecT s u m ()
--spaces1 = skipMany1
cons :: [Lang] -> Lang
cons []     = Nil
cons (x:xs) = Cons x (cons xs)

--
-- atoms
-- 
nil :: Parser Lang
nil = (string "nil") *> return Nil

ident :: Parser Lang
ident = do x <- letter
           xs <- many (digit <|> letter <|> oneOf "!?+-*/")
           return $ Ident (x:xs)

t :: Parser Lang -- XXX: rename?
t = char 't' *> return T

atom :: Parser Lang
atom = nil <|> ident <|> t

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
parseRaw input = case parse program "error" input of
    Right val -> Right val
    Left err  -> Left err
