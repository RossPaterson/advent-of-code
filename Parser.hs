-- Simple applicative parser combinators (mostly compatible with Parsec)
module Parser where

import Control.Applicative
import Data.Char
import Data.List

newtype Parser a = Parser (ReadS a)

instance Functor Parser where
    fmap f (Parser p) = Parser $ \ s -> [(f x, t) | (x, t) <- p s]

instance Applicative Parser where
    pure x = Parser $ \ s -> [(x, s)]
    Parser pf <*> Parser px =
        Parser $ \ s -> [(f x, u) | (f, t) <- pf s, (x, u) <- px t]

instance Alternative Parser where
    empty = Parser (const [])
    Parser px <|> Parser py = Parser $ \ s -> px s ++ py s

instance Monad Parser where
    return = pure
    Parser px >>= f =
        Parser $ \ s -> [(y, u) | (x, t) <- px s, let Parser q = f x, (y, u) <- q t]

runParser :: Parser a -> String -> a
runParser (Parser p) s = case [x | (x, t) <- p s, null t] of
    x:_ -> x
    _ -> error ("no parse: " ++ show s)

count :: Int -> Parser a -> Parser [a]
count n p = sequence (replicate n p)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser satisfyP
  where
    satisfyP (c:s) | p c = [(c, s)]
    satisfyP _ = []

char :: Char -> Parser Char
char c = satisfy (== c)

space :: Parser Char
space = satisfy isSpace

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

anyChar :: Parser Char
anyChar = satisfy (const True)

-- non-negative integer
nat :: Parser Int
nat = read <$> some digit

string :: String -> Parser String
string str = Parser matchStr
  where
    matchStr t
      | str `isPrefixOf` t = [(str, drop (length str) t)]
      | otherwise = []
