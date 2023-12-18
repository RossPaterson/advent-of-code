-- | Simple applicative parser combinators (mostly compatible with Parsec).
--
-- Example (parsing pairs of non-negative integers):
--
-- > (,) <$ char '(' <*> nat <* char ',' <*> nat <* char ')'
--
-- These parsers backtrack extensively, so use them on short strings.
module Parser (
    Parser,
    runParser,
    -- * Lists
    -- | See also 'some' and 'many' from "Control.Applicative".
    count, sepBy1,
    -- * Basic values
    nat, int, enumValue,
    octNumber,
    hexNumber,
    -- * Literals
    string, char,
    -- * Single characters
    satisfy, space, digit, letter, anyChar,
    ) where

import Control.Applicative
import Data.Char
import Data.List
import Numeric

-- | Simple applicative parser
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

-- | Run a parser on a string, failing unless there is exactly one parse.
runParser :: Parser a -> String -> a
runParser (Parser p) s = case [x | (x, t) <- p s, null t] of
    [] -> error ("no parse: " ++ show s)
    [x] -> x
    _ -> error ("ambiguous parse: " ++ show s)

-- specialized parsers

-- | @'satisfy' p@ matches any single character on which @p@ returns 'True'.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser satisfyP
  where
    satisfyP (c:s) | p c = [(c, s)]
    satisfyP _ = []

-- | Literal match of a given character.
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Matches a space character.
space :: Parser Char
space = satisfy isSpace

-- | Matches any digit.
digit :: Parser Char
digit = satisfy isDigit

-- | Matches any letter.
letter :: Parser Char
letter = satisfy isAlpha

-- | This parser succeeds for any character, and returns the parsed character.
anyChar :: Parser Char
anyChar = satisfy (const True)

-- | Parse a non-negative integer.
nat :: Num a => Parser a
nat = (fromInteger . read) <$> some digit

-- | Parse an integer.
int :: Num a => Parser a
int = negate <$ char '-' <*> nat <|> nat

-- | Parse an octal integer.
octNumber :: (Eq a, Num a) => Parser a
octNumber = Parser readOct

-- | Parse a hexadecimal integer.
hexNumber :: (Eq a, Num a) => Parser a
hexNumber = Parser readHex

-- | Literal match of the given string.
string :: String -> Parser String
string str = Parser matchStr
  where
    matchStr t
      | str `isPrefixOf` t = [(str, drop (length str) t)]
      | otherwise = []

-- | Parses the values of an enumerated type by name.
enumValue :: (Show a, Enum a, Bounded a) => Parser a
enumValue = foldr1 (<|>) [c <$ string (show c) | c <- [minBound..maxBound]]

-- general combinators

-- | @'count' n p@ parses @n@ occurrences of @p@ and returns a list of
-- @n@ values returned by @p@.
count :: Applicative p => Int -> p a -> p [a]
count n p = sequenceA (replicate n p)

-- | @'sepBy1' p sep@ parses one or more occurrences of @p@, separated
-- by @sep@, and returns a list of values returned by @p@.
sepBy1 :: Alternative p => p a -> p sep -> p [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
