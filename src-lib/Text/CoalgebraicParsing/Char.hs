-- | Character parsers adopted from parsec
-- https://hackage.haskell.org/package/parsec-3.0.0/docs/src/Text-Parsec-Char.html#oneOf
module Text.CoalgebraicParsing.Char where

import Prelude hiding (foldl)
import Data.Char
import Data.Foldable hiding (elem)
import Data.Monoid
import Control.Applicative
import Text.CoalgebraicParsing


anyChar :: (Foldable f, Alternative f) => Parser Char f Char
anyChar = anyToken

char :: (Foldable f, Alternative f) => Char -> Parser Char f Char
char c = anyChar `satisfy` (== c)

oneOf :: (Foldable f, Alternative f) => [Char] -> Parser Char f Char
oneOf cs = anyToken `satisfy` (\c -> c `elem` cs)

noneOf :: (Foldable f, Alternative f) => [Char] -> Parser Char f Char
noneOf cs = anyToken `satisfy` (\c -> not $ c `elem` cs)

spaces :: (Foldable f, Alternative f) => Parser Char f ()
spaces = skipMany space

newline :: (Foldable f, Alternative f) => Parser Char f Char
newline = char '\n'

tab :: (Foldable f, Alternative f) => Parser Char f Char
tab = char '\t'

-- Lifting of Methods on Char to char parsers

space :: (Foldable f, Alternative f) => Parser Char f Char
space = satisfy anyToken isSpace

upper :: (Foldable f, Alternative f) => Parser Char f Char
upper = satisfy anyToken isUpper

lower :: (Foldable f, Alternative f) => Parser Char f Char
lower = satisfy anyToken isLower

alphaNum :: (Foldable f, Alternative f) => Parser Char f Char
alphaNum = satisfy anyToken isAlphaNum

letter :: (Foldable f, Alternative f) => Parser Char f Char
letter = satisfy anyToken isAlpha

digit :: (Foldable f, Alternative f) => Parser Char f Char
digit = satisfy anyToken isDigit

hexDigit :: (Foldable f, Alternative f) => Parser Char f Char
hexDigit = satisfy anyToken isHexDigit

octDigit :: (Foldable f, Alternative f) => Parser Char f Char
octDigit = satisfy anyToken isOctDigit

instance (Alternative f, Monoid a) => Monoid (Parser t f a) where
  mempty = pure mempty
  mappend p q = mappend <$> p <*> q

toStringParser :: Functor f => Parser Char f Char -> Parser Char f String
toStringParser = fmap (:[])

-- XXX maybe we want to have a built-in 'tokens' parser later
--   for performance reasons.
string :: (Foldable f, Alternative f) => String -> Parser Char f String
string = foldl (<>) (pure []) . fmap (toStringParser . token)
