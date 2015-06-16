{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Coalgebraic parsing.
module Text.CoalgebraicParsing
  ( -- * Parsers
    Parser
    -- ** Destruct parsers
  , results
  , consume
  , parse
  , isAlive
    -- ** Construct parsers
  , any
  , kill
  , token
  , intersect
  , neg
  , minus
  ) where

import Prelude hiding (foldl, pred, any)

import Control.Applicative
import Control.Monad

import Data.Foldable hiding (any)

-- | A parser that produces results of type 'a' in a data
-- structure of type 'f' when fed tokens of type 't'.
data Parser t f a = Parser
  { -- | Return the results if we would stop parsing now.
    results :: f a
    -- | Consume a token and return a new parser that reflects the
    -- consumed token in its internal state.
  , consume :: t -> Parser t f a
    -- | Status flag indicating whether this parser can have
    -- results in the future. This is the same as being not
    -- bisimilar to 'empty'.
  , isAlive :: Bool
  }

instance Functor f => Functor (Parser t f) where
  fmap f p = Parser
    { results = fmap f (results p)
    , consume = \t -> fmap f (consume p t)
    , isAlive = isAlive p
    }

instance Alternative f => Applicative (Parser p f) where
  pure a = Parser
    { results = pure a
    , consume = \t -> empty
    , isAlive = False
    }

  p <*> q = Parser
    { results = results p <*> results q
    , consume = \t -> consume p t <*> q <|> kill p <*> consume q t
    , isAlive = isAlive q
    }

instance Alternative f => Alternative (Parser t f) where
  empty = Parser
    { results = empty
    , consume = \t -> empty
    , isAlive = False
    }

  p <|> q = Parser
    { results = results p <|> results q
    , consume = \t -> case (isAlive p, isAlive q) of
        (True, True)  -> consume p t <|> consume q t
        (False, True) -> consume q t
        (True, False) -> consume p t
        _             -> empty
    , isAlive = isAlive p || isAlive q
    }

  -- we inline the implementation of 'some' and 'many' here
  -- since otherwise computing 'isAlive' diverges.
  some p = Parser
    { results = empty
    , consume = \t -> fmap (:) (consume p t) <*> many p
    , isAlive = isAlive p
    }

  many p = some p <|> pure []

instance (Alternative f, Foldable f) => Monad (Parser t f) where
  return = pure
  p >>= f = Parser
    { results = empty
    , consume = \t -> consume p t >>= f
    , isAlive = isAlive p
    } <|> asum (fmap f (results p))

instance (Alternative f, Foldable f) => MonadPlus (Parser t f) where
  mzero = empty
  mplus = (<|>)

-- | Matching any token and returning it.
any :: Alternative f => Parser t f t
any = Parser
  { results = empty
  , consume = pure
  , isAlive = True
  }

-- | Semantic Predicate
pred :: (Alternative f, Foldable f) => (a -> Bool) -> Parser t f a -> Parser t f a
pred = mfilter

-- | Remove a parser's future behavior.
kill :: Alternative f => Parser t f a -> Parser t f a
kill p = empty { results = results p }

-- | Accept exactly the given token.
token :: (Alternative f, Foldable f, Eq t) => t -> Parser t f t
token t = pred (== t) any

-- | Parse a list of tokens.
parse :: Parser t f a -> [t] -> f a
parse p ts = results (foldl consume p ts)

-- | Accept words that are accepted by both parsers.
intersect :: Applicative f => Parser t f a -> Parser t f b -> Parser t f (a, b)
intersect p q = Parser
  { results = (,) <$> results p <*> results q
  , consume = \t -> intersect (consume p t) (consume q t)
  , isAlive = isAlive p && isAlive q
  }

-- | Accept words that are *not* accepted by the parser.
neg :: (Alternative f, Foldable f) => Parser t f a -> Parser t f ()
neg p = Parser
  { results = if null (toList (results p)) then pure () else empty
  , consume = \t -> neg (consume p t)
  , isAlive = not (isAlive p)
  }

-- | Accept words accepted by the first but not the second parser.
minus :: Parser t [] a -> Parser t []  b -> Parser t [] a
minus p q = fmap fst (p `intersect` neg q)
