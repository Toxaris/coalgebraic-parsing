{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Coalgebraic parsing.
module Text.CoalgebraicParsing
  ( -- * Parsers
    Parser
    -- ** Destruct parsers
  , results
  , consume
  , parse
    -- ** Construct parsers
  , anyToken
  , kill
  , token
  , intersect
  , neg
  , minus
  , satisfy
  ) where

import Prelude hiding (foldl)

import Control.Applicative
import Control.Monad

import Data.Foldable

-- | A parser that produces results of type 'a' in a data
-- structure of type 'f' when fed tokens of type 't'.
data Parser t f a = Parser
  { -- | Return the results if we would stop parsing now.
    results :: f a
    -- | Consume a token and return a new parser that reflects the
    -- consumed token in its internal state.
  , consume :: t -> Parser t f a
  }

instance Functor f => Functor (Parser t f) where
  fmap f p = Parser
    { results = fmap f (results p)
    , consume = \t -> fmap f (consume p t)
    }

instance Alternative f => Applicative (Parser p f) where
  pure a = Parser
    { results = pure a
    , consume = \t -> empty
    }

  p <*> q = Parser
    { results = results p <*> results q
    , consume = \t -> consume p t <*> const q t <|> const (kill p) t <*> consume q t
    }

instance Alternative f => Alternative (Parser t f) where
  empty = Parser
    { results = empty
    , consume = \t -> empty
    }

  p <|> q = Parser
    { results = results p <|> results q
    , consume = \t -> consume p t <|> consume q t
    }

instance (Alternative f, Foldable f) => Monad (Parser t f) where
  return = pure
  p >>= f = Parser
    { results = empty
    , consume = \t -> consume p t >>= f
    } <|> asum (fmap f (results p))

instance (Alternative f, Foldable f) => MonadPlus (Parser t f) where
  mzero = empty
  mplus = (<|>)

-- | Matching any token and returning it.
anyToken :: Alternative f => Parser t f t
anyToken = Parser
  { results = empty
  , consume = pure
  }

-- | Semantic Predicate
satisfy :: (Alternative f, Foldable f) => Parser t f a -> (a -> Bool) -> Parser t f a
satisfy p f = mfilter f p

-- | Remove a parser's future behavior.
kill :: Alternative f => Parser t f a -> Parser t f a
kill p = p { consume = \t -> empty }

-- | Accept exactly the given token.
token :: (Alternative f, Foldable f, Eq t) => t -> Parser t f t
token t = anyToken `satisfy` (== t)

-- | Parse a list of tokens.
parse :: Parser t f a -> [t] -> f a
parse p ts = results (foldl consume p ts)

-- | Accept words that are accepted by both parsers.
intersect :: Applicative f => Parser t f a -> Parser t f b -> Parser t f (a, b)
intersect p q = Parser
  { results = (,) <$> results p <*> results q
  , consume = \t -> intersect (consume p t) (consume q t)
  }

-- | Accept words that are *not* accepted by the parser.
neg :: (Alternative f, Foldable f) => Parser t f a -> Parser t f ()
neg p = Parser
  { results = if null (toList (results p)) then pure () else empty
  , consume = \t -> neg (consume p t)
  }

-- | Accept words accepted by the first but not the second parser.
minus :: Parser t [] a -> Parser t []  b -> Parser t [] a
minus p q = fmap fst (p `intersect` neg q)
