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
  , fmaps
  , skipMany
  , consumed
    -- ** Delegate parsing
  , delegate
  , delegateOnce
  , delegateWhile
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

-- | Maps a collection of functions over a parser's results.
fmaps :: Applicative f => f (a -> b) -> Parser t f a -> Parser t f b
fmaps fs p = Parser
  { results = fs <*> results p
  , consume = \t -> fmaps fs (consume p t)
  }

instance Alternative f => Applicative (Parser p f) where
  pure a = Parser
    { results = pure a
    , consume = \t -> empty
    }

  p <*> q = Parser
    { results = results p <*> results q
    , consume = \t -> consume p t <*> q <|> fmaps (results p) (consume q t)
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
parse :: Foldable r => Parser t f a -> r t -> f a
parse p ts = results $ feed p ts

-- | Feed a list of tokens to the parser
feed :: Foldable r => Parser t f a -> r t -> Parser t f a
feed p ts = foldl consume p ts

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

-- | applying p zero or more times, skipping the results
skipMany :: (Alternative f, Foldable f) => Parser t f a -> Parser t f ()
skipMany p = fmap (const ()) (many p)

-- | Delegate processing to another parser and always return the
-- current state of this parser as result.
delegate :: Applicative f => Parser t f a -> Parser t f (Parser t f a)
delegate p = Parser
  { results = pure p
  , consume = \t -> delegate (consume p t)
  }

-- | Records the tokens consumed by p and returns them as result
consumed :: Alternative f => Parser t f a -> Parser t f [t]
consumed p = fmap fst $ (many anyToken) `intersect` p

-- | Delegate to the first parser, while the second parser matches
delegateWhile :: Alternative f => Parser t f a -> Parser t f b -> Parser t f (Parser t f a)
delegateWhile p q = fmap fst $ intersect (delegate p) q

-- | Delegate processing of one token to another parser
delegateOnce :: Alternative f => Parser t f a -> Parser t f (Parser t f a)
delegateOnce p = delegateWhile p anyToken

-- | Delegate processing of 'n' tokens to another parser
delegateN :: (Alternative f, Foldable f) => Parser t f a -> Int -> Parser t f (Parser t f a)
delegateN p n = delegateWhile p (replicateM n anyToken)

-- | Feeds the results produced by the first parser as input
-- to the second. Often t == t'
feedTo :: Applicative f => Foldable r => Parser t f (r t') -> Parser t' f a -> Parser t f (Parser t' f a)
feedTo p q = fmap (feed q) p
