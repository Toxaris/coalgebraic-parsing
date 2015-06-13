{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Coalgebraic parsing.
module Text.CoalgebraicParsing
  ( -- * Parsers
    Parser
  , results
  , consume
  , parse
  , token
    -- * Transition tables   
  , Table
  , delta
  , step
  , skip
  , kill
  , cond
  ) where

import Control.Applicative

-- | A transition table for a state machine with input tokens
-- 't', behavior 'f' and result 'a'.
newtype Table t f a = Table
  { -- | Look up the target of a transition in the table.
    delta :: t -> f a
  }

instance Functor f => Functor (Table t f) where
  fmap f tbl = Table (\t -> fmap f (delta tbl t))

instance Applicative f => Applicative (Table t f) where
  pure a = Table (\t -> pure a)
  p <*> q = Table (\t -> delta p t <*> delta q t)

instance Alternative f => Alternative (Table t f) where
  empty = Table (\t -> empty)
  p <|> q = Table (\t -> delta p t <|> delta q t)

-- | A transition table that maps all inputs to the same
-- behavior.
skip :: f a -> Table t f a
skip p = Table (\t -> p)

-- | A transition table that behaves different depending on a
-- predicate on tokens.
cond :: (t -> Bool) -> f a -> f a -> Table t f a
cond f p q = Table (\t -> if f t then p else q)

-- | A parser that produces results of type 'a' in a data
-- structure of type 'f' when fed tokens of type 't'.
data Parser t f a = Parser
  { -- | Return the results if we would stop parsing now.
    results :: f a
    -- | The transition table of this parser.
  , step :: Table t (Parser t f) a
  }

instance Functor f => Functor (Parser t f) where
  fmap f p = Parser
    { results = fmap f (results p)
    , step = fmap f (step p)
    }

instance Alternative f => Applicative (Parser p f) where
  pure a = Parser
    { results = pure a
    , step = empty
    }

  p <*> q = Parser
    { results = results p <*> results q
    , step = step p <*> skip q <|> skip (kill p) <*> step q
    }

instance Alternative f => Alternative (Parser t f) where
  empty = Parser
    { results = empty
    , step = empty
    }

  p <|> q = Parser
    { results = results p <|> results q
    , step = step p <|> step q
    }

-- | Remove a parser's future behavior.
kill :: Alternative f => Parser t f a -> Parser t f a
kill p = p { step = empty }

-- | Accept exactly the given token.
token :: (Alternative f, Eq t) => t -> Parser t f t
token t = Parser
  { results = empty
  , step = cond (t ==) (pure t) empty
  }

-- | Consume a token and return a new parser that reflects the
-- consumed token in its internal state.
consume :: Parser t f a -> t -> Parser t f a
consume = delta . step

-- | Parse a list of tokens.
parse :: Parser t f a -> [t] -> f a
parse p ts = results (foldl consume p ts)
