module Text.CoalgebraicParsing.Cobind where

-- * Comonads without Copoint
-- Original comonad definitions from Edward Kmett:
--   https://hackage.haskell.org/package/comonad-0.6.1.1/docs/src/Control-Comonad.html#Comonad

class Functor w => Cobind w where
  duplicate :: w a -> w (w a)
  extend    :: (w a -> b) -> w a -> w b

  duplicate = extend id
  extend f  = fmap f . duplicate

-- | 'extend' with the arguments swapped. Dual to '>>=' for a 'Monad'.
(=>>) :: Cobind w => w a -> (w a -> b) -> w b
(=>>) = flip extend
{-# INLINE (=>>) #-}

-- | 'extend' in operator form
(<<=) :: Cobind w => (w a -> b) -> w a -> w b
(<<=) = extend
{-# INLINE (<<=) #-}

-- | Right-to-left Cokleisli composition
(=<=) :: Cobind w => (w b -> c) -> (w a -> b) -> w a -> c
f =<= g = f . extend g
{-# INLINE (=<=) #-}

-- | Left-to-right Cokleisli composition
(=>=) :: Cobind w => (w a -> b) -> (w b -> c) -> w a -> c
f =>= g = g . extend f
{-# INLINE (=>=) #-}
