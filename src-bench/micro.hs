import Prelude hiding (concat, foldl)

import Control.Applicative
import Control.Monad hiding (forM_)

import Criterion.Main

import Data.Foldable
import Data.Monoid
import Data.Traversable

import Text.CoalgebraicParsing
import Text.CoalgebraicParsing.Char

main = defaultMain
  [ bgroup "many" $
    [ bgroup "anyToken" $
        let p :: String -> [String]
            p = parse (many anyToken) in
        [ bench (show n) $
            nf p (replicate n 'a')
        | n <- [2, 4 .. 10]
        ]
    ]
  ]
