import Prelude hiding (concat, foldl)

import Control.Applicative
import Control.Monad hiding (forM_)

import Data.Foldable
import Data.Monoid
import Data.Char

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.CoalgebraicParsing
import Text.CoalgebraicParsing.Char

main = defaultMain tests

assertAccept :: String -> String -> Parser Char [] a -> Assertion
assertAccept msg text p = do
  when (null (parse p text)) $ do
    assertString (msg ++ " rejects " ++ show text ++ ".\nShould accept.")

assertReject :: String -> String -> Parser Char [] a -> Assertion
assertReject msg text p = do
  unless (null (parse p text)) $ do
    assertString (msg ++ " accepts " ++ show text ++ ".\nShould reject.")

-- simple layout sensitive parsing
--
-- Example program:
--
-- 0000
-- 00
-- def foo
--   11
--   11
-- 0
-- def bar
--   def baz
--     22
--   1
levelId :: Int -> Parser Char [] String
levelId n = consumed $ many (token (intToDigit n))

levelLine :: Int -> Parser Char [] String
levelLine n = consumed $ some $
  (levelId n) <* newline <|>
  (string "def") <* space <* (many alphaNum) <* newline <*
    (level (succ n) (levelLine (succ n)))

level :: Int -> Parser Char [] String -> Parser Char [] String
level n p = consumed $ do
  next <- delegateWhile p (neg newline)
  (replicateM_ (2 * n) space) <* level n next

tests =
  [ testCase "empty doesn't accept anything" $ do
      assertReject "empty" "xxxx" empty
      assertReject "empty" "x" empty
      assertReject "empty" "empty" empty
      assertReject "empty" "" empty
  , testCase "minus stuff" $ do
      let p = (token 'x' <|> token 'y') `minus` token 'x'
      assertReject "p" "x" p
      assertAccept "p" "y" p

      let q = (token 'x') `minus` (token 'x' *> token 'x')
      assertReject "q" "xx" q
      assertAccept "q" "x" q
      assertReject "q" "xy" q

      let r = many (token 'x')  `minus` (token 'x' *> token 'x')
      assertAccept "r" "" r
      assertAccept "r" "x" r
      assertReject "r" "xx" r
      assertAccept "r" "xxx" r
  , testCase "x^n y^n z^n" $ do
      let x = token 'x'
          y = token 'y'
          z = token 'z'
          repeat p q = pure 0 <|> p *> fmap succ (repeat p q) <* q
          xyz = (repeat x y <* many z) `intersect` (many x *> repeat y z)

      forM_ [0..4] $ \x -> do
        forM_ [0..4] $ \y -> do
          forM_ [0..4] $ \z -> do
            let txt = concat (zipWith replicate [x, y, z] "xyz")
            if x == y && y == z
              then assertAccept "xyz" txt xyz
              else assertReject "xyz" txt xyz
  , testCase "monadic ops" $ do
      let nstars = do
            num <- some (asum (fmap token "0123456789"))
            replicateM_ (read num) (token '*')
      assertAccept "nstars" "0" nstars
      assertAccept "nstars" "1*" nstars
      assertAccept "nstars" "2**" nstars
      assertAccept "nstars" "3***" nstars
      assertAccept "nstars" "9*********" nstars
      assertAccept "nstars" "10**********" nstars
      assertAccept "nstars" "11***********" nstars

      assertReject "nstars" "0*" nstars
      assertReject "nstars" "1**" nstars
      assertReject "nstars" "2***" nstars
      assertReject "nstars" "3****" nstars
      assertReject "nstars" "9**********" nstars
      assertReject "nstars" "10***********" nstars
      assertReject "nstars" "11************" nstars

      assertReject "nstars" "1" nstars
      assertReject "nstars" "2*" nstars
      assertReject "nstars" "3**" nstars
      assertReject "nstars" "9********" nstars
      assertReject "nstars" "10*********" nstars
      assertReject "nstars" "11**********" nstars
  , testCase "string" $ do
      let foo = string "foo"
      assertReject "string \"foo\"" "" foo
      assertReject "string \"foo\"" "f" foo
      assertReject "string \"foo\"" "fo" foo
      assertAccept "string \"foo\"" "foo" foo
      assertReject "string \"foo\"" "fooo" foo

      let epsilon = string ""
      assertAccept "string \"\"" "" epsilon
      assertReject "string \"\"" "foo" epsilon
  , testCase "<> operator" $ do
      let foo = string "foo"
          bar = string "bar"
      assertAccept "foo <> bar" "foobar" (foo <> bar)
      assertReject "foo <> bar" "foo" (foo <> bar)
      assertReject "foo <> bar" "bar" (foo <> bar)
  , testCase "foobarb" $ do
      let foobarb = (string "foobar" <|> string "foo") *> token 'b'
      assertAccept "foobarb" "foobarb" foobarb
      assertAccept "foobarb" "foob" foobarb
  , testCase "interleaving via delegateOnce" $ do
       let interleave p q = asum
             [ (,) <$> kill p <*> kill q
             , do p <- delegateOnce p
                  q <- delegateOnce q
                  interleave p q
             ]
       assertAccept "interleave (string \"foo\") (many (char '_'))"
         "f_o_o_" (interleave (string "foo") (many (char '_')))
       assertReject "interleave (string \"foo\") (many (char '_'))"
         "foo" (interleave (string "foo") (many (char '_')))
  , testCase "layout" $ do
      assertAccept "" "0000\n" (levelLine 0)
      assertAccept "" "0000\n\
                      \0000\n" (levelLine 0)
      assertReject "" "0000\n\
                      \ 0000\n" (levelLine 0)
      assertAccept "" "def foo\n\
                      \  11\n" (levelLine 0)
  ]
