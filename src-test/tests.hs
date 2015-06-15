import Control.Applicative
import Control.Monad

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.CoalgebraicParsing

main = defaultMain tests

assertAccept :: String -> String -> Parser Char [] a -> Assertion
assertAccept msg text p = do
  when (null (parse p text)) $ do
    assertString (msg ++ " rejects " ++ show text ++ ".\nShould accept.")

assertReject :: String -> String -> Parser Char [] a -> Assertion
assertReject msg text p = do
  unless (null (parse p text)) $ do
    assertString (msg ++ " accepts " ++ show text ++ ".\nShould reject.")

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
  ]
