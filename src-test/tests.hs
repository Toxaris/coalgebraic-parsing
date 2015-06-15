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
    assertString ("parse " ++ msg ++ " " ++ show text)

assertReject :: String -> String -> Parser Char [] a -> Assertion
assertReject msg text p = do
  unless (null (parse p text)) $ do
    assertString ("parse " ++ msg ++ " " ++ show text)

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
  ]
