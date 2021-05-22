module Test.Main where

import MasonPrelude hiding (log)

import Effect.Console (log)
import Producer
  ( producer
  , producer2
  , producer3
  , producer4
  , producer5
  , producer6
  , produce
  , lift
  , liftRefEq
  )
import Test.Assert (assert', assertFalse')

plus1 :: Int -> Int
plus1 = (_ + 1)

add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c

add4 :: Int -> Int -> Int -> Int -> Int
add4 a b c d = a + b + c + d

add5 :: Int -> Int -> Int -> Int -> Int -> Int
add5 a b c d e = a + b + c + d + e

add6 :: Int -> Int -> Int -> Int -> Int -> Int -> Int
add6 a b c d e f = a + b + c + d + e + f

main :: Effect Unit
main = do
  assert' "1" $ producer log "test" == producer log "test"
  assertFalse' "2" $ producer log "best" == producer log "test"
  assert' "3" $ (plus1 <$> lift 5) == (plus1 <$> lift 5)
  assert' "4" $ (produce $ plus1 <$> lift 5) == 6
  assert' "5" $ liftRefEq plus1 == liftRefEq plus1
  assertFalse' "6" $ liftRefEq plus1 == liftRefEq (_ + 1)
  assert' "7" $ producer2 (+) 1 2 == producer2 (+) 1 2
  assertFalse' "8" $ producer2 (+) 1 2 == producer2 (+) 2 2
  assert' "9" $ producer3 add3 1 2 3 == producer3 add3 1 2 3
  assertFalse' "10" $ producer3 add3 1 2 3 == producer3 add3 2 2 3
  assert' "11" $ producer4 add4 1 2 3 4 == producer4 add4 1 2 3 4
  assertFalse' "12" $ producer4 add4 1 2 3 4 == producer4 add4 2 2 3 4
  assert' "13" $ producer5 add5 1 2 3 4 5 == producer5 add5 1 2 3 4 5
  assertFalse' "14" $ producer5 add5 1 2 3 4 5 == producer5 add5 2 2 3 4 5
  assert' "15" $ producer6 add6 1 2 3 4 5 6 == producer6 add6 1 2 3 4 5 6
  assertFalse' "16" $ producer6 add6 1 2 3 4 5 6 == producer6 add6 2 2 3 4 5 6
  assert' "17" $ (liftRefEq plus1 <*> lift 1)  == (liftRefEq plus1 <*> lift 1)
  assertFalse' "18" $ (liftRefEq plus1 <*> lift 1)  == (liftRefEq plus1 <*> lift 2)
