module Test.Main where

import MasonPrelude hiding (log)

import Effect.Console (log)
import Producer (producer, produce, lift, liftRefEq)
import Test.Assert (assert', assertFalse')

plus1 :: Int -> Int
plus1 = (_ + 1)

main :: Effect Unit
main = do
  assert' "1" $ producer log "test" == producer log "test"
  assertFalse' "2" $ producer log "best" == producer log "test"
  assert' "3" $ (plus1 <$> lift 5) == (plus1 <$> lift 5)
  assert' "4" $ (produce $ plus1 <$> lift 5) == 6
  assert' "5" $ liftRefEq plus1 == liftRefEq plus1
  assertFalse' "6" $ liftRefEq plus1 == liftRefEq (_ + 1)
