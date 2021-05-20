module Producer
  (Producer(..)
  , producer
  , lift
  , liftRefEq
  , produce
  )
  where

import MasonPrelude

import RefEq (RefEq(..), (===))

foreign import unsafeEqImpl :: ∀ a b. (a -> a -> Boolean) -> a -> b -> Boolean

unsafeEq :: ∀ a b. Eq a => a -> b -> Boolean
unsafeEq = unsafeEqImpl eq

newtype Producer a = Producer (∀ r. (∀ b. Eq b => (b -> a) /\ b -> r) -> r)

instance functorProducer :: Functor Producer where
  map f p = producer mapHelper $ RefEq f /\ p

instance applyProducer :: Apply Producer where
  apply pf pa = producer applyHelper $ pf /\ pa

mapHelper :: ∀ a b. RefEq (a -> b) /\ Producer a -> b
mapHelper ((RefEq f) /\ p) = f $ produce p

applyHelper :: ∀ a b. Producer (a -> b) /\ Producer a -> b
applyHelper (pf /\ pa) = produce pf $ produce pa

instance eqProducer :: Eq (Producer a) where
  eq (Producer p1) (Producer p2) =
    p1
      \(f1 /\ b1) ->
        p2
          \(f2 /\ b2) ->
            if f1 === f2 then
              unsafeEq b1 b2
            else
              false

producer :: ∀ a b. Eq b => (b -> a) -> b -> Producer a
producer f b = Producer (_ $ f /\ b)

lift :: ∀ a. Eq a => a -> Producer a
lift = producer identity

-- | The producer will be equal to other Producers if what they produce is referentially equal
liftRefEq :: ∀ a. a -> Producer a
liftRefEq = producer unRefEq <. RefEq

unRefEq :: ∀ a. RefEq a -> a
unRefEq (RefEq a) = a

produce :: ∀ a. Producer a -> a
produce (Producer p) = p \(f /\ b) -> f b
