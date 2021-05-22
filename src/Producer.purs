module Producer
  ( Producer(..)
  , class Producible
  , produce
  , producer
  , producer2
  , producer3
  , producer4
  , producer5
  , producer6
  , lift
  , liftRefEq
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

producer2 :: ∀ a b c. Eq b => Eq c => (b -> c -> a) -> b -> c -> Producer a
producer2 f b c = liftRefEq f <*> lift b <*> lift c

producer3 :: ∀ a b c d.
  Eq b => Eq c => Eq d
  => (b -> c -> d -> a)
  -> b -> c -> d
  -> Producer a
producer3 f b c d = liftRefEq f <*> lift b <*> lift c <*> lift d

producer4 :: ∀ a b c d e.
  Eq b => Eq c => Eq d => Eq e
  => (b -> c -> d -> e -> a)
  -> b -> c -> d -> e
  -> Producer a
producer4 f b c d e = liftRefEq f <*> lift b <*> lift c <*> lift d <*> lift e

producer5 :: ∀ a b c d e f.
  Eq b => Eq c => Eq d => Eq e => Eq f
  => (b -> c -> d -> e -> f -> a)
  -> b -> c -> d -> e -> f
  -> Producer a
producer5 fn b c d e f =
  liftRefEq fn
  <*> lift b <*> lift c <*> lift d <*> lift e <*> lift f

producer6 :: ∀ a b c d e f g.
  Eq b => Eq c => Eq d => Eq e => Eq f => Eq g
  => (b -> c -> d -> e -> f -> g -> a)
  -> b -> c -> d -> e -> f -> g
  -> Producer a
producer6 fn b c d e f g =
  liftRefEq fn
  <*> lift b <*> lift c <*> lift d <*> lift e <*> lift f <*> lift g

lift :: ∀ a. Eq a => a -> Producer a
lift = producer identity

-- | The producer will be equal to other Producers if what they produce is referentially equal
liftRefEq :: ∀ a. a -> Producer a
liftRefEq = producer unRefEq <. RefEq

unRefEq :: ∀ a. RefEq a -> a
unRefEq (RefEq a) = a

class Producible a b | a -> b where
  produce :: a -> b

instance producibleProducer :: Producible (Producer a) a where
  produce (Producer p) = p \(f /\ b) -> f b
else instance producibleA :: Producible a a where
  produce = identity
