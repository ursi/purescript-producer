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

producer2 :: ∀ a b c. Eq (b /\ c) => (b -> c -> a) -> b -> c -> Producer a
producer2 f b c = producer producer2Helper $ RefEq f /\ b /\ c

producer2Helper :: ∀ a b c. RefEq (b -> c -> a) /\ b /\ c -> a
producer2Helper (RefEq f /\ b /\ c) = f b c

producer3 :: ∀ a b c d. Eq (b /\ c /\ d) => (b -> c -> d -> a) -> b -> c -> d -> Producer a
producer3 f b c d = producer producer3Helper $ RefEq f /\ b /\ c /\ d

producer3Helper :: ∀ a b c d. RefEq (b -> c -> d -> a) /\ b /\ c /\ d -> a
producer3Helper (RefEq f /\ b /\ c /\ d) = f b c d

producer4 :: ∀ a b c d e.
  Eq (b /\ c /\ d /\ e)
  => (b -> c -> d -> e -> a)
  -> b -> c -> d -> e
  -> Producer a
producer4 f b c d e = producer producer4Helper $ RefEq f /\ b /\ c /\ d /\ e

producer4Helper :: ∀ a b c d e. RefEq (b -> c -> d -> e -> a) /\ b /\ c /\ d /\ e -> a
producer4Helper (RefEq f /\ b /\ c /\ d /\ e) = f b c d e

producer5 :: ∀ a b c d e f.
  Eq (b /\ c /\ d /\ e /\ f)
  => (b -> c -> d -> e -> f -> a)
  -> b -> c -> d -> e -> f
  -> Producer a
producer5 fn b c d e f = producer producer5Helper $ RefEq fn /\ b /\ c /\ d /\ e /\ f

producer5Helper :: ∀ a b c d e f.
  RefEq (b -> c -> d -> e -> f -> a) /\ b /\ c /\ d /\ e /\ f -> a
producer5Helper (RefEq fn /\ b /\ c /\ d /\ e /\ f) = fn b c d e f

producer6 :: ∀ a b c d e f g.
  Eq (b /\ c /\ d /\ e /\ f /\ g)
  => (b -> c -> d -> e -> f -> g -> a)
  -> b -> c -> d -> e -> f -> g
  -> Producer a
producer6 fn b c d e f g =
  producer producer6Helper
  $ RefEq fn /\ b /\ c /\ d /\ e /\ f /\ g

producer6Helper :: ∀ a b c d e f g.
  RefEq (b -> c -> d -> e -> f -> g -> a) /\ b /\ c /\ d /\ e /\ f /\ g -> a
producer6Helper (RefEq fn /\ b /\ c /\ d /\ e /\ f /\ g) = fn b c d e f g

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
