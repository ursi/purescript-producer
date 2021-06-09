module Producer
  ( Producer(..)
  , class Produce
  , lift
  , produce
  , producer
  , producer2
  , producer3
  , producer4
  , producer5
  , producer6
  )
  where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import RefEq (RefEq(..), (===))

foreign import unsafeEqImpl :: ∀ a b. (a -> a -> Boolean) -> a -> b -> Boolean

unsafeEq :: ∀ a b. Eq a => a -> b -> Boolean
unsafeEq = unsafeEqImpl eq

-- https://thimoteus.github.io/posts/2018-09-21-existential-types.html
newtype Producer a = Producer (∀ r. (∀ b. Eq b => (b -> a) /\ b -> r) -> r)

instance Functor Producer where
  map f p = producer mapHelper $ RefEq f /\ p

mapHelper :: ∀ a b. RefEq (a -> b) /\ Producer a -> b
mapHelper ((RefEq f) /\ p) = f $ produce p

{-
An apply instance can be created, however, it fails to satisfy this test

(producer add 1 <*> lift 2)  == (producer2 add 1 2)

which  is unacceptable. Therefore, it it not included. It's also worth noting that the producerN functions, when created using <*>, are much less performant (unverified)

instance applyProducer :: Apply Producer where
  apply pf pa = producer applyHelper $ pf /\ pa

applyHelper :: ∀ a b. Producer (a -> b) /\ Producer a -> b
applyHelper (pf /\ pa) = produce pf $ produce pa
-}

instance Eq (Producer a) where
  eq (Producer p1) (Producer p2) =
    p1
      \(f1 /\ b1) ->
        p2
          \(f2 /\ b2) ->
            if f1 === f2 then
              -- b1 and b2 must be the same type because f1 and f2 are the same function
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

class Produce a b | a -> b where
  lift :: a -> Producer b
  produce :: a -> b

-- | For the `Produce (a -> b) (a -> b)` instance of `lift`, the output will be equal to other `Producer`s if the functions are referentially equal. This documentation is here because apparently else instances can't get their own documentation.
instance Produce (Producer a) a where
  lift = identity
  produce (Producer p) = p \(f /\ b) -> f b
else instance Produce (a -> b) (a -> b) where
  lift = producer unRefEq <<< RefEq
  produce = identity
else instance Eq a => Produce a a where
  lift = producer identity
  produce = identity

unRefEq :: ∀ a. RefEq a -> a
unRefEq (RefEq a) = a
