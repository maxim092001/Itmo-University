module HW2.T2 ( distOption
              , wrapOption
              , distPair
              , wrapPair
              , distQuad
              , wrapQuad
              , distAnnotated
              , wrapAnnotated
              , distExcept
              , wrapExcept
              , unwrapPrioritised
              , distPrioritised
              , wrapPrioritised
              , distStream
              , wrapStream
              , mergeList
              , distList
              , wrapList
              , distFun
              , wrapFun
              ) where

import HW2.T1

-- Following laws are hold by all dist*, wrap* functions:
--
-- • Homomorphism:
--     distF (wrapF a, wrapF b)  ≅  wrapF (a, b)
--
-- • Associativity:
--     distF (p, distF (q, r))   ≅  distF (distF (p, q), r)
--
-- • Left and right identity:
--     distF (wrapF (), q)  ≅  q
--     distF (p, wrapF ())  ≅  p
--
-- In the laws stated above, we reason up to the following isomorphisms:
--
--  ((a, b), c)  ≅  (a, (b, c))  -- for associativity
--      ((), b)  ≅  b            -- for left identity
--      (a, ())  ≅  a            -- for right identity

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

wrapPair :: a -> Pair a
wrapPair a = P a a

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# (e1 <> e2)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e

wrapExcept :: a -> Except e a
wrapExcept = Success

-- ^ Unwraps value inside the @Priroritised@
unwrapPrioritised :: Prioritised a -> a
unwrapPrioritised (High a)   = a
unwrapPrioritised (Medium a) = a
unwrapPrioritised (Low a)    = a

-- ^ Picks the highest priorities of two and returns a pair as an inside value
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, b)   = High (a, unwrapPrioritised b)
distPrioritised (a, High b)   = High (unwrapPrioritised a, b)
distPrioritised (Medium a, b) = Medium (a, unwrapPrioritised b)
distPrioritised (a, Medium b) = Medium (unwrapPrioritised a, b)
distPrioritised (a, b)        = Low (unwrapPrioritised a, unwrapPrioritised b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> ta, b :> tb) = (a, b) :> distStream (ta, tb)

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

-- ^ Simply merges two given lists
mergeList :: List a -> List a -> List a
mergeList Nil a       = a
mergeList a Nil       = a
mergeList (a :. ta) b = a :. mergeList ta b

-- ^ Associates each element of the first list with each element of the second list (i.e. the resulting list is of length n × m)
distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (a :. ta, b) = mergeList (loop a b) (distList (ta, b)) where
  loop :: a -> List b -> List (a, b)
  loop a l = case l of
               Nil         -> Nil
               (b' :. tb') -> (a, b') :. loop a tb'

wrapList :: a -> List a
wrapList x = x :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f1, F f2) = F (\i -> (f1 i, f2 i))

wrapFun :: a -> Fun i a
wrapFun x = F (const x)

