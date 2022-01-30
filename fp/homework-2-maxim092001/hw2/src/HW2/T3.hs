module HW2.T3 ( joinOption
              , joinExcept
              , joinAnnotated
              , joinList
              , joinFun
              ) where

import HW2.T1
import HW2.T2 (mergeList)


-- The following laws are hold:
--
-- • Associativity:
--
--     joinF (mapF joinF m)  ≡  joinF (joinF m)
-- In other words, given F (F (F a)), it does not matter whether we join the outer layers or the inner layers first.
-- • Left and right identity:
--
--  joinF      (wrapF m)  ≡  m
--  joinF (mapF wrapF m)  ≡  m
--
--  In other words, layers created by wrapF are identity elements to joinF.

joinOption :: Option (Option a) -> Option a
joinOption (Some x) = x
joinOption _        = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success ex) = ex
joinExcept (Error e)    = Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# ei) :# eo) = a :# (eo <> ei)

joinList :: List (List a) -> List a
joinList Nil      = Nil
joinList (h :. t) = mergeList h (joinList t)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> unwrapF (f i) i) where
  unwrapF (F g) = g
