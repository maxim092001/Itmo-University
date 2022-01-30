module HW2.T5 ( ExceptState (..)
              , mapExceptState
              , wrapExceptState
              , joinExceptState
              , modifyExceptState
              , throwExceptState
              , EvaluationError (..)
              , evalBinaryExprExceptState
              , eval
              ) where

import Control.Monad
import HW2.T1
import HW2.T2
import HW2.T4 hiding (eval)

newtype ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f exSt = ES $ mapExcept (mapAnnotated f) . runES exSt

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> wrapExcept (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState st = ES $ \s ->
  case runES st s of
    Error e -> Error e
    Success (a :# s') -> runES a s'

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero deriving Show


-- ^ Evaluate binary expression function for @ExceptState@ monad
evalBinaryExprExceptState
  :: Expr
  -> Expr
  -> (Double -> Double -> (Prim Double, Double))
  -> ExceptState EvaluationError [Prim Double] Double
evalBinaryExprExceptState x y primAndDoubleOp = evalBinaryExpr x y primAndDoubleOp eval modifyExceptState

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double

eval (Op (Div e1 e2)) = do
  x <- eval e1
  y <- eval e2
  when (y == 0) $
    throwExceptState DivideByZero
  modifyExceptState (Div x y :)
  return (x / y)

eval expr = evalExpr expr evalBinaryExprExceptState eval modifyExceptState
