module HW2.T4 ( State (..)
              , mapState
              , wrapState
              , joinState
              , modifyState
              , Prim (..)
              , Expr (..)
              , evalBinaryExpr
              , evalUnaryExpr
              , eval
              , evalExpr
              , evalBinaryExprState
              ) where

import Control.Monad
import HW2.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f st = S $ mapAnnotated f . runS st

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState st = S $ \s ->
  let a :# s' = runS st s
  in runS a s'

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum
  deriving (Show)

data Expr = Val Double | Op (Prim Expr) deriving (Show)

instance Num Expr where
  x + y = Op $ Add x y
  x - y = Op $ Sub x y
  x * y = Op $ Mul x y
  signum x = Op $ Sgn x
  abs x = Op $ Abs x
  fromInteger x = Val $ fromInteger x

instance Fractional Expr where
  x / y = Op $ Div x y
  fromRational x = Val $ fromRational x

-- | Evaluate binary expression into @Monad m => m Double@
evalBinaryExpr
  :: Monad m
  => Expr -- ^ Left expression
  -> Expr -- ^ Right expression
  -> (Double -> Double -> (Prim Double, Double)) -- ^ Binary double operation
  -> (Expr -> m Double) -- ^ Evaluate expression
  -> (([Prim Double] -> [Prim Double]) -> m ()) -- ^ Modify state
  -> m Double
evalBinaryExpr e1 e2 primAndDouble ev modify = do
  x <- ev e1
  y <- ev e2
  let (p, d) = primAndDouble x y
  modify (p :)
  pure d

-- | Evaluate unary expression into @Monad m => m Double@
evalUnaryExpr
  :: Monad m
  => Expr -- ^ Expression
  -> (Double -> (Prim Double, Double)) -- ^ Unary double operation
  -> (Expr -> m Double) -- ^ Evaluate expression
  -> (([Prim Double] -> [Prim Double]) -> m ()) -- ^ Modify state
  -> m Double

evalUnaryExpr e primAndDouble ev modify = do
  x <- ev e
  let (p, d) = primAndDouble x
  modify (p :)
  pure d

evalExpr
  :: Monad m
  => Expr -- ^ Expression to evaluate
  -> (Expr -> Expr -> (Double -> Double -> (Prim Double, Double)) -> m Double) -- ^ Binary expression evaluate function
  -> (Expr -> m Double) -- ^ Evaluate expression function
  -> (([Prim Double] -> [Prim Double]) -> m ()) -- ^ Modify state
  -> m Double

evalExpr (Val x) _ _ _               = return x

evalExpr (Op (Add x y)) binaryOp _ _ = binaryOp x y (\ d1 d2 -> (Add d1 d2, d1 + d2))

evalExpr (Op (Sub x y)) binaryOp _ _ = binaryOp x y (\ d1 d2 -> (Sub d1 d2, d1 - d2))

evalExpr (Op (Mul x y)) binaryOp _ _ = binaryOp x y (\ d1 d2 -> (Mul d1 d2, d1 * d2))

evalExpr (Op (Div x y)) binaryOp _ _ = binaryOp x y (\ d1 d2 -> (Div d1 d2, d1 / d2))

evalExpr (Op (Abs x)) _ e modify     = evalUnaryExpr x (\d -> (Abs d, abs d)) e modify

evalExpr (Op (Sgn x)) _ e modify     = evalUnaryExpr x (\d -> (Sgn d, signum d)) e modify

-- ^ Evalute function for @State@ monad
evalBinaryExprState
  :: Expr
  -> Expr
  -> (Double -> Double -> (Prim Double, Double))
  -> State [Prim Double] Double
evalBinaryExprState x y primAndDouble = evalBinaryExpr x y primAndDouble eval modifyState

eval :: Expr -> State [Prim Double] Double
eval expr = evalExpr expr evalBinaryExprState eval modifyState
