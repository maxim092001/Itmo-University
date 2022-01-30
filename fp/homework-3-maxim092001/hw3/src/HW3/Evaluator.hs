{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HW3.Evaluator (eval) where

import Codec.Compression.Zlib (CompressParams (..), bestCompression, compressWith, decompressWith,
                               defaultCompressParams, defaultDecompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Applicative
import Control.Monad ((>=>))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Char (isSpace)
import Data.Foldable (foldl1, toList)
import Data.Ix
import qualified Data.List as LS
import qualified Data.Map as MP
import Data.Maybe (fromMaybe)
import Data.Semigroup (stimes)
import Data.Sequence ((><))
import qualified Data.Sequence as SQ
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock
import Data.Word
import HW3.Action
import HW3.Base
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (eval' expr)

eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' e = case e of
            HiExprValue v        -> pure v
            HiExprApply f params -> evalApply f params
            HiExprRun e          -> evalAction e
            HiExprDict l         -> evalDict l

evalApply :: HiMonad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evalApply f params = do
  fu <- evalHiFun f
  evalApplyFun (hiValueToArityFun fu) params

evalHiFun :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalHiFun (HiExprValue f)   = pure f
evalHiFun (HiExprApply e p) = evalApply e p
evalHiFun (HiExprRun e)     = evalAction e
evalHiFun (HiExprDict l)    = evalDict l

evalApplyFun :: HiMonad m => ArityWrapper SomeArityFun -> [HiExpr] -> ExceptT HiError m HiValue
evalApplyFun (B f) = evalBinary f
evalApplyFun (U f) = evalUnary  f
evalApplyFun (T f) = evalTernary f
evalApplyFun (A f) = evalAny f
evalApplyFun (Z f) = \_ -> throwE HiErrorInvalidFunction

evalAction :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalAction e = do
  e <- eval' e
  evalAction' e

evalAction' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalAction' = \case
  HiValueAction a -> ExceptT (fmap Right (runAction a))
  _               -> throwE HiErrorInvalidArgument

-- | Evaluate unary functions
evalUnary :: HiMonad m => ArityFun Unary -> [HiExpr] -> ExceptT HiError m HiValue
evalUnary f [x] = do
  evalX <- eval' x
  evalUnary' f evalX
evalUnary _ _ = throwE HiErrorArityMismatch

evalUnary' :: HiMonad m => ArityFun Unary -> HiValue -> ExceptT HiError m HiValue
evalUnary' f x = case (x, f) of
    (HiValueBool b,   Not)        -> pure $ HiValueBool $ not b
    (HiValueString s, Length)     -> pure $ HiValueNumber $ toRational $ T.length s
    (HiValueString s, ToUpper)    -> pure $ HiValueString $ T.toUpper s
    (HiValueString s, ToLower)    -> pure $ HiValueString $ T.toLower s
    (HiValueString s, Reverse)    -> pure $ HiValueString $ T.reverse s
    (HiValueString s, Trim)       -> pure $ HiValueString $ T.strip s
    (HiValueList l, Reverse)      -> pure $ HiValueList $ SQ.reverse l
    (HiValueBytes s, Reverse)     -> pure $ HiValueBytes $ BS.reverse s
    (HiValueBytes s, Length)      -> pure $ HiValueNumber $ toRational $ BS.length s
    (HiValueList l, Length)       -> pure $ HiValueNumber $ toRational $ SQ.length l
    (HiValueList l, PackBytes)    -> packBytes $ toList l
    (HiValueBytes s, UnpackBytes) -> pure $ HiValueList $ SQ.fromList $ unpackBytes s
    (HiValueBytes s, Zip)         -> pure $ HiValueBytes $ toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } (fromStrict s)
    (HiValueBytes s, Unzip)       -> pure $ HiValueBytes $ toStrict $ decompressWith defaultDecompressParams (fromStrict s)
    (HiValueString t, EncodeUtf8) -> pure $ HiValueBytes $ encodeUtf8 t
    (HiValueBytes s, DecodeUtf8)  -> pure $ decodeUtf8Bytes s
    (v, Serialise)                -> pure $ HiValueBytes $ toStrict $ serialise v
    (v, Deserialise)              -> deserialiseHiValue v
    (HiValueString s, Read)       -> pure $ HiValueAction $ HiActionRead $ T.unpack s
    (HiValueString s, MkDir)      -> pure $ HiValueAction $ HiActionMkDir $ T.unpack s
    (HiValueString s, ChDir)      -> pure $ HiValueAction $ HiActionChDir $ T.unpack s
    (HiValueString s, ParseTime)  -> pure $ maybe HiValueNull HiValueTime ((readMaybe @UTCTime) $ T.unpack s)
    (HiValueString s, Echo)       -> pure $ HiValueAction $ HiActionEcho s
    (HiValueString s, Count)      -> pure $ HiValueDict $ MP.mapKeys (HiValueString . T.singleton) (MP.map HiValueNumber (count $ T.unpack s))
    (HiValueList l, Count)        -> pure $ HiValueDict $ MP.map HiValueNumber (count $ toList l)
    (HiValueBytes s, Count)       -> pure $ HiValueDict $ MP.mapKeys word8ToNum (MP.map HiValueNumber (count $ BS.unpack s))
    (HiValueDict d, Keys)         -> pure $ HiValueList $ SQ.fromList $ MP.keys d
    (HiValueDict d, Values)       -> pure $ HiValueList $ SQ.fromList $ MP.elems d
    (HiValueDict d, Invert)       -> pure $ HiValueDict $ MP.map (HiValueList . SQ.fromList) (invertDict d)
    (k, Dict d)                   -> pure $ fromMaybe HiValueNull (MP.lookup k d)
    _                             -> throwE HiErrorInvalidArgument

-- | Evaluate functions with 'Binary' arity.
--   'Or' and 'And' evaluates lazily
evalBinary :: HiMonad m => ArityFun Binary -> [HiExpr] -> ExceptT HiError m HiValue
evalBinary And [x, y] = do
  vx <- eval' x
  case vx of
    HiValueBool False -> pure vx
    HiValueNull       -> pure vx
    _                 -> eval' y
evalBinary Or [x, y] = do
  vx <- eval' x
  case vx of
    HiValueNull       -> eval' y
    HiValueBool False -> eval' y
    _                 -> pure vx
evalBinary f [x, y] = do
  evalX <- eval' x
  evalY <- eval' y
  evalBinary' f evalX evalY
evalBinary _ _ = throwE HiErrorArityMismatch

evalBinary' :: HiMonad m => ArityFun Binary -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalBinary' f x y = case (x, y, f) of
    (HiValueNumber vx, HiValueNumber  0, Div)             -> throwE HiErrorDivideByZero
    (HiValueNumber vx, HiValueNumber vy, Div)             -> pure $ HiValueNumber (vx / vy)
    (HiValueNumber vx, HiValueNumber vy, Mul)             -> pure $ HiValueNumber (vx * vy)
    (HiValueNumber vx, HiValueNumber vy, Sub)             -> pure $ HiValueNumber (vx - vy)
    (HiValueNumber vx, HiValueNumber vy, Add)             -> pure $ HiValueNumber (vx + vy)

    (bx, by, Equals)                                      -> do
                                                                  b <- evalEquals bx by
                                                                  pure $ HiValueBool b

    (bx, by, NotEquals)                                   -> do
                                                                  b <- evalEquals bx by
                                                                  pure $ HiValueBool $ not b

    (bx, by, LessThan)                                    -> do
                                                                  b <- evalLessThan bx by
                                                                  pure $ HiValueBool b

    (bx, by, GreaterThan)                                 -> do
                                                                  b <- evalLessThan bx by
                                                                  e <- evalEquals bx by
                                                                  pure $ HiValueBool (not b && not e)

    (bx, by, NotLessThan)                                 -> do
                                                                  b <- evalLessThan bx by
                                                                  pure $ HiValueBool (not b)

    (bx, by, NotGreaterThan)                              -> do
                                                                  b <- evalLessThan bx by
                                                                  e <- evalEquals bx by
                                                                  pure $ HiValueBool (b || e)
    (HiValueString sx, HiValueString sy, Add)           -> pure $ HiValueString (sx <> sy)
    (HiValueString s,  HiValueNumber v,  Mul) | v > 0   -> (\n -> HiValueString (stimes n s)) <$> getIntOrArgError v
    (HiValueString sx, HiValueString sy, Div)           -> pure $ HiValueString (sx <> "/" <> sy)
    (HiValueNumber l, HiValueNumber r, Range)           -> pure $ HiValueList $ HiValueNumber <$> SQ.fromList [l..r]
    (f, HiValueList s, Fold)                            -> do
                                                          let arityFun = hiValueToArityFun f
                                                          case (arityFun, Data.Foldable.toList s) of

                                                            (B f, [])          -> pure HiValueNull
                                                            (B f, l)           -> foldl1 (\v1 v2 -> do
                                                                              x <- v1
                                                                              y <- v2
                                                                              evalApplyFun arityFun [HiExprValue x, HiExprValue y]
                                                                              ) (pure <$> l)
                                                            _                  -> throwE HiErrorInvalidFunction
    (HiValueList lx, HiValueList ly, Add)               -> pure $ HiValueList $ lx >< ly
    (HiValueList l, HiValueNumber v, Mul)     | v > 0   -> (\n -> HiValueList (stimes n l)) <$> getIntOrArgError v
    (HiValueBytes sx, HiValueBytes sy, Add)             -> pure $ HiValueBytes $ sx <> sy
    (HiValueBytes s, HiValueNumber v, Mul)    | v > 0   -> (\n -> HiValueBytes (stimes n s)) <$> getIntOrArgError v
    (HiValueString f, HiValueString t, Write)           -> pure $ HiValueAction $ HiActionWrite (T.unpack f) (encodeUtf8 t)
    (HiValueTime t, HiValueNumber n, Add)               -> pure $ HiValueTime $ addUTCTime (fromRational n) t
    (HiValueTime tx, HiValueTime ty, Sub)               -> pure $ HiValueNumber $ toRational $ diffUTCTime tx ty
    (HiValueNumber vx, HiValueNumber vy, Rand)          -> (\case (ix, iy) -> HiValueAction $ HiActionRand ix iy) <$> liftA2 (,) (getIntOrArgError vx) (getIntOrArgError vy)
    _                                                   -> throwE HiErrorInvalidArgument


-- | Evaluate functions with 'Ternary' arity
evalTernary :: HiMonad m => ArityFun Ternary -> [HiExpr] -> ExceptT HiError m HiValue
evalTernary f [x, y, z] = do
  evalX <- eval' x
  evalTernaryLazy f evalX y z
evalTernary _ _ = throwE HiErrorArityMismatch

evalTernaryLazy :: HiMonad m => ArityFun Ternary -> HiValue -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalTernaryLazy f x y z = case (x, y, z, f) of
  (HiValueBool b, first, second, If) -> eval' $ if b then first else second
  _                                  -> throwE HiErrorInvalidArgument


-- | Evaluate functions with 'Any' arity
evalAny :: HiMonad m => ArityFun Any -> [HiExpr] -> ExceptT HiError m HiValue
evalAny f l = do
  evalL <- traverse eval' l
  evalAny' f evalL

evalAny' :: HiMonad m => ArityFun Any -> [HiValue] -> ExceptT HiError m HiValue
evalAny' f l = case f of
    (StringSliceOrIndex s) -> case l of
                                [HiValueNumber x] -> maybe HiValueNull (HiValueString . T.singleton) <$> listIndex (T.unpack s) x
                                [x, y]            -> HiValueString . T.pack <$> getSlice (T.unpack s) x y
                                [x]               -> throwE HiErrorInvalidArgument
                                _                 -> throwE HiErrorArityMismatch
    List                   -> pure $ HiValueList $ SQ.fromList l
    (ListSliceOrIndex s)   -> case l of
                                [HiValueNumber x] -> fromMaybe  HiValueNull <$> listIndex (Data.Foldable.toList s) x
                                [x, y]            -> HiValueList . SQ.fromList <$> getSlice (Data.Foldable.toList s) x y
                                [x]               -> throwE HiErrorInvalidArgument
                                _                 -> throwE HiErrorArityMismatch
    (BytesSliceOrIndex s)  -> case l of
                                [HiValueNumber x] -> fromMaybe HiValueNull <$> listIndex (unpackBytes s) x
                                [x, y]            -> (>=>) (getSlice (unpackBytes s) x) packBytes y
                                [x]               -> throwE HiErrorInvalidArgument
                                _                 -> throwE HiErrorArityMismatch

evalDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m HiValue
evalDict l = HiValueDict .  MP.fromList <$> traverse (\case (e1, e2) -> liftA2 (,) (eval' e1) (eval' e2)) l

-- Helper functions

evalEquals :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m Bool
evalEquals x y = pure (x == y)

evalLessThan :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m Bool
evalLessThan x y = pure (x < y)

getIntOrArgError :: HiMonad m => Rational -> ExceptT HiError m Int
getIntOrArgError x = maybe (throwE HiErrorInvalidArgument) pure (getIntFromRational x)

listIndex :: HiMonad m => [a] -> Rational -> ExceptT HiError m (Maybe a)
listIndex l r = (\x -> if (x >= 0) && (x < LS.length l) then Just $ l !! x else Nothing) <$> getIntOrArgError r

getSlice :: HiMonad m => [a] -> HiValue -> HiValue -> ExceptT HiError m [a]
getSlice s (HiValueNumber x) HiValueNull       = (`getPrefixSlice` s) <$> getIntOrArgError x
getSlice s HiValueNull (HiValueNumber x)       = (`getSuffixSlice` s) <$> getIntOrArgError x
getSlice s (HiValueNumber x) (HiValueNumber y) = (\case (n, m) -> if m >= 0 && n >= 0 then slice n m s else getSuffixSlice m (getPrefixSlice n s)) <$> liftA2 (,) (getIntOrArgError x) (getIntOrArgError y)
getSlice s HiValueNull HiValueNull = pure s
getSlice _ _ _ = throwE HiErrorInvalidArgument

getPrefixSlice :: Int -> [a] -> [a]
getPrefixSlice x s = getS x s LS.drop takeEnd

getSuffixSlice :: Int -> [a] -> [a]
getSuffixSlice x s = getS x s LS.take dropEnd

getS :: Int -> [a] -> (Int -> [a] -> [a]) -> (Int -> [a] -> [a]) -> [a]
getS x t f g = (if x >= 0 then f else g) (abs x) t

slice :: Int -> Int -> [a] -> [a]
slice a b = LS.take (b - a) . LS.drop a

takeEnd :: Int -> [a] -> [a]
takeEnd i xs
    | i <= 0 = []
    | otherwise = f xs (drop i xs)
    where f (x : xs) (y : ys) = f xs ys
          f xs _              = xs

dropEnd :: Int -> [a] -> [a]
dropEnd i xs
    | i <= 0 = xs
    | otherwise = f xs (drop i xs)
    where f (x : xs) (y : ys) = x : f xs ys
          f _ _               = []

packBytes :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
packBytes l = HiValueBytes . BS.pack <$> traverse hiValueToWord8 l

unpackBytes :: BS.ByteString -> [HiValue]
unpackBytes s = word8ToNum <$> BS.unpack s

word8ToNum :: Word8 -> HiValue
word8ToNum = HiValueNumber . toRational . toInteger @Word8

hiValueToWord8 :: HiMonad m => HiValue -> ExceptT HiError m Word8
hiValueToWord8 = \case
   (HiValueNumber x) | x >= 0 && x <= 255 -> (fromIntegral @Int @Word8) <$> getIntOrArgError x
   _                                      -> throwE HiErrorInvalidArgument

deserialiseHiValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
deserialiseHiValue (HiValueBytes s) = do
  let x = deserialiseOrFail $ fromStrict s
  case x of
    Left _  -> throwE HiErrorInvalidArgument
    Right v -> pure v
deserialiseHiValue _                = throwE HiErrorInvalidArgument

count :: Ord k => [k] -> MP.Map k Rational
count s = MP.fromListWith (+) [(c, 1) | c <- s]

invertDict :: (Ord k, Ord v) => MP.Map k v -> MP.Map v [k]
invertDict m = MP.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, v) <- MP.toList m]
