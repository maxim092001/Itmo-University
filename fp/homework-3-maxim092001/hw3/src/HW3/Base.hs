{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}

module HW3.Base ( HiFun (..)
                , HiExpr (..)
                , HiError (..)
                , HiValue (..)
                , HiMonad (..)
                , HiAction (..)
                , Arity (..)
                , SomeArityFun (..)
                , ArityFun (..)
                , ArityWrapper (..)
                , hiValueToArityFun
                , numeratorAndDenominator
                , getIntFromRational
                , decodeUtf8Bytes
                ) where

import Codec.Serialise
import Data.ByteString
import Data.Map
import Data.Ratio
import Data.Sequence
import Data.Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock
import GHC.Generics

data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

data HiValue =
    HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

data HiAction =
    HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq)

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorDivideByZero
  | HiErrorArityMismatch
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

-- Helper structures


-- | Arity ADT
data Arity =
    Zero
  | Unary
  | Binary
  | Ternary
  | Any

-- | Wrapper for 'ArityFun'.
--   With this wrapper we can match any 'ArityFun' and be sure of its arity.
data ArityWrapper a where
  B   :: ArityFun 'Binary  -> ArityWrapper SomeArityFun
  U   :: ArityFun 'Unary   -> ArityWrapper SomeArityFun
  T   :: ArityFun 'Ternary -> ArityWrapper SomeArityFun
  A   :: ArityFun 'Any     -> ArityWrapper SomeArityFun
  Z   :: ArityFun 'Zero    -> ArityWrapper SomeArityFun

-- | 'ArityFun' GADT.
--   In this GADT we define arity of each function/value.
data ArityFun (a :: Arity) where
  Add                 :: ArityFun 'Binary
  Sub                 :: ArityFun 'Binary
  Mul                 :: ArityFun 'Binary
  Div                 :: ArityFun 'Binary
  And                 :: ArityFun 'Binary
  Or                  :: ArityFun 'Binary
  Not                 :: ArityFun 'Unary
  LessThan            :: ArityFun 'Binary
  GreaterThan         :: ArityFun 'Binary
  Equals              :: ArityFun 'Binary
  NotLessThan         :: ArityFun 'Binary
  NotGreaterThan      :: ArityFun 'Binary
  NotEquals           :: ArityFun 'Binary
  If                  :: ArityFun 'Ternary
  Length              :: ArityFun 'Unary
  ToUpper             :: ArityFun 'Unary
  ToLower             :: ArityFun 'Unary
  Reverse             :: ArityFun 'Unary
  Trim                :: ArityFun 'Unary
  List                :: ArityFun 'Any
  Range               :: ArityFun 'Binary
  Fold                :: ArityFun 'Binary
  Num                 :: Rational -> ArityFun 'Zero
  StringSliceOrIndex  :: Text -> ArityFun 'Any
  Null                :: ArityFun 'Zero
  Bool                :: ArityFun 'Zero
  ListSliceOrIndex    :: Seq HiValue -> ArityFun 'Any
  PackBytes           :: ArityFun 'Unary
  UnpackBytes         :: ArityFun 'Unary
  EncodeUtf8          :: ArityFun 'Unary
  DecodeUtf8          :: ArityFun 'Unary
  Zip                 :: ArityFun 'Unary
  Unzip               :: ArityFun 'Unary
  Serialise           :: ArityFun 'Unary
  Deserialise         :: ArityFun 'Unary
  BytesSliceOrIndex   :: ByteString -> ArityFun 'Any
  Read                :: ArityFun 'Unary
  Write               :: ArityFun 'Binary
  MkDir               :: ArityFun 'Unary
  ChDir               :: ArityFun 'Unary
  Action              :: HiAction -> ArityFun 'Zero
  ParseTime           :: ArityFun 'Unary
  Rand                :: ArityFun 'Binary
  Time                :: UTCTime -> ArityFun 'Zero
  Echo                :: ArityFun 'Unary
  Count               :: ArityFun 'Unary
  Keys                :: ArityFun 'Unary
  Values              :: ArityFun 'Unary
  Invert              :: ArityFun 'Unary
  Dict                :: Map HiValue HiValue -> ArityFun 'Unary

data SomeArityFun where
  SomeArityFun :: forall (a :: Arity). ArityFun a -> SomeArityFun

-- Helper functions

-- | Convert any 'HiValue' to 'ArityWrapper SomeArityFun'
hiValueToArityFun :: HiValue -> ArityWrapper SomeArityFun
hiValueToArityFun (HiValueFunction f) = case f of
                                        HiFunAdd            -> B Add
                                        HiFunSub            -> B Sub
                                        HiFunDiv            -> B Div
                                        HiFunMul            -> B Mul
                                        HiFunOr             -> B Or
                                        HiFunAnd            -> B And
                                        HiFunNot            -> U Not
                                        HiFunLessThan       -> B LessThan
                                        HiFunGreaterThan    -> B GreaterThan
                                        HiFunEquals         -> B Equals
                                        HiFunNotLessThan    -> B NotLessThan
                                        HiFunNotGreaterThan -> B NotGreaterThan
                                        HiFunNotEquals      -> B NotEquals
                                        HiFunIf             -> T If
                                        HiFunLength         -> U Length
                                        HiFunToUpper        -> U ToUpper
                                        HiFunToLower        -> U ToLower
                                        HiFunReverse        -> U Reverse
                                        HiFunTrim           -> U Trim
                                        HiFunList           -> A List
                                        HiFunRange          -> B Range
                                        HiFunFold           -> B Fold
                                        HiFunPackBytes      -> U PackBytes
                                        HiFunUnpackBytes    -> U UnpackBytes
                                        HiFunEncodeUtf8     -> U EncodeUtf8
                                        HiFunDecodeUtf8     -> U DecodeUtf8
                                        HiFunZip            -> U Zip
                                        HiFunUnzip          -> U Unzip
                                        HiFunSerialise      -> U Serialise
                                        HiFunDeserialise    -> U Deserialise
                                        HiFunRead           -> U Read
                                        HiFunWrite          -> B Write
                                        HiFunMkDir          -> U MkDir
                                        HiFunChDir          -> U ChDir
                                        HiFunParseTime      -> U ParseTime
                                        HiFunRand           -> B Rand
                                        HiFunEcho           -> U Echo
                                        HiFunCount          -> U Count
                                        HiFunKeys           -> U Keys
                                        HiFunValues         -> U Values
                                        HiFunInvert         -> U Invert
hiValueToArityFun (HiValueNumber n) = Z $ Num n
hiValueToArityFun (HiValueString s) = A $ StringSliceOrIndex s
hiValueToArityFun HiValueNull       = Z Null
hiValueToArityFun (HiValueBool _)   = Z Bool
hiValueToArityFun (HiValueList seq) = A $ ListSliceOrIndex seq
hiValueToArityFun (HiValueBytes b)  = A $ BytesSliceOrIndex b
hiValueToArityFun (HiValueAction a) = Z $ Action a
hiValueToArityFun (HiValueTime t)   = Z $ Time t
hiValueToArityFun (HiValueDict d)   = U $ Dict d

numeratorAndDenominator :: Rational -> (Integer, Integer)
numeratorAndDenominator x = (numerator x, denominator x)

getIntFromRational :: Rational -> Maybe Int
getIntFromRational x = case numeratorAndDenominator x of
                         (n, 1) | n >= toInteger (minBound @Int) && n <= toInteger (maxBound @Int) -> Just $ fromInteger n
                         _      -> Nothing

decodeUtf8Bytes :: ByteString -> HiValue
decodeUtf8Bytes s = either (const HiValueNull) HiValueString (decodeUtf8' s)
