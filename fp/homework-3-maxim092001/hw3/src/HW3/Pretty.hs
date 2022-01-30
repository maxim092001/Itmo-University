{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HW3.Pretty (prettyValue) where

import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import Data.Foldable
import Data.List hiding (group)
import qualified Data.Map as MP
import Data.Ratio
import Data.Scientific
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock
import Data.Word
import HW3.Base
import Numeric
import Prettyprinter
import Prettyprinter.Internal hiding (group)
import Prettyprinter.Render.Terminal

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueList l)     = prettyHiList $ toList l
prettyValue HiValueNull         = "null"
prettyValue (HiValueString t)   = viaShow t
prettyValue (HiValueBool b)     = prettyBoolValue b
prettyValue (HiValueFunction f) = prettyFunctionValue f
prettyValue (HiValueNumber v)   = prettyNumberValue v
prettyValue (HiValueTime t)     = prettyTime t
prettyValue (HiValueAction a)   = prettyAction a
prettyValue (HiValueBytes s)    = prettyByteStringValue s
prettyValue (HiValueDict d)     = prettyHiDict d

prettyHiList :: [HiValue] -> Doc AnsiStyle
prettyHiList [] = "[]"
prettyHiList l = encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (prettyValue <$> l)

prettyHiDict :: MP.Map HiValue HiValue -> Doc AnsiStyle
prettyHiDict = \case
  m | null m -> "{}"
  m -> encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) ((\case (k,v) -> prettyValue k <> ":" <+> prettyValue v) <$> MP.assocs m)

prettyBoolValue :: Bool -> Doc AnsiStyle
prettyBoolValue b = if b then "true" else "false"

prettyFunctionValue :: HiFun -> Doc AnsiStyle
prettyFunctionValue f = (pretty @String) (case f of
  HiFunDiv            -> "div"
  HiFunMul            -> "mul"
  HiFunSub            -> "sub"
  HiFunAdd            -> "add"
  HiFunNot            -> "not"
  HiFunAnd            -> "and"
  HiFunOr             -> "or"
  HiFunLessThan       -> "less-than"
  HiFunGreaterThan    -> "greater-than"
  HiFunEquals         -> "equals"
  HiFunNotLessThan    -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals      -> "not-equals"
  HiFunIf             -> "if"
  HiFunLength         -> "length"
  HiFunToUpper        -> "to-upper"
  HiFunToLower        -> "to-lower"
  HiFunReverse        -> "reverse"
  HiFunTrim           -> "trim"
  HiFunList           -> "list"
  HiFunFold           -> "fold"
  HiFunRange          -> "range"
  HiFunPackBytes      -> "pack-bytes"
  HiFunUnpackBytes    -> "unpack-bytes"
  HiFunEncodeUtf8     -> "encode-utf8"
  HiFunDecodeUtf8     -> "decode-utf8"
  HiFunZip            -> "zip"
  HiFunUnzip          -> "unzip"
  HiFunSerialise      -> "serialise"
  HiFunDeserialise    -> "deserialise"
  HiFunRead           -> "read"
  HiFunWrite          -> "write"
  HiFunChDir          -> "cd"
  HiFunMkDir          -> "mkdir"
  HiFunParseTime      -> "parse-time"
  HiFunEcho           -> "echo"
  HiFunRand           -> "rand"
  HiFunCount          -> "count"
  HiFunKeys           -> "keys"
  HiFunValues         -> "values"
  HiFunInvert         -> "invert"
                               )

prettyNumberValue :: Rational -> Doc AnsiStyle
prettyNumberValue x = case numeratorAndDenominator x of
                       (n, 1) -> pretty n
                       (n, d) -> prettyDecimal (x, n, d)

-- Divide by 2 and 5 done to check finite/infinite number
prettyDecimal :: (Rational, Integer, Integer) -> Doc AnsiStyle
prettyDecimal (x, n, d) = if divUntilBy (divUntilBy d 2) 5 == 1 then
                            (pretty @String) $ showFFloat Nothing (toRealFloat $ fst $ fromRationalRepetendUnlimited x) ""
                          else prettyFractional d (quotRem n d)

prettyFractional :: Integer -> (Integer, Integer) -> Doc AnsiStyle
prettyFractional d (0, x) = pretty x <> slash <> pretty d
prettyFractional d (x, y) = pretty x <+> (pretty @String) (if y > 0 then "+" else "-") <+> prettyFractional d (0, abs y)

prettyByteStringValue :: BS.ByteString -> Doc AnsiStyle
prettyByteStringValue s = hsep ([lbracket <> "#"] ++ (pretty <$> (word8ToHexString <$> BS.unpack s)) ++ ["#" <> rbracket])

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction a = case a of
                    HiActionRead f      -> "read" <> lparen <> viaShow f <> rparen
                    HiActionWrite f bs  -> "write" <> lparen <> viaShow f <> comma <+> prettyByteStringValue bs <> rparen
                    HiActionMkDir f     -> "mkdir" <> lparen <> viaShow f <> rparen
                    HiActionChDir f     -> "cd" <> lparen <> viaShow f <> rparen
                    HiActionCwd         -> "cwd"
                    HiActionNow         -> "now"
                    HiActionRand x y    -> "rand" <> lparen <> viaShow x <> comma <+> viaShow y <> rparen
                    HiActionEcho t      -> "echo" <> lparen <> viaShow t <> rparen

prettyTime :: UTCTime -> Doc AnsiStyle
prettyTime t = "parse-time" <> lparen <> dquote <> viaShow t <> dquote <> rparen

word8ToHexString :: Word8 -> String
word8ToHexString w | w < 16 = "0" ++ showHex w ""
            | otherwise = showHex w ""

divUntilBy :: Integer -> Integer -> Integer
divUntilBy n x
  | n `mod` x == 0 = divUntilBy (n `div` x) x
  | otherwise = n
