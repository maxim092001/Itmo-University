{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HW3.Parser (parse) where

import Control.Applicative as AP
import Control.Monad
import Control.Monad.Combinators.Expr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Internal
import Data.List (intercalate)
import Data.Scientific
import Data.Sequence (fromList)
import qualified Data.Text as T
import Data.Void
import Data.Word
import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> parseExpr <* eof) ""

parseExpr :: Parser HiExpr
parseExpr = makeExprParser parseHiExpr operatorTable

-- | Expression parser.
--   This is the main function in parser, it parses values like:
--   "{value}[{()}, {.}, {!}, {()}, ... ]".
--   Parentheses, dots and exclamation marks may be in any order.
--   This is done via 'rcParens', 'rcDot', 'rcEx' functions.
parseHiExpr :: Parser HiExpr
parseHiExpr = do
  op <- pTerm
  do
    parens <- rcParens op
    dots <- rcDot parens
    rcEx dots

parseHiExprValue :: Parser HiExpr
parseHiExprValue = (HiExprValue <$> parensV parseHiValue) <|> parseHiList <|> parseDict

parseParamsList :: Parser [HiExpr]
parseParamsList = parseExpr `sepBy` symbol ","

parseDict :: Parser HiExpr
parseDict = HiExprDict <$> (parensV . dictParens . lexeme) (liftA2 (,) (lexeme $ parseExpr <* symbol ":") parseExpr `sepBy` symbol ",")

parseHiValue :: Parser HiValue
parseHiValue =
      parseHiValueBool
  <|> parseHexValue
  <|> parseHiValueNumber
  <|> parseHiValueFun
  <|> parseHiValueString
  <|> parseHiValueAction

parseHiValueAction :: Parser HiValue
parseHiValueAction = HiValueAction <$> lexeme (choice
  [ HiActionCwd <$ "cwd"
  , HiActionNow <$ "now"
  ])

parseHiValueBool :: Parser HiValue
parseHiValueBool = lexeme $ choice
  [ HiValueBool True  <$ string "true"
  , HiValueBool False <$ string "false"
  ]

parseHiValueNumber :: Parser HiValue
parseHiValueNumber = lexeme $ HiValueNumber . toRational <$> L.signed sc L.scientific

parseHiValueFun :: Parser HiValue
parseHiValueFun = HiValueFunction <$> lexeme (choice
  [ HiFunDiv            <$ string "div"
  , HiFunMul            <$ string "mul"
  , HiFunAdd            <$ string "add"
  , HiFunSub            <$ string "sub"
  , HiFunAnd            <$ string "and"
  , HiFunOr             <$ string "or"
  , HiFunLessThan       <$ string "less-than"
  , HiFunGreaterThan    <$ string "greater-than"
  , HiFunEquals         <$ string "equals"
  , HiFunNotLessThan    <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals      <$ string "not-equals"
  , HiFunIf             <$ string "if"
  , HiFunNot            <$ string "not"
  , HiFunLength         <$ string "length"
  , HiFunToUpper        <$ string "to-upper"
  , HiFunToLower        <$ string "to-lower"
  , HiFunReverse        <$ string "reverse"
  , HiFunTrim           <$ string "trim"
  , HiFunList           <$ string "list"
  , HiFunRange          <$ string "range"
  , HiFunFold           <$ string "fold"
  , HiFunPackBytes      <$ string "pack-bytes"
  , HiFunUnpackBytes    <$ string "unpack-bytes"
  , HiFunEncodeUtf8     <$ string "encode-utf8"
  , HiFunDecodeUtf8     <$ string "decode-utf8"
  , HiFunZip            <$ string "zip"
  , HiFunUnzip          <$ string "unzip"
  , HiFunSerialise      <$ string "serialise"
  , HiFunDeserialise    <$ string "deserialise"
  , HiFunRead           <$ string "read"
  , HiFunWrite          <$ string "write"
  , HiFunMkDir          <$ string "mkdir"
  , HiFunChDir          <$ string "cd"
  , HiFunParseTime      <$ string "parse-time"
  , HiFunRand           <$ string "rand"
  , HiFunEcho           <$ string "echo"
  , HiFunCount          <$ string "count"
  , HiFunKeys           <$ string "keys"
  , HiFunValues         <$ string "values"
  , HiFunInvert         <$ string "invert"
  ])

parseHiValueString :: Parser HiValue
parseHiValueString = lexeme $ choice
  [ HiValueNull   <$ string "null"
  , HiValueString . T.pack <$> stringLiteral
  ]

parseHiList :: Parser HiExpr
parseHiList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> (parensV . listParens . lexeme) parseParamsList

parseHexValue :: Parser HiValue
parseHexValue = do
  x <- tagParens $ lexeme $ sepEndBy hex space1
  pure $ HiValueBytes $ BS.pack $ (fromInteger @Word8) <$> x

-- Helper functions

-- | Parse only two-digit hexadecimal values.
hex :: Parser Integer
hex = do
  d1 <- hexDigitChar
  d2 <- hexDigitChar
  let x = [d1, d2]
  pure $ read ("0x" ++ x)

-- | Parser terms.
--   (HiExpr)
--   HiValue
pTerm :: Parser HiExpr
pTerm = choice
  [ parens parseExpr
  , parseHiExprValue
  ]

hiFunToApply :: HiFun -> HiExpr -> HiExpr -> HiExpr
hiFunToApply f e1 e2 = HiExprApply (HiExprValue $ HiValueFunction f) [e1, e2]

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binaryL "*"  $ hiFunToApply HiFunMul
    , InfixL       $ hiFunToApply HiFunDiv <$ (lexeme . try) (string "/" <* notFollowedBy (satisfy (== '=')))
    ]
  , [ binaryL "+"  $ hiFunToApply HiFunAdd
    , binaryL "-"  $ hiFunToApply HiFunSub
    ]
  , [ binaryN "<="  $ hiFunToApply HiFunNotGreaterThan
    , binaryN "<"   $ hiFunToApply HiFunLessThan
    , binaryN ">="  $ hiFunToApply HiFunNotLessThan
    , binaryN ">"   $ hiFunToApply HiFunGreaterThan
    , binaryN "=="  $ hiFunToApply HiFunEquals
    , binaryN "/="  $ hiFunToApply HiFunNotEquals
    ]
  , [ binaryR "&&" $ hiFunToApply HiFunAnd ]
  , [ binaryR "||" $ hiFunToApply HiFunOr ]
  ]


binaryL :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryL name f = InfixL (f <$ symbol name)

binaryN :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryN name f = InfixN (f <$ symbol name)

binaryR :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryR name f = InfixR (f <$ symbol name)


-- Parentheses functions.

-- | Parentheses function for value. Made for constructions like (((...(v)...)))
parensV :: Parser a -> Parser a
parensV p = symbol "(" *> parensV p <* symbol ")" <|> p

parens :: Parser a -> Parser a
parens = symbolParens "(" ")"

listParens :: Parser a -> Parser a
listParens = symbolParens "[" "]"

tagParens :: Parser a -> Parser a
tagParens = symbolParens "[#" "#]"

dictParens :: Parser a -> Parser a
dictParens = symbolParens "{" "}"

symbolParens :: String -> String -> Parser a -> Parser a
symbolParens o e = between (symbol o) (symbol e)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parser for strings.
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- | Identifier parser. Used only for dot access
parseId :: Parser String
parseId = intercalate "-" <$> (((:) <$> letterChar <*> AP.many alphaNumChar) `sepBy1` "-")

-- Recursive parsers for multi-usage identifiers.
-- This parsers allows to parse constructions like "read("x").v!.text!"

rcParens :: HiExpr -> Parser HiExpr
rcParens p = (do
  v <- HiExprApply p <$> parens parseParamsList
  rcParens v) <|> pure p

rcDot :: HiExpr -> Parser HiExpr
rcDot p = (do
  v <- HiExprApply p . (\x -> [(HiExprValue . HiValueString . T.pack) x]) <$> (char '.' *> parseId)
  rcDot v) <|> rcParens p

rcEx :: HiExpr -> Parser HiExpr
rcEx p = (do
  v <- HiExprRun p <$ symbol "!"
  rcEx v) <|> (rcDot =<< rcParens p)
