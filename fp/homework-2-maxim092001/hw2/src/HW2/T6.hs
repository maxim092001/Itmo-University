{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module HW2.T6 ( Parser (..)
               , ParseError (..)
               , runP
               , pChar
               , parserError
               , pEof
               , pAbbr
               , parseExpr
               , expr
               , pE
               , pE'
               , pT
               , pT'
               , pF
               , pCharPredicate
               , pCharEq
               , pSpace
               , strToInt
               , pStrInt
               , pDouble
               ) where

import Control.Applicative
import Control.Monad
import Data.Char (digitToInt, isDigit, isSpace, isUpper)
import Data.Functor
import Data.Scientific
import HW2.T1 (Annotated (..), Except (..), mapExcept)
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5
import Numeric.Natural

newtype ParseError = ErrorAtPos Natural deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a) deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P excSt) s = case runES excSt (0, s) of
                     Error e          -> Error e
                     Success (a :# _) -> Success a


-- 1) For an empty string we will get @ErrorAtPos@ in first case.
-- 2) Consuming new character we create pair with incremented pos and tail of given string.
-- As result we put our current char.
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []       -> Error (ErrorAtPos pos)
    (c : cs) -> Success(c :# (pos + 1, cs))

parserError :: Parser a
parserError = P $ ES $ \(pos, _) -> Error $ ErrorAtPos pos

instance Alternative Parser where

  empty :: Parser a
  empty = parserError

  (<|>) :: Parser a -> Parser a -> Parser a
  (P pSt) <|> (P qSt)  = P $ ES $ \(pos, s) -> case runES pSt (pos, s) of
                                    Success x -> Success x
                                    Error _ -> runES qSt (pos, s)

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _  -> Error $ ErrorAtPos pos

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter Data.Char.isUpper pChar)
  pEof
  pure abbr

parseExpr :: String -> Except ParseError Expr
parseExpr = runP expr

-- Helper functions for expression parser
-- Parser grammar:
-- expr -> E
-- E    -> TE'
-- E'   -> ('+'|'-')TE'|eps
-- T    -> FT'
-- T'   -> ('*'|'/')FT'|eps
-- F    -> VAL|(E)
expr :: Parser Expr
expr = pSpace *> pE <* pSpace <* pEof

pE :: Parser Expr
pE = do
  t <- pSpace *> pT <* pSpace
  pE' t <|> pure t

pE' :: Expr -> Parser Expr
pE' acc = do
  op <- pSpace *> (pCharEq '+' <|> pCharEq '-') <* pSpace
  t <- pT <* pSpace
  res <- case op of
           '+' -> pure (Op (Add acc t))
           '-' -> pure (Op (Sub acc t))
           _  -> parserError
  pE' res <|> pure res

pT :: Parser Expr
pT = do
  f <- pSpace *> pF
  pT' f <|> pure f

pT' :: Expr -> Parser Expr
pT' acc = do
  op <- pSpace *> (pCharEq '*' <|> pCharEq '/') <* pSpace
  f <- pF <* pSpace
  res <- case op of
           '*' -> pure (Op (Mul acc f))
           '/' -> pure (Op (Div acc f))
           _   -> parserError
  pT' res <|> pure res

pF :: Parser Expr
pF = pSpace *> (fmap Val pDouble <|> (pCharEq '(' *> pE <* pCharEq ')'))

pCharPredicate :: (Char -> Bool) -> Parser Char
pCharPredicate predicate = mfilter predicate pChar

pCharEq :: Char -> Parser Char
pCharEq c = pCharPredicate (c ==)

-- Skip whitespaces
pSpace :: Parser String
pSpace = many (pCharPredicate isSpace)

-- Convert @String@ to @Integer@
strToInt :: String -> Integer
strToInt = foldl (\ acc c -> toInteger (digitToInt c) + 10 * acc) 0

pStrInt :: Parser String
pStrInt = some (pCharPredicate isDigit)

pDouble :: Parser Double
pDouble = do
  x <- pStrInt
  pCharEq '.'
  y <- pStrInt
  pure $ toRealFloat $ scientific (strToInt (x ++ y)) (-length y)
