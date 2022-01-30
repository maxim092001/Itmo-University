module HW1.T1
  ( Day (..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

import Numeric.Natural

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)

nextDay :: Day -> Day

nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

afterDays :: Natural -> Day -> Day
afterDays n d = afterDays' n d where
  afterDays' 0 d = d
  afterDays' n d = afterDays' (n - 1) (nextDay d)

isWeekend :: Day -> Bool
isWeekend d
  | (d == Saturday) || (d == Sunday) = True
  | otherwise = False

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty d      = 1 + daysToParty (nextDay d)
