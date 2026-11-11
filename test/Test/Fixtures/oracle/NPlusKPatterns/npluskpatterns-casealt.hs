{- ORACLE_TEST pass -}
{-# LANGUAGE NPlusKPatterns #-}

module NPlusKPatternsCaseAlt where

predecessor x = case x of
  0 -> 0
  n+1 -> n
