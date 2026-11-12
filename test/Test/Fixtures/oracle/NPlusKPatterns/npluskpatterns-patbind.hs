{- ORACLE_TEST pass -}
{-# LANGUAGE NPlusKPatterns #-}

module NPlusKPatternsPatBind where

(m+1) = 3

n + 1 = n

value = xs
  where
    (k+1 : xs) = [3, 4]
