{- ORACLE_TEST pass -}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NPlusKPatternsTypedPatBind where

a + 0 :: Int = 5

b + 0 : bs :: [Int] = [5]

n + 1 = n
