{- ORACLE_TEST pass -}
{-# LANGUAGE NPlusKPatterns #-}

module NPlusKPatternsFunArg where

takeN 0 _ = []
takeN (n+1) (x:xs) = x : takeN n xs
