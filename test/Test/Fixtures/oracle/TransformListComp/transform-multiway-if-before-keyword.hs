{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TransformListComp #-}

module TransformMultiWayIfBeforeKeyword where

grouped xs = [x | x <- xs, then group by if | x -> x using groupWith]

sorted xs = [x | x <- xs, then sortWith by if | x -> x]
