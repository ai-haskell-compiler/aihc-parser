{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}

module TransformCaseBeforeKeyword where

grouped xs = [x | x <- xs, then group by case x of _ -> x using groupWith]

sorted xs = [x | x <- xs, then sortWith by case x of _ -> x]
