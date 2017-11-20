{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module IntervalTree where

import FingerTree (Measured, μ, FingerTree, View(..), viewL, Split(..), splitTree, takeUntil, dropUntil)
import PQueue (Prio(..))
import OrdSeq (Key(..))

data Interval = Interval {low :: Double, high :: Double}

newtype IntervalTree = IntervalTree (FingerTree (Key Double, Prio Double) Interval)

instance Measured Interval (Key Double, Prio Double) where
    μ i = (Key (low i), Prio (high i))

atleast, greater :: Double -> (Key Double, Prio Double) -> Bool
atleast k (_,n) = Prio k <= n
greater k (n,_) = Key k < n

intervalSearch :: IntervalTree -> Interval -> Maybe Interval
intervalSearch (IntervalTree t) i
    | atleast (low i) (μ t) && low x <= high i = Just x
    | otherwise = Nothing
    where Split _ x _ = splitTree (atleast (low i)) mempty t

intervalMatch :: IntervalTree -> Interval -> [Interval]
intervalMatch (IntervalTree t) i = matches (takeUntil (greater (high i)) t)
    where matches xs = case viewL (dropUntil (atleast (low i)) xs) of
            Nil -> []
            Cons x xs -> x : matches xs