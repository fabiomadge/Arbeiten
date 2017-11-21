{-# LANGUAGE MultiParamTypeClasses #-}

module Test where

import Test.QuickCheck
import Data.Foldable as F (toList)
import Data.List as List (sortOn) 
import FingerTree
import qualified Seq
import qualified PQueue

instance Monoid Integer where
    mempty = 0
    mappend = (+)

instance Measured Integer Integer where
    μ = id

prop_to_from :: [Integer] -> Bool
prop_to_from xs = xs == (toList . (toTree:: [Integer] -> FingerTree Integer Integer)) xs

prop_tail :: [Integer] -> Bool
prop_tail [] = True
prop_tail xs = tail xs == (toList . tailL. (toTree:: [Integer] -> FingerTree Integer Integer)) xs

prop_init :: [Integer] -> Bool
prop_init [] = True
prop_init xs = init xs == (toList . tailR. (toTree:: [Integer] -> FingerTree Integer Integer)) xs

prop_concat :: [Integer] -> [Integer] -> Bool
prop_concat xs ys = xs ++ ys == toList ((toTree:: [Integer] -> FingerTree Integer Integer) xs FingerTree.>< (toTree:: [Integer] -> FingerTree Integer Integer) ys)

breakTree :: (Integer -> Bool) -> [Integer] -> (FingerTree Integer Integer, FingerTree Integer Integer)
breakTree p ns = split p ((toTree:: [Integer] -> FingerTree Integer Integer) ns)

prop_index :: [Integer] -> [Integer] -> Bool
prop_index xs ys = xs == Seq.toList xs' && ys == Seq.toList ys'
    where (xs', ys') = Seq.splitAt ((toInteger . Prelude.length) xs) (Seq.fromList xs Seq.>< Seq.fromList ys)

prop_sorted :: [Integer] -> Bool
prop_sorted xs = List.sortOn (0 -) xs == sorted xs
    where
        popAll q = case PQueue.extractMax q of
            Nothing -> []
            Just (p,q) -> p : popAll q 
        pushAll = foldr PQueue.insert PQueue.empty
        sorted = popAll . pushAll