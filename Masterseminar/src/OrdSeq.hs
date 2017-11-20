{-# LANGUAGE MultiParamTypeClasses #-}

module OrdSeq where

import FingerTree (Measured, μ, FingerTree, Elem(..), (><), (<|), split, View(..), viewL)

data Key a = NoKey | Key a deriving (Eq, Ord, Show)

instance Monoid (Key a) where
    mempty = NoKey
    k `mappend` NoKey = k
    _ `mappend` k = k

newtype OrdSeq a = OrdSeq (FingerTree (Key a) (Elem a))

instance Measured (Elem a) (Key a) where
    μ (Elem x) = Key x

partition :: Ord a => a -> OrdSeq a -> (OrdSeq a, OrdSeq a)
partition k (OrdSeq xs) = (OrdSeq l, OrdSeq r)
    where (l, r) = split (>= Key k) xs

insert :: Ord a => a -> OrdSeq a -> OrdSeq a
insert x (OrdSeq xs) = OrdSeq (l >< Elem x <| r)
    where (l, r) = split (>= Key x) xs

deleteAll :: Ord a => a -> OrdSeq a -> OrdSeq a
deleteAll x (OrdSeq xs) = OrdSeq (l >< r')
    where
        (l, r) = split (>= Key x) xs
        (_, r') = split (< Key x) r

merge :: Ord a => OrdSeq a -> OrdSeq a -> OrdSeq a
merge (OrdSeq xs) (OrdSeq ys) = OrdSeq (merge' xs ys)
    where merge' as bs = case viewL bs of
            Nil -> as
            Cons a bs -> l >< a <| merge' bs r
                where (l, r) = split (> μ a) as