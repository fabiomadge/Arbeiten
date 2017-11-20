{-# LANGUAGE MultiParamTypeClasses #-}

module PQueue where

import FingerTree (Measured, μ, FingerTree(Empty), (<|), (><), Split(..), splitTree, Elem(..), getElem)

data Prio a = MInfty | Prio a deriving (Eq, Ord, Show)

instance (Ord a) => Monoid (Prio a) where
    mempty = MInfty
    MInfty `mappend` p = p
    p `mappend` MInfty = p
    Prio m `mappend` Prio n = Prio (max m n)

newtype PQueue a = PQueue (FingerTree (Prio a) (Elem a)) deriving (Eq, Ord, Show)

instance (Ord a) => Measured (Elem a) (Prio a) where
    μ (Elem x) = Prio x

extractMax :: Ord a => PQueue a -> Maybe (a, PQueue a)
extractMax (PQueue Empty) = Nothing
extractMax (PQueue q) = Just (x, PQueue (l >< r))
    where Split l (Elem x) r = splitTree (μ q <=) mempty q

empty :: Ord a => PQueue a
empty = PQueue Empty

insert :: Ord a => a -> PQueue a -> PQueue a
insert x (PQueue q) = PQueue (Elem x <| q)