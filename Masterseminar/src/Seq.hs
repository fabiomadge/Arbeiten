{-# LANGUAGE MultiParamTypeClasses #-}

module Seq where

import Prelude hiding (splitAt)
import FingerTree (Measured, μ, FingerTree(Empty), split, Split(..), splitTree, lookupTree, Elem(..), getElem)
import qualified FingerTree as F ((<|),(|>),(><))

-- random-access sequences

infixr 6 <|
infixl 6 |>
infix 5 ><

newtype Size = Size { getSize :: Integer } deriving (Eq, Ord, Show)

instance Monoid Size where
    mempty = Size 0
    Size a `mappend` Size b = Size (a + b)

newtype Seq a = Seq (FingerTree Size (Elem a)) deriving (Eq, Ord, Show)

instance Measured (Elem a) Size where
    μ _ = Size 1

empty :: Seq a
empty = Seq Empty

(|>) :: Seq a -> a -> Seq a
Seq xs |> a = Seq (xs F.|> Elem a)

(<|) :: a -> Seq a -> Seq a
a <| Seq xs = Seq (Elem a F.<| xs)

(><) :: Seq a -> Seq a -> Seq a
Seq xs >< Seq ys = Seq (xs F.>< ys)

length :: Seq a -> Integer
length (Seq xs) = getSize (μ xs)

(!) :: Seq a -> Integer -> a
Seq xs ! i = (getElem . snd) (lookupTree (Size i <) mempty xs)

splitAt :: Integer -> Seq a -> (Seq a, Seq a)
splitAt i (Seq xs) = (Seq l, Seq r)
    where (l, r) = split (Size i <) xs

insertAt :: Integer -> a -> Seq a -> Seq a
insertAt i x s = l |> x >< r
    where (l, r) = splitAt i s

deleteAt :: Integer -> Seq a -> Seq a
deleteAt i (Seq xs) = Seq (l F.>< r)
    where Split l x r = splitTree (Size i <) mempty xs

fromList :: [a] -> Seq a
fromList = foldl (flip (<|)) empty

toList :: Seq a -> [a]
toList (Seq s) = foldl (\l (Elem a) -> a : l) [] s