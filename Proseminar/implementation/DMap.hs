module DMap (DMap, empty, member, rearrange, insert, lookupWithB, toList) where

import qualified Data.Map.Strict as Map
import Data.Maybe

data DMap a b = DMap (Map.Map a b, Map.Map b a)

instance (Show a, Show b) => Show (DMap a b) where
  show (DMap (a,_)) = show a

empty :: Int -> DMap a b
empty _ = DMap (Map.empty, Map.empty)

lookupWithA :: Ord a => Ord b => a -> DMap a b -> Maybe (a,b)
lookupWithA k (DMap (a,_)) = Map.lookup k a >>= (\x -> Just (k, x))

lookupWithB :: Ord a => Ord b => b -> DMap a b -> Maybe (a,b)
lookupWithB k (DMap (_,b)) = Map.lookup k b >>= (\x -> Just (x, k))

insert :: Ord a => Ord b => (a,b) -> DMap a b -> DMap a b
insert (a,b) (DMap (am,bm)) = DMap (Map.insert a b am, Map.insert b a bm)

delete :: Ord a => Ord b => (a,b) -> DMap a b -> DMap a b
delete (a,b) (DMap (am,bm)) = DMap (Map.delete a am, Map.delete b bm)

rearrange :: Ord a => Ord b => (a,b) -> DMap a b -> DMap a b
rearrange p@(a,b) = insert p . deleteWithB b . deleteWithA a

deleteWithA :: Ord a => Ord b => a -> DMap a b -> DMap a b
deleteWithA a m = maybe m (`delete` m) (lookupWithA a m)

deleteWithB :: Ord a => Ord b => b -> DMap a b -> DMap a b
deleteWithB b m = maybe m (`delete` m) (lookupWithB b m)

toList :: Ord a => Ord b => DMap a b -> [(a, b)]
toList (DMap (a,b)) = Map.toList a

member :: Ord b => b -> DMap a b -> Bool
member k (DMap (_,b)) = Map.member k b
