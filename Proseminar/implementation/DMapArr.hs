module DMapArr (DMap, empty, member, rearrange, insert, lookupWithB, toList) where

import Data.Array
import Data.Maybe

data DMap a b = DMap (Array a (Maybe b), Array b (Maybe a))

instance (Ix a, Show a, Show b) => Show (DMap a b) where
  show (DMap (a,_)) = show a

empty :: Int -> DMap Int Int
empty i = DMap (ept, ept)
  where
    ept = array (1,i) (zip [1..i] (replicate i Nothing))

lookupWithA :: Ix a => Ix b => a -> DMap a b -> Maybe (a,b)
lookupWithA k (DMap (a,_)) = a ! k >>= (\x -> Just (k, x))

lookupWithB :: Ix a => Ix b => b -> DMap a b -> Maybe (a,b)
lookupWithB k (DMap (_,b)) = b ! k >>= (\x -> Just (x, k))

insert :: Ix a => Ix b => (a,b) -> DMap a b -> DMap a b
insert (a,b) (DMap (am,bm)) = DMap (am // [(a, Just b)], bm // [(b, Just a)])

delete :: Ix a => Ix b => (a,b) -> DMap a b -> DMap a b
delete (a,b) (DMap (am,bm)) = DMap (am // [(a, Nothing)], bm // [(b, Nothing)])

rearrange :: Ix a => Ix b => (a,b) -> DMap a b -> DMap a b
rearrange p@(a,b) = insert p . deleteWithB b . deleteWithA a

deleteWithA :: Ix a => Ix b => a -> DMap a b -> DMap a b
deleteWithA a m = maybe m (`delete` m) (lookupWithA a m)

deleteWithB :: Ix a => Ix b => b -> DMap a b -> DMap a b
deleteWithB b m = maybe m (`delete` m) (lookupWithB b m)

toList :: Ix a => Ix b => DMap a b -> [(a, b)]
toList (DMap (a,_)) = map (\(a,Just b) -> (a, b)) $ filter (isJust . snd) $ assocs a

member :: Ix b => b -> DMap a b -> Bool
member k (DMap (_,b)) = isJust $ b ! k
