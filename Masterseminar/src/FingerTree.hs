{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes, UnicodeSyntax #-}

module FingerTree where

import Data.Monoid ((<>))
import Data.Foldable (toList)

infixl 5 <||
infixr 5 ||>
infix 5 ><
infixl 5 .+|
infixr 5 |+.

class Extendable a t where
    infixr 6 <|
    infixl 6 |>
    (<|) :: a -> t a -> t a 
    (|>) :: t a -> a -> t a

instance Extendable a [] where
    (<|) = (:)
    xs |> x = xs ++ [x]

data View s a = Nil | Cons a (s a)
    deriving (Eq, Ord, Show)

class Viewable a t where
    viewL :: t a -> View t a
    viewR :: t a -> View t a

instance Viewable a [] where
    viewL [] = Nil
    viewL (x:xs) = Cons x xs
    viewR [] = Nil
    viewR xs = Cons (last xs) (init xs)

class Monoid v => Measured a v where
    μ :: a -> v

(.+|) :: Measured a v => v -> a -> v
i .+| s = i <> μ s

(|+.) :: Measured a v => a -> v -> v
s |+. i = μ s <> i

data Digit a =
      One a
    | Two a a
    | Three a a a
    | Four a a a a
    deriving (Eq, Ord, Show)

instance Foldable Digit where
    foldMap f (One a) = f a
    foldMap f (Two a b) = f a <> f b
    foldMap f (Three a b c) = f a <> f b <> f c
    foldMap f (Four a b c d) = f a <> f b <> f c <> f d

instance Measured a v => Measured (Digit a) v where
    μ = foldl (.+|) mempty

instance Extendable a Digit where
    a <| One b = Two a b
    a <| Two b c = Three a b c
    a <| Three b c d = Four a b c d

    One a |> b = Two a b
    Two a b |> c = Three a b c
    Three a b c |> d = Four a b c d

instance Viewable a Digit where
    viewL (One _) = Nil
    viewL (Two a b) = Cons a (One b)
    viewL (Three a b c) = Cons a (Two b c)
    viewL (Four a b c d) = Cons a (Three b c d)

    viewR (One _) = Nil
    viewR (Two a b) = Cons b (One a)
    viewR (Three a b c) = Cons c (Two a b)
    viewR (Four a b c d) = Cons d (Three a b c)

data Node v a = Node2 v a a | Node3 v a a a
    deriving (Eq, Ord, Show)

instance Monoid v => Foldable (Node v) where
    foldMap f (Node2 _ a b) = f a <> f b
    foldMap f (Node3 _ a b c) = f a <> f b <> f c

instance Measured a v => Measured (Node v a) v where
    μ (Node2 s _ _) = s
    μ (Node3 s _ _ _) = s

instance Measured a v => Extendable a (Node v) where
    a <| Node2 s b c = Node3 (a |+. s) a b c
    Node2 s a b |> c = Node3 (s .+| c) a b c

node2 :: Measured a v => a -> a -> Node v a
node2 a b = Node2 (μ a .+| b) a b

node3 :: Measured a v => a -> a -> a -> Node v a
node3 a b c = Node3 (μ a .+| b .+| c) a b c

data FingerTree v a =
      Empty
    | Single a
    | Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)
    deriving (Eq, Ord, Show)

instance Monoid v => Foldable (FingerTree v) where
    foldMap _ Empty = mempty
    foldMap f (Single a) = f a
    foldMap f (Deep _ pr m sf) = fm pr <> foldMap (foldMap f) m <> fm sf
        where fm = foldMap f

instance Measured a v => Measured (FingerTree v a) v where
    μ Empty = mempty
    μ (Single x) = μ x
    μ (Deep s _ _ _) = s

deep :: Measured a v => Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep pr m sf = Deep (μ pr .+| m .+| sf) pr m sf

instance Measured a v => Extendable a (FingerTree v) where
    a <| Empty = Single a
    a <| Single b = deep (One a) Empty (One b)
    a <| Deep _ (Four b c d e) m sf = deep (Two a b) (node3 c d e <| m) sf
    a <| Deep _ pr m sf = deep (a <| pr) m sf

    Empty |> a = Single a
    Single a |> b = deep (One a) Empty (One b)
    Deep _ pr m (Four a b c d) |> e = deep pr (m |> node3 a b c) (Two d e)
    Deep _ pr m sf |> a = deep pr m (sf |> a)

(<||) :: (Foldable f, Extendable a g) => f a -> g a -> g a
(<||) = flip (foldr (<|))

(||>) :: (Foldable f, Extendable a g) => g a -> f a -> g a
(||>) = foldl (|>)

toTree :: (Foldable f, Measured a v) => f a -> FingerTree v a 
toTree s = s <|| Empty

nodeTodigit :: Node v a -> Digit a
nodeTodigit (Node2 _ a b) = Two a b
nodeTodigit (Node3 _ a b c) = Three a b c

instance Measured a v => Viewable a (FingerTree v) where
    viewL Empty = Nil
    viewL (Single x) = Cons x Empty
    viewL (Deep _ pr m sf) = case viewL pr of
        Nil -> let One a = pr in Cons a (case viewL m of
            Nil -> toTree sf
            Cons b m -> deep (nodeTodigit b) m sf)
        Cons a pr -> Cons a (deep pr m sf)

    viewR Empty = Nil
    viewR (Single x) = Cons x Empty
    viewR (Deep _ pr m sf) = case viewR sf of
        Nil -> let One a = sf in Cons a (case viewR m of
            Nil -> toTree pr
            Cons b m -> deep pr m (nodeTodigit b))
        Cons a sf -> Cons a (deep pr m sf)    

isEmpty :: Monoid v => FingerTree v a -> Bool
isEmpty = null

headL :: Measured a v => FingerTree v a -> a
headL x = case viewL x of Cons a _ -> a

headR :: Measured a v => FingerTree v a -> a
headR x = case viewR x of Cons a _ -> a

tailL :: Measured a v => FingerTree v a -> FingerTree v a
tailL x = case viewL x of Cons _ x -> x

tailR :: Measured a v => FingerTree v a -> FingerTree v a
tailR x = case viewR x of Cons _ x -> x

foldablesToList :: Foldable t => [t a] -> [a]
foldablesToList = concatMap toList

nodes :: Measured a v => [a] -> [Node v a]
nodes [a,b] = [node2 a b]
nodes [a,b,c] = [node3 a b c]
nodes [a,b,c,d] = [node2 a b, node2 c d]
nodes (a:b:c:xs) = node3 a b c : nodes xs

app3 :: Measured a v => FingerTree v a -> [a] -> FingerTree v a -> FingerTree v a
app3 Empty ts xs = ts <|| xs
app3 xs ts Empty = xs ||> ts
app3 (Single x) ts xs = x <| (ts <|| xs)
app3 xs ts (Single x) = (xs ||> ts) |> x
app3 (Deep _ pr1 m1 sf1) ts (Deep _ pr2 m2 sf2) =
    deep pr1 (app3 m1 (nodes (sf1 <|| (ts ||> pr2))) m2) sf2
    
(><) :: Measured a v => FingerTree v a -> FingerTree v a -> FingerTree v a
xs >< ys = app3 xs [] ys

data Split f a = Split (f a) a (f a)
    deriving (Eq, Ord, Show)

splitDigit :: Measured a v => (v -> Bool) -> v -> Digit a -> Split [] a
splitDigit _ _ (One a) =  Split [] a []
splitDigit p i d
    | p i' = Split [] a (toList as)
    | otherwise = let Split l x r = splitDigit p i' as in Split (a <| l) x r
    where
        Cons a as = viewL d
        i' = i .+| a

deepL :: Measured a v => [a] -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deepL [] m sf = case viewL m of
        Nil -> toTree sf
        Cons a m -> deep (nodeTodigit a) m sf
deepL (p:pr) m sf = deep (One p ||> pr) m sf

deepR :: Measured a v => Digit a -> FingerTree v (Node v a) -> [a] -> FingerTree v a
deepR pr m [] = case viewR m of
        Nil -> toTree pr
        Cons a m -> deep pr m (nodeTodigit a)
deepR pr m (s:sf) = deep pr m (One s ||> sf)

splitTree :: Measured a v => (v -> Bool) -> v -> FingerTree v a -> Split (FingerTree v) a
splitTree _ _ (Single x) = Split Empty x Empty
splitTree p i (Deep _ pr m sf)
    | p vpr = let Split l x r = splitDigit p i pr in Split (toTree l) x (deepL r m sf)
    | p vm = let
        Split ml xs mr = splitTree p vpr m
        Split l x r = splitDigit p (vpr .+| ml) (nodeTodigit xs)
    in
        Split (deepR pr ml l) x (deepL r mr sf)
    | otherwise = let Split l x r = splitDigit p vm sf in Split (deepR pr m l) x (toTree r)
    where
        vpr = i .+| pr
        vm = vpr .+| m

split :: Measured a v => (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
split _ Empty = (Empty, Empty)
split p xs
    | p (μ xs) = let Split l x r = splitTree p mempty xs in (l, x <| r)
    | otherwise = (xs, Empty)

takeUntil, dropUntil :: Measured a v => (v -> Bool) -> FingerTree v a -> FingerTree v a
takeUntil p = fst . split p
dropUntil p = snd . split p

lookupDigit :: Measured a v => (v -> Bool) -> v -> Digit a -> (v, a)
lookupDigit p i d = case viewL d of
    Nil -> let One a = d in (i, a)
    Cons d ds -> let vd = i .+| d in if p vd then (i, d) else lookupDigit p vd ds 

lookupNode :: Measured a v => (v -> Bool) -> v -> Node v a -> (v, a)
lookupNode p i (Node2 _ a b)
    | p va = (i, a)
    | otherwise = (va, b)
    where va = i .+| a
lookupNode p i (Node3 _ a b c)
    | p va = (i, a)
    | p vb = (va, b)
    | otherwise = (vb, c)
    where
        va = i .+| a
        vb = va .+| b

lookupTree :: Measured a v => (v -> Bool) -> v -> FingerTree v a -> (v, a)
lookupTree _ i (Single x) = (i, x)
lookupTree p i (Deep _ pr m sf)
    | p vpr = lookupDigit p i pr
    | p vm = let (i', t) = lookupTree p vpr m in lookupNode p i' t
    | otherwise = lookupDigit p vm sf
    where
        vpr = i .+| pr
        vm = vpr .+| m

newtype Elem a = Elem { getElem :: a } deriving (Eq, Ord, Show)