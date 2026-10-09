module Array.Immutable
(   Array
,   empty
,   fromList
,   size
,   get
,   append
,   set
,   swap
) where

import qualified Data.Vector as V

newtype Array a = Array (V.Vector a)

empty :: Array a
empty = Array V.empty

fromList :: [a] -> Array a
fromList list = Array (V.fromList list)

size :: Array a -> Int
size (Array vec) = V.length vec

get :: Int -> Array a -> Maybe a
get idx (Array vec) = vec V.!? idx

append :: a -> Array a -> Array a
append val (Array vec) = Array (V.snoc vec val)

set :: Int -> a -> Array a -> Maybe (Array a)
set idx val (Array vec) = setInner (vec V.!? idx) where
    setInner Nothing = Nothing
    setInner (Just _) = Just (Array (vec V.// [(idx, val)]))

swap :: Int -> Int -> Array a -> Maybe (Array a)
swap idxA idxB (Array vec) = swapInner (vec V.!? idxA) (vec V.!? idxB) where
    swapInner (Just valA) (Just valB) = Just (Array (vec V.// [(idxB, valA), (idxA, valB)]))
    swapInner _ _ = Nothing