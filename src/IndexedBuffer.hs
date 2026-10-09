module IndexedBuffer
(   IndexedBuffer
,   empty
,   fromList
,   size
,   get 
) where

import qualified Data.Vector as V

newtype IndexedBuffer a = IndexedBuffer (V.Vector a)

empty :: IndexedBuffer a
empty = IndexedBuffer V.empty

fromList :: [a] -> IndexedBuffer a
fromList list = IndexedBuffer (V.fromList list)

size :: IndexedBuffer a -> Int
size (IndexedBuffer vec) = V.length vec

get :: Int -> IndexedBuffer a -> Maybe a
get idx (IndexedBuffer vec) = vec V.!? idx

append :: a -> IndexedBuffer a -> IndexedBuffer a
append val (IndexedBuffer vec) = IndexedBuffer (V.snoc vec val)

set :: Int -> a -> IndexedBuffer a -> Maybe (IndexedBuffer a)
set idx val (IndexedBuffer vec) = setInner (vec V.!? idx) where
    setInner Nothing = Nothing
    setInner (Just _) = Just (IndexedBuffer (vec V.// [(idx, val)]))

swap :: Int -> Int -> IndexedBuffer a -> Maybe (IndexedBuffer a)
swap idxA idxB (IndexedBuffer vec) = swapInner (vec V.!? idxA) (vec V.!? idxB) where
    swapInner (Just valA) (Just valB) = Just (IndexedBuffer (vec V.// [(idxB, valA), (idxA, valB)]))
    swapInner _ _ = Nothing