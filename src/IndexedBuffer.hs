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