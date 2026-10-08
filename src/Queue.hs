module Queue
  ( Queue
  , empty
  , enqueue
  , dequeue
  , peek
  , isEmpty
  , size
  ) where

data Queue a
    = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue [] []) = Queue (x : []) []
enqueue x (Queue front back) = Queue front (x : back)

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue (x : []) back) = Just (x, Queue (myReverse back) [])
dequeue (Queue (x : front) back) = Just (x, Queue front back)

peek :: Queue a -> Maybe a
peek (Queue [] []) = Nothing
peek (Queue (first : _) _) = Just first

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

size :: Queue a -> Int
size (Queue front back) = (countItems front) + (countItems back) where
    countItems :: [a] -> Int
    countItems items = foldr (\_ acc -> 1 + acc) 0 items

myReverse :: [a] -> [a]
myReverse items = go items [] where
    go :: [a] -> [a] -> [a]
    go [] reversed = reversed
    go (x : xs) reversed = go xs (x : reversed)
