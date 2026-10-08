module Stack
(   Stack
,   empty
,   push
,   pop
,   peek
,   isEmpty
,   size
) where

data Stack a
    = Stack [a]

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs)
    = Stack (x : xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x : xs)) = Just (x, Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x : _)) = Just x

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty (Stack _) = False

size :: Stack a -> Int
size (Stack stack) = foldr (\_ acc -> 1 + acc) 0 stack