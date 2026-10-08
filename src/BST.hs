module BST
(   BST
,   empty
,   insert
,   member
,   findMin
,   delete
,   toList
) where

data BST a
    = Empty
    | BST (BST a) a (BST a)

empty :: BST a
empty = Empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = BST Empty x Empty
insert x (BST left val right)
    | x == val = BST left val right
    | x < val = BST (insert x left) val right
    | otherwise = BST left val (insert x right)

member :: Ord a => a -> BST a -> Bool
member x Empty = False
member x (BST left val right)
    | x == val = True
    | x < val = member x left
    | otherwise = member x right

findMin :: BST a -> Maybe a
findMin Empty = Nothing
findMin (BST Empty val _) = Just val
findMin (BST left _ _) = findMin left

delete :: Ord a => a -> BST a -> BST a
delete x Empty = Empty
delete x (BST left val right)
    | x < val = (BST (delete x left) val right)
    | x > val = (BST left val (delete x right))
    | x == val = deleteNode (BST left val right) where
        deleteNode :: Ord a => BST a -> BST a
        deleteNode Empty = Empty
        deleteNode (BST Empty val Empty) = Empty
        deleteNode (BST left val Empty) = left
        deleteNode (BST Empty val right) = right
        deleteNode (BST left val right) = BST left successor (delete successor right) where
            successor = unwrap (findMin right)

unwrap :: Maybe a -> a
unwrap Nothing = error "called unwrap on Nothing"
unwrap (Just x) = x

toList :: BST a -> [a]
toList tree = go tree [] where
    go :: BST a -> [a] -> [a]
    go Empty xs = xs
    go (BST left val right) xs = go left (val : (go right xs))