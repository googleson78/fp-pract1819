-- Resources:
--
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
--
-- https://www.seas.upenn.edu/~cis194/spring13/lectures/03-rec-poly.html
-- and
-- https://www.seas.upenn.edu/~cis194/spring13/lectures/05-type-classes.html
--
-- Prelude is a library that is always imported by default
-- we're going to hide Maybe from it ("(..)" means hide all the constructors of Maybe)
-- so we can implement our own
import Prelude hiding (Maybe(..))

-- the only possible implementation for a function of type a -> a
f :: a -> a
f x = x

-- no implementations for this type
--g :: a -> b

-- the only possible implementation for a function of type a -> b -> a
f3 :: a -> b -> a
f3 x _ = x

-- if we assume that f4 id xs == xs holds, then
-- map is the only possible implementation for this function
f4 :: (a -> b) -> [a] -> [b]
f4 = map

-- Our datatype that allows for the possibility of failure
data Maybe a
    = Nothing
    | Just a
    deriving (Show)

-- head that doesn't throw exceptions
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- another way to create a safe head, we only deal with lists that always have atleast one element
data NonEmpty a = NE a [a]
    deriving (Show)

safeHead' :: NonEmpty a -> a
safeHead' (NE x _) = x

-- "smart" constructor of NonEmptyes
nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty []     = Nothing
nonEmpty (x:xs) = Just (NE x xs)

-- another function that becomes exception-free with the use of NonEmpty
foldr1Safe :: (a -> a -> a) -> NonEmpty a -> a
foldr1Safe f (NE x xs) = foldr f x xs


-- a binary tree of Ints
data BinaryTreeInt
    = EmptyInt
    | NodeInt Int BinaryTreeInt BinaryTreeInt
    deriving (Show)

-- summing a binary tree of ints
sumTreeInt :: BinaryTreeInt -> Int
sumTreeInt EmptyInt               = 0
sumTreeInt (NodeInt x left right) = x + sumTreeInt left + sumTreeInt right

-- a binary tree of as
data BinaryTree a
    = Empty
    | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show)

-- a demonstration of how to place typeclass constraints
-- we require that a is an instance of Num, allowing us to use (+) on it
sumTree' :: (Num a) => BinaryTree a -> a
sumTree' Empty               = 0
sumTree' (Node x left right) = x + sumTree' left + sumTree' right

-- very simple datatype
data A = B | C
    deriving (Show)

-- to demonstrate the syntax of an instance declaration
instance Eq A where
    B == B = True
    C == C = True
    _ == _ = False

-- an example in which we need the Eq constraint
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []     = False
elem' x (y:ys) = x == y || elem' x ys

-- Eq for BinaryTreeInt
instance Eq BinaryTreeInt where
    EmptyInt == EmptyInt = True
    NodeInt x1 l1 r1 == NodeInt x2 l2 r2 = x1 == x2
                                        && l1 == l2
                                        && r1 == r2

-- Eq for (BinaryTree a)
-- an example in which we need to constrain as to as which are in Eq
-- this is because obviously we need to be able to compare values in nodes
instance (Eq a) => Eq (BinaryTree a) where
    Empty == Empty = True
    Node x1 l1 r1 == Node x2 l2 r2 = x1 == x2
                                  && l1 == l2
                                  && r1 == r2

-- again, we need to constrain as to only things that are in Eq
instance (Eq a) => Eq (Maybe a) where
    Nothing == Nothing = True
    Just x  == Just y  = x == y
    _       == _       = False

-- Ord instance for (Maybe a), in which Nothing is the least element
-- and everything else is compared in whatever way they originally are compared
-- be careful with the totality of the function below!
-- Nothing < Just _
instance (Ord a) => Ord (Maybe a) where
    Nothing <= _      = True
    Just x  <= Just y = x <= y
    _       <= _      = False

-- mapping but for trees
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Empty = Empty
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

-- convert a BinaryTree to a list
-- this is equivalent to creating an "iterator" out of the tree in other languages
-- we can iterate on the tree in different ways, as usual
treeToList :: BinaryTree a -> [a]
treeToList Empty = []
-- "root left right"
treeToList (Node x l r)  = [x] ++ treeToList l ++ treeToList r

-- "left root right"
--treeToList (Node x l r)  = treeToList l ++ [x] ++ treeToList r
--
-- "left right root"
--treeToList (Node x l r)  = treeToList l ++ treeToList r ++ [x]

-- demonstrating the similarities between sumTree prodTree and treeToList
sumTree :: (Num a) => BinaryTree a -> a
sumTree Empty        = 0
sumTree (Node x l r) = x + sumTree l + sumTree r

prodTree :: (Num a) => BinaryTree a -> a
prodTree Empty        = 1
prodTree (Node x l r) = x * sumTree l * sumTree r

-- a generally useful function, and also a useful idiom with pattern matching
--
-- catMaybes [Nothing, Just 5, Just 3, Nothing] = [5, 3]
-- catMaybes [Nothing, Nothing, Nothing] = []
catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | Just x <- xs]

-- a useful function, but also a hint at how similar this is to mapTree and map
doIfJust :: (a -> b) -> Maybe a -> Maybe b
doIfJust _ Nothing  = Nothing
doIfJust f (Just x) = Just $ f x
