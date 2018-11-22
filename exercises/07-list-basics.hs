-- We first talked about some philosophical Haskell stuff
-- such as laziness and types. You can read about it from
-- http://learnyouahaskell.com/introduction

-- in haskell we have tuples as values and tuples as types. for example:
x :: (Int, Int)
x = (1, 1)

-- we can take the first element of a tuple with fst and the second with snd
-- more often though we will just pattern match on tuples instead
sumTuple :: (Int, Int) -> Int
sumTuple (x, y) = x + y

-- there are also types and values for larger tuples, but they are rarer
y :: (Int, Int, Int)
y = (1, 2, 3)

-- we won't be doing an explicit exercise on these, because they are not very complicated
-- but we will mention them with necessary

-- in haskell lists are a seperate type, they are not just a special case of a tuple

-- They are still defined in the same way, but thanks to the type system we can
-- differentiate at compile time between tuples and lists.

-- A list is:
-- (1) [] is a list. This is called the empty list. It's type is [] :: [a]
-- The 'a' means that this is polymorphic - for all the types in haskell,
-- [] is an empty list of them.
-- For example [] is an empty list of Ints, and an empty list of Strings, at the same time.
--
-- (2) If x :: a and xs :: [a] then x:xs is also a list.
-- This is the cons operation from scheme and it's still read that way.

-- We can work with lists using equivalents to car and cdr, which are head and tail-
-- head :: [a] -> a
-- tail :: [a] -> [a]

-- But we will instead always prefer to "pattern-match".

-- This is case analysis on the structure of our elements.
-- More concretely for lists this means that we can define functions on lists by
-- defining their values for our base case (1) and our recursive case.
--
-- For example, in order to sum a list:
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- By "pattern matching" on (x:xs) we automatically bind the head of our list to x
-- and the tail of our list to xs. This is really convenient.

-- In pattern matches if we don't care about our argument we can write _ instead.
-- (and this is good style)
null' :: [a] -> Bool
null' [] = True
null' _  = False

-- null' [] == True
-- null' [1,2,3] == False

-- Let's implement standard list functions.

-- For head we don't have anything to return from an empty list.
-- For this reason we need some way to make the types fit
-- and
-- error :: String -> a
-- is just such one.
-- However, avoid this whenever possible. We will talk about this later.
head' :: [a] -> a
head' []    = error "The empty list has no head"
head' (x:_) = x

-- head' []      == uh-oh
-- head' [1,2,3] == 1

tail' :: [a] -> [a]
tail' [] = error "The empty list has no tail"
tail' (_:xs) = xs

-- tail' []      == uh-oh
-- tail' [1,2,3] == [2,3]

take' :: Int -> [a] -> [a]
take' 0 _      = []  -- recursion bottom
take' n []     = []  -- special case
take' n (x:xs) = x : take (n - 1) xs

-- take' 10 []      == []
-- take' 2 [1,2,3]  == [1,2]
-- take' 10 [1,2,3] == [1,2,3]

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs  -- recursion bottom
drop' _ []     = []  -- special case
drop' n (_:xs) = drop (n - 1) xs

-- drop' 2 [1,2,3]  == [3]
-- drop' 10 [1,2,3] == []

-- index
(!) :: [a] -> Int -> a
(x:_)  ! 0 = x
[]     ! _ = error "Can't index the empty list"
(_:xs) ! n = xs ! (n - 1)

-- [1,2,3] ! 2 == 3
-- [1,2,3] ! 100 == uh-oh
--
length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

-- length' []      = 0
-- length' [1,2,3] = 3

-- We can eta-reduce this, by removing the extra xs.
reverse' :: [a] -> [a]
reverse' xs = helper [] xs
    where helper :: [a] -> [a] -> [a]
          helper acc [] = acc
          helper acc (y:ys) = helper (y : acc) ys

reverse'' :: [a] -> [a]
reverse'' = helper []
    where helper :: [a] -> [a] -> [a]
          helper acc [] = acc
          helper acc (y:ys) = helper (y : acc) ys
