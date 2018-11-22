-- In Haskell all our functions are actually single argument.

plusOne :: Int -> Int
plusOne n = n + 1
-- We can simulate multiple argument functions by just returning
-- functions every time.
f :: Int -> Int -> Int
f x y = x + y

-- in reality, f is *NOT* a function which takes two arguments,
-- but instead
-- a function that takes one argument (x), and returns a function of type (Int -> Int)
-- which when given an argument (y), will return their sum (x + y), of type Int.
--
-- This is thanks to the fact that the (->) operator is right-associative
--
-- f's type is actually
-- f :: Int -> (Int -> Int)
--
-- if we write out the brackets. This makes it clearer that by providing it with one argument we get a function.
--
-- This is very convenient a lot of the time, because it allows us to write functions very concisely.
--
-- For example our earlier plusOne could just instead be our plus function (+), applied to only one argument.
plusOne' :: Int -> Int
plusOne' = (+) 1

-- This works, because (+) is of type Int -> Int -> Int,
-- meaning when supplied with 1, we will get a function of one Int, which adds 1 to it's argument.

-- Note that you don't actually need to think about this all the time,
-- and instead it's fine to think of (+) as a function on two arguments.
-- It will just be very convenient very often, to partially apply functions.
-- For example if we want to increase all elements of a list by 10 it's way cleaner and easier to write
-- map (+10) xs
-- than
-- map (\x -> x + 10) xs
--
-- (Which is the syntax for lambdas, by the way
-- \arg1 arg2 .. argn -> result
--
-- it's a '\', becuase if you squint hard enough it looks similar to the OG letter lambda)
--
-- Another note:
-- (+10) :: Int -> Int
-- is legal syntax.
-- If we have an operator we can enclose it in brackets and provide it with one of it's arguments
-- depending on which side we place the argument. This is often called a section.
-- Do note that the side matters
-- (/2) is a function which divides something by two
-- (2/) is a function which divides two by something

-- Let's write the most basic function I can think of.
-- Application
-- In Haskell this has an operator - ($).
-- Why is it useful?
-- Because functions have the highest priority, we often need to write brackets in order to enforce
-- order of operations. However we don't like brackets, because they are annoying to write and clutter
-- our code. So instead we have another form of function application - ($), which has the _lowest_ priority
-- between operators.
-- so instead of
-- map plusOne (map plusOne [1,2,3])
-- we could write
-- map plusOne $ map plusOne [1,2,3]
--
-- we can even chain them, because ($) is right-associative
-- map plusOne $ map plusOne $ map plusOne $ map plusOne $ map plusOne $ map plusOne $ map plusOne $ map plusOne [1,2,3]
apply :: (a -> b) -> a -> b
apply f x = f x

-- But wait! The x's are at the end, we can eta-reduce this.
apply' :: (a -> b) -> a -> b
apply' f = f

-- But wait! The f's are at the end, we can eta-reduce this.
--apply'' :: (a -> b) -> a -> b
--apply'' = uh.....???
-- If you think about it enough, apply is actually the id function
-- id :: a -> a
-- specialised to functions.
apply'' :: (a -> b) -> a -> b
apply'' = id

-- apply succ 1   == 2
-- succ `apply` 2 == 3

-- applyTwice needs to take a function with the same result type,
-- as it's argument.
-- Because if you think about it, after applying f to x once, we need to feed the
-- result of (f x) back to f, meaning that (f x), must have the same type
-- as the argument of f
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- applyTwice succ 1 == 3

-- or we can do
-- applyTwice f x = f $ f x
-- or even better, by using (.), which is the composition operator in haskell
-- applyTwice f = f . f

-- Pay attention to the types. The first two arguments can be swapped, but this is
-- the standard way and how (.) is.
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- (compose succ succ) 2 = 4
-- compose (*2) (*4)   2 = 16

-- Lists!
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

-- Lists!
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs) = if p x then x : filter' p xs
                          else filter' p xs

-- Useful for the next one.
-- Partitions the list into two parts - one which matches
-- the predicate, and one which doesn't.
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = (filter p xs, filter (not . p) xs)

-- partition' (<5) [0..10] == ([0,1,2,3,4], [5,6,7,8,9,10])

-- with ints!
quickSort :: [Integer] -> [Integer]
quickSort []     = []
quickSort (p:xs) = quickSort smaller ++ [p] ++ quickSort larger
    where
        (smaller, larger) = partition' (<p) xs

-- the accumulator is on the right
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- insert an element into a sorted list, in such a way as to keep the list sorted
insert :: Integer -> [Integer] -> [Integer]
insert x [] = [x]
insert x l@(y:ys) = if x < y then x : l
                             else y : (insert x ys)

-- we just insert all the elements into an empty list, by keeping it sorted every time
-- because our base - [] is already sorted, this means our final list will also be sorted
insertionSort :: [Integer] -> [Integer]
insertionSort xs = foldr' insert [] xs

-- eta reduced version
insertionSort' :: [Integer] -> [Integer]
insertionSort' = foldr' insert []
