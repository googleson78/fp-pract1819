-- list comprehension example
--
-- we enumerate where our elements come from using "<-"
-- on the left of '|' we write what to do with our elements
-- in this case we create 2-tuples
-- with multiple sources for elements we take them in order from left to right
-- so first we take one from xs, then all of the ones from ys
-- then the next one from xs, and then all of the ones from ys again
-- an example makes this clearer
-- cartesian [1,2] [4,5] = [(1,4), (1, 5), (2, 4), (2, 5)]
--
-- we can also place addiitonal restrictions (filtering), with more commas
cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = [(x, y) | x <- xs, y <- ys]


-- repeatedly apply a function, creating a list of the results
-- iterate' f x == [x, f x, f (f x), f (f (f x))...]
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- nats without using the built-in [0..] syntax
--
-- nats == [0, 1, 2, 3, 4, 5...]
nats :: [Integer]
nats = iterate' succ 0

-- x `divides` y iff x divides y
--
-- divides 5 10   == True
-- 5 `divides` 10 == True
-- divides 5 9    == False
divides :: Integer -> Integer -> Bool
divides x y = y `rem` x == 0

-- all the divisors of a number
-- use list comprehensions!
-- divisors 20 == [1, 2, 4, 5, 10, 20]
-- divisors 7 == [1, 7]
divisors :: Integer -> [Integer]
divisors x = [y | y <- [1..x], y `divides` x]

-- use list comprehensions!
-- a number is prime iff it's only divisors are 1 and the number itself
--
-- isPrime 7   == True
-- isPrime 144 == False
isPrime :: Integer -> Bool
isPrime x = [1, x] == divisors x

-- all the primes, yay
--
-- primes == [2, 3, 5, 7, 11...]
primes :: [Integer]
primes = [p | p <- tail nats, isPrime p]

-- applies the sieve of eratosthenes to a list
eratosthenes :: [Integer] -> [Integer]
eratosthenes (x:xs) = x : eratosthenes (filter (not . divides x) xs)

-- faster primes using the sieve of eratosthenes
primes' :: [Integer]
primes' = eratosthenes [2..]

-- positive2Tuples must contain all 2-tuples of positive naturals
-- the naive solution - [(x, y) | x <- tail nats, y <- tail nats]
-- would get us stuck infinitely getting elements from the second list
--
-- instead we need to think of a way to map all the natural numbers
-- to all the tuples of natural numbers
positive2Tuples :: [(Integer, Integer)]
positive2Tuples = concat $ map partitionPos $ tail nats

-- we will use this function - for each numbe n it will return
-- a list of all the 2-tuples that sum to n
--
-- partitionPos 1 = []
-- partitionPos 2 = [(1, 1)]
-- partitionPos 3 = [(1, 2), (2, 1)]
partitionPos :: Integer -> [(Integer, Integer)]
partitionPos n = [(i, n - i) | i <- [1..(n - 1)]]

-- naive way - just map over nats

facts :: [Integer]
facts = map fact nats

fact :: Integer -> Integer
fact n = product [1..n]

fibs :: [Integer]
fibs = map fib nats

fib :: Integer -> Integer
fib n = fib (n - 1) + fib (n - 2)

-- but this is sloooooooooow
-- we can memoize! for facts we only need the previous result

-- with foldl we have access to the preivous value, but we can't
-- get all the values, so we can have facts'
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v []     = v
foldl' f v (x:xs) = foldl' f (v `f` x) xs

-- so let's write a function that stores all intermediate values
-- this is called a scan
--
-- scanl' (+) 0 [1,2,3,4,5] == [0,1,3,6,10,15]
-- scanl' (*) 1 [1,2,3,4,5] == [1,1,2,6,24,120]
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ v []     = [v]
scanl' f v (x:xs) = v : scanl' f (v `f` x) xs

-- now we can simply use our foldr implementation for fact
-- to get free memoization on all the facts
facts' :: [Integer]
facts' = scanl (*) 1 [1..]

-- we can do something similar with fibs
-- but we need to keep track of the last two numbers

-- generalized zip
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipwith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- we start with two 1's as the base of our recursion
-- and then we recursively refer to the already generated list
-- to generate the next two
--
-- the "tail fibs'" is a way to "shift" fibs by one, and them sum
-- the corresponding terms of fibs' and tail fibs'
fibs' :: [Integer]
fibs' = 1 : 1 : zipWith (+) fibs' (tail fibs')
