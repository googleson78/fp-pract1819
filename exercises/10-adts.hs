-- calculate the next approximation
--
-- curr is our current approximation, and n is the number
-- for which we are approximating
improve :: Double -> Double -> Double
improve n curr = (curr + n / curr) / 2

-- calculate whether a and b are within eps distance of each other
--
-- we use this to know when we should stop searching taking better approximations
within :: Double -> Double -> Double -> Bool
within eps a b = abs (a - b) < eps

-- get the first x from a list, such that the previous head - y, of the list is within eps y x
--
-- we use within here
-- we pattern match on the first two, because we need to have info on both of them
firstWithin :: Double -> [Double] -> Double
firstWithin eps (x:y:ys) = if within eps x y then y else firstWithin eps (y:ys)

-- we generate an infinite amount of approximations by using iterate
--
-- pretty cool if I may say so myself
sqrt' :: Double -> Double
sqrt' n = firstWithin 0.001 $ iterate (improve n) 1

-- let's look at ways we can define our own types
--
-- first and most basic - we can define a type synonym
--
-- this has NO effect on the compiler (type checker)
-- it effective treats Point and (Double, Double) as one and the same -
-- think of a #define Point (Double, Double)
--
-- it's only for our convenience
type Point = (Double, Double)

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- something further - we can use newtype to again define "synonyms"
-- but this time we need to create a "constructor" for our newtype
-- in this case our type is called Natural and it's constructor is Nat
--
-- Nat is simply a function of type (Integer -> Natural), it's not like
-- "conventional" constructors you're used to in Java/C++, it doesn't do
-- anything magical, it simply creates a "box" which surrounds it's contents -
-- in this case a single Integer
--
-- newtypes are different from type in that the compiler (type checker) enforces them
-- we cannot interchangably use Integer and Natural, and we can't use functions
-- that work on Integers on Naturals (or vice versa)
--
-- they still have no runtime representation however - during the compilation process
-- they are removed and have no overhead. In this way they are very useful to enforce
-- some "smart construction" mechanism
--
-- in order to work on things with constructors we need to pattern match on constructors
newtype Natural = Nat Integer
    deriving (Show)

-- "smart constructor"
natural :: Integer -> Natural
natural x = if x < 0 then error "Nats can't be less than 0"
                     else Nat x -- we construct the Nat here

-- pattern matching
evenNat :: Natural -> Bool
evenNat (Nat x) = even x

-- now the "most" powerful way to create our own types
--
-- data declarations
-- they allow us to declare so-called "algebraic data types" - ADTs
-- they are called like that because they stem from product and sum types

-- the most basic type we can create is the empty type - it has no constructors
data Void

-- we also have the unit type - it already exists in Haskell and it's named ()
-- some people call it "the empty tuple"
-- it has only one constructor
-- it might seem useless, but we will (hopefully) see later that it's not -
-- it's useful for expressing "emptiness"
data Unit = Unit
    deriving (Show)

-- we can declare "enums" using our sum type
-- the syntax is constructors seperated by '|'s
--
-- in order to work with these we need to pattern match (use case statements in other words)
data Language = Haskell
              | Ruby
              | Python
              | Agda
              | Scheme
              | Java
              | Idris
              | C
    deriving (Show)

-- pattern matching example
functional :: Language -> Bool
functional Haskell = True
functional Scheme  = True
functional Agda    = True
functional Idris   = True
functional _       = False

-- constructing a list of these values
langs :: [Language]
langs = [Haskell, Idris, Agda]

type FN = Int
type StudentName = String

-- we can declare "product" types the same way we construct a newtype
-- but we instead list out more "fields"
-- you can think of these as fields in a C struct
-- in this case Stud is simply a function of type FN -> StudentName -> Language -> Student
--
-- again we can work with these using pattern matching
data Student = Stud FN StudentName Language
    deriving (Show)

doingWellInLife :: Student -> Bool
doingWellInLife (Stud 81248 _ _) = False
doingWellInLife (Stud fn name lang) = fn < 100000 && name == "Georgi" && functional lang

-- lists are simply ADTs
-- their the empty list [] :: [a]
-- and (:) :: a -> [a] -> [a]
-- an example of an operator as a constructor
--
-- for now we can define a more restricted version containing only lists
data IntList = Nil
             | Cons Int IntList
    deriving (Show)

-- working on them with pattern matching, again
mapInt :: (Int -> Int) -> IntList -> IntList
mapInt _ Nil         = Nil
mapInt f (Cons x xs) = f x `Cons` mapInt f xs


-- as an exercise, let's define a data type that expresses a "calculator"
--
-- we will have constructors for defining the different expressions we can
-- create, corresponding to simply numbers, +1, -1, (+), (-), (^), conditional if's
-- calculator
--
-- regarding the If, we will consider everything that is not 0 to be "False"
data CalcExpr = Number Integer
              | Succ CalcExpr -- +1
              | Pred CalcExpr -- -1
              | Add CalcExpr CalcExpr -- left plus right
              | Sub CalcExpr CalcExpr -- left minus right
              | Pow CalcExpr CalcExpr -- left to the power of right
              | If CalcExpr CalcExpr CalcExpr
    deriving (Show)

-- our CalcExpr datatype expresses the syntactic structure of our expressions
-- the abstract syntax tree
--
-- we now need something to give it meaning (semantics)
-- this is usually called "evaluation" - so eval
--
-- eval (Pow (Number 2) (Succ (Number 9))) == 1024
eval :: CalcExpr -> Integer
eval (Number n) = n -- an integer will simply evaluate to itself
eval (Succ e) = succ $ eval e -- and for the rest will simply recurse on the CalcExpr type
eval (Pred e) = pred $ eval e
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Pow e1 e2) = eval e1 ^ eval e2
eval (If cond thenExpr elseExpr)
    = let b = eval cond
      in if b == 0
         then eval thenExpr
         else eval elseExpr
