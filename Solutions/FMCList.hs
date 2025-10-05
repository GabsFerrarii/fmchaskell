{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import System.Win32 (xBUTTON1)

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "head undefined for empty list"
head (x:_) = x

tail :: [a] -> [a]
tail [] = error "tail undefined for empty list"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (_:xs) = length xs + 1

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
(x:xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "minimum undefined for empty list"
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = error "maximum undefined for empty list"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

take :: Int -> [a] -> [a]
take n _      | n <= 0 = []
take _ []     = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n xs     | n <= 0 = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p (x:xs) | p x = x : takeWhile p xs
takeWhile _ _ = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs@(y:ys) | p y = dropWhile p ys
dropWhile _ xs = xs

-- tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'

-- init
init :: [a] -> [a]
init []  = error "init undefined for empty list"
init [_] = []
init (x:xs) = x : init xs

-- inits
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

-- subsequences

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
-- TODO

any :: (a -> Bool) -> [a] -> Bool
any _ []     = False
any p (x:xs) = p x || any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ []     = True
all p (x:xs) = p x && all p xs

-- and
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

-- or
or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

-- concat
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem y = any (== y)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) = (y == x) || elem' y xs

-- (!!)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- cycle
cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = xs ++ cycle xs

-- repeat
repeat :: a -> [a]
repeat x = x : repeat x

-- replicate
-- replicate :: Int -> a -> [a]
-- TODO 

-- isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = (x == y) && isPrefixOf xs ys

-- isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf xs (y : ys) = isPrefixOf xs (y : ys) || isInfixOf xs ys

-- isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome s =
  let sFormat = map C.toLower (filter C.isAlphaNum s)
  in sFormat == reverse sFormat

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

