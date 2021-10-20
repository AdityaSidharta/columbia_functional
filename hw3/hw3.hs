import Data.Word

-- You may import other libraries for this assignment; do so below this line

{-

 Name: Aditya Kelvianto Sidharta
 Uni: aks2266

 Collaborators:

 References:

 ------------------------------

 COMS 4995 002 Parallel Function Programming

 Homework 3

 Due at 11:59 PM Sunday, October 24, 2021

 Modify this file with your solutions and submit it on Courseworks

 Do not modify the type signatures for any of the provided functions
 
 Above, include your name, UNI, list of people with whom your spoke about the
 assignment, and online references your consulted.

 Write your code alone.  You many consult the instructor, TAs, and other
 students, but do not copy/modify other's code.

 Please do not delete or modify any of the block comments below (i.e.,
 {- -} comments) as we use them to identify where your solutions begin and end.

 Feel free to add and delete single-line comments (i.e., --)

 -----

 Grading: 70% correctness: first and foremost, it needs to be correct
          30% style: is it readable, functional, concise?

 Use lts-18.10 as the "resolver" for the Haskell Tool Stack.
 E.g., stack --resolver lts-18.10 ghci  

 Your code should load under GHCi 8.10.7 with no warnings under -Wall, e.g.
 :set -Wall
 :l hw3

 ==============================

 In the first nine problems of this assignment, you will implement an
 infinite precision integer arithemtic class called "Int100" that uses
 lists of Word8 (unsigned bytes) to represent integers in base 100 (to
 simplify decimal printing)

 Each list of Word8 digits consists of at least one digit and the last
 element -- the most significant digit -- must be non-zero.

 E.g.,

 IntP [0,2] represents 200
 IntN [5]   represents -5
 IntZ       represents 0
 IntP []    is illegal (must have at least one digit)
 IntN [0]   is illegal (last element most be non-zero)
 IntP [1,0] is illegal (last element must be non-zero)
 IntP [100] is illegal (digits must be in [0..99])

-}

data Int100 = IntP [Word8]
            | IntN [Word8]
            | IntZ
    deriving Eq

{- 1) Write the toInt100 function, which converts an
      Integer to an Int100

      E.g.,

      toInt100 0 = IntZ
      toInt100 (-1) = IntN [1]
      toInt100 99 = IntP [99]
      toInt100 100 = IntP [0,1]
      toInt100 (-199) = IntN [99,1]
--  -}
toInt100 :: Integer -> Int100
toInt100 x | x == 0 = IntZ
           | x > 0 = IntP (getVal x)
           | otherwise = IntN (getVal (-x))
       where getVal x | x <= 99 = [fromIntegral x]
                     | otherwise = let (q, r) = x `quotRem` 100 in fromIntegral r : getVal q


{- 2) Write an instance of Show (just the show function)
      for the Int100 type.  You may assume the Int100 object
      is not illegal, i.e., every digit in the lists is in the range [0..99]
      and every list ends in a non-zero digit.

      The implementation provided below is intended for you to use
      while you are debugging other functions

      E.g., 

      show IntZ         = "0"
      show (IntP [3,4]) = "403"
      show (IntN [42])  = "-42
-}

instance Show Int100 where
    show IntZ = "0"
    show (IntN l) = "-" ++ show (IntP l)
    show (IntP l) = showdigit l
        where showdigit [] = ""
              showdigit [x] = show x
              showdigit (x:xs) | x < 10 = showdigit xs ++ "0" ++ show x
                               | otherwise = showdigit xs ++ show x

{- 3) Write a function addDigits that adds two positive integers
      represented as lists in base-100 form, LSD first (e.g., as in an Int100).
      Assume every list element is in [0..99] and that the last element, if
      any, is non-zero.

      Your function should produce a list whose last element is non-zero
      or the empty list.

      E.g.,

      addDigits [] [] = []
      addDigits [] [1,2,3]  = [1,2,3]
      addDigits [99,1] [1]  = [0,2]
      addDigits [99,99] [1] = [0,0,1]
      addDigits [99,99,99] [99,99] = [98,99,0,1]
-}
addDigits :: [Word8] -> [Word8] -> [Word8]
addDigits x y = addWithCarry x y 0
      where addWithCarry [] [] 0 = []
            addWithCarry [] [] c = [c]
            addWithCarry (x:xs) [] c = ((x + c) `rem` 100) : addWithCarry xs [] ((x + c) `quot` 100)
            addWithCarry [] (y:ys) c = ((y + c) `rem` 100) : addWithCarry [] ys ((y + c) `quot` 100)
            addWithCarry (x:xs) (y:ys) c = ((x + y + c) `rem` 100) : addWithCarry xs ys ((x + y + c) `quot` 100)

{- 4) Write a function subDigits that subtracts the second positive
      integer from the first.  Assume both numbers are in base-100 form,
      the last element, if any, is non-zero, and that the
      first number is greater or equal to the second (i.e., that the result
      is non-negative)

      Your function should produce a list in base-100 form, i.e.,
      each list element is in [0..99] and the last list element, if any,
      is non-zero.

      E.g.,

      subDigits [] [] = []
      subDigits [1,2,3,4] [1,2,3,4] = []
      subDigits [1,2,3,4] [0,2,3,4] = [1]
      subDigits [0,0,0,0,1] [1] = [99,99,99,99]
      subDigits [0,0,0,1] [0,1] = [0,99,99]
      subDigits [99,99,0,99] [1,10,1,1] = [98,89,99,97]
-}
subDigits :: [Word8] -> [Word8] -> [Word8]
subDigits x y = let result = subWithBorrow x y 0 in reverse(dropWhile (==0) (reverse result))
      where subWithBorrow [] [] 0 = []
            subWithBorrow (x:xs) [] c | x < c = (100 + x - c) : subWithBorrow xs [] 1
                                      | otherwise = (x - c) : subWithBorrow xs [] 0
            subWithBorrow (x:xs) (y:ys) c | x < y + c = (100 + x - y - c) : subWithBorrow xs ys 1
                                          | otherwise = (x - y - c) : subWithBorrow xs ys 0
            subWithBorrow _ _ _ = error "y should not be bigger than x"


{- 5) Write a function multDigit that multiplies a positive base-100 integer
      by an integer in the range [0..99].  Assume the last element of the
      base-100 list is non-zero and that the input integer is in the range
      [0..99]    

      E.g.,

      multDigit 0 [42] = []
      multDigit 1 [1,2,3] = [1,2,3]
      multDigit 2 [1,2,3] = [2,4,6]
      multDigit 99 [99,99,99] = [1,99,99,98]
-}
multDigit :: Word8 -> [Word8] -> [Word8]
multDigit x y = multWithCarry x y 0
      where multWithCarry 0 _ _ = []
            multWithCarry x [] 0 = []
            multWithCarry x [] c = [c]
            multWithCarry x (y:ys) c = let result = toInteger x * toInteger y + toInteger c
                                       in fromIntegral(result `rem` 100) : multWithCarry x ys (fromIntegral(result `quot` 100))


{- 6) Write a function multDigits that multiplies two positive base-100
      integers.  You may use multDigit and addDigits.

      E.g.,

      multDigits [] [1,2,3] = []
      multDigits [1,2,3] [] = []
      multDigits [1] [99,98,97] = [99,98,97]
      multDigits [0,1] [99,98,97] = [0,99,98,97]
      multDigits [0,2] [99,98,97] = [0,98,97,95,1]
      multDigits [99,99,99] [99,99,99] = [1,0,0,98,99,99]

-}
multDigits :: [Word8] -> [Word8] -> [Word8]
multDigits x y = foldl1 addDigits (zipWith (++) ([replicate x 0 | x <- [0..]]) (listDigits x y))
      where listDigits [] _ = []
            listDigits _ [] = []
            listDigits (x:xs) y = multDigit x y : listDigits xs y



{- 7) Write a function compare100 that compares the magnitude of the
      two base-100 numbers given to it.
      Assume both input lists have non-zero last elements, but
      that they may be of different lengths.

      E.g.,

      compare100 [] [] = EQ
      compare100 [1] [] = GT
      compare100 [] [1] = LT
      compare100 [55,1] [55,1] = EQ
      compare100 [0,56] [1,55] = GT
      compare100 [0,0,0,2] [99,99,99,1] = GT
      compare100 [98,99,5] [99,98,5] = GT
      compare100 [99,98,5] [98,99,5] = LT
-}
compare100 :: [Word8] -> [Word8] -> Ordering
compare100 x y = compareReverse (reverse x) (reverse y)
      where compareReverse [] [] = EQ
            compareReverse _ [] = GT
            compareReverse [] _ = LT
            compareReverse (x:xs) (y:ys) | x > y = GT
                                         | x < y = LT
                                         | otherwise = compareReverse xs ys

{- 8) Write an instance of the Num type class for Int100.
      Include implementations of fromInteger, +, *, signum, abs,
      and negate. Use your toInt100, addDigits, subDigits, multDigits,
      and compare100 functions.

      E.g.,

     (fromInteger (-1234567)) :: Int100 = -1234567
     fromInteger 0 :: Int100 = 0
     fromInteger 42 :: Int100 = 42

     signum ((-5) :: Int100) = -1
     signum (0 :: Int100) = 0
     signum (421 :: Int100) = 1

     negate ((-123) :: Int100) = 123
     negate (0 :: Int100) = 0
     negate (453 :: Int100) = -453

     abs ((-123) :: Int100) = 123
     abs (123 :: Int100) = 123
     abs (0 :: Int100) = 0
 
     1234 + (4567 :: Int100) = 5801
     1234 + ((-1233) :: Int100) = 1
     1232 + ((-1233) :: Int100) = -1
     1234 - (1234 :: Int100) = 0
         
     99 * ((-99) :: Int100) = -9801
     (-998801) * (200 :: Int100) = -199760200
     (-16) * ((-32) :: Int100) = 512
-}
instance Num Int100 where
   fromInteger = toInt100

   IntP x + IntP y = IntP (addDigits x y)
   IntP x + IntN y | compare100 x y == LT = IntN (subDigits y x)
                   | compare100 x y == EQ = IntZ
                   | otherwise = IntP (subDigits x y)
   IntP x + IntZ = IntP x
   IntN x + IntP y | compare100 x y == GT = IntN (subDigits x y)
                   | compare100 x y == EQ = IntZ
                   | otherwise = IntP (subDigits y x)
   IntN x + IntN y = IntN (addDigits x y)
   IntN x + IntZ = IntN x
   IntZ + IntP y = IntP y
   IntZ + IntN y = IntN y
   IntZ + IntZ = IntZ
   
   IntP x * IntP y = IntP (multDigits x y)
   IntP x * IntN y = IntN (multDigits x y)
   IntP _ * IntZ = IntZ
   IntN x * IntP y = IntN (multDigits x y)
   IntN x * IntN y = IntP (multDigits x y)
   IntN _ * IntZ = IntZ
   IntZ * IntP _ = IntZ
   IntZ * IntN y_ = IntZ
   IntZ * IntZ = IntZ

   signum IntZ = 0
   signum (IntP _) = 1
   signum (IntN _) = -1

   negate IntZ = IntZ
   negate (IntP x) = IntN x
   negate (IntN x) = IntP x
   
   abs IntZ = IntZ 
   abs (IntP x) = IntP x
   abs (IntN x) = IntP x

{- 9) Write an instance of the Ord type class for Int100.
      in particular, write an implementation of the compare
      function; Haskell will infer the rest.  Use your
      compare100 function.

      E.g.,

     (0 :: Int100) `compare` 0 = EQ
     (0 :: Int100) `compare` 1 = LT
     (1 :: Int100) `compare` 0 = GT
     (1 :: Int100) `compare` 1 = EQ
     ((-1) :: Int100) `compare` 1 = LT
     (1 :: Int100) `compare` (-1) = GT

-}
instance Ord Int100 where
  IntP x `compare` IntP y = compare100 x y
  IntP _ `compare` IntZ = GT
  IntP _ `compare` IntN _ = GT
  IntZ `compare` IntP _ = LT
  IntZ `compare` IntZ = EQ
  IntZ `compare` IntN _ = GT
  IntN _ `compare` IntP _ = LT
  IntN _ `compare` IntZ = LT
  IntN x `compare` IntN y = compare100 x y

{- 10) Implement the Functor type classes for the BHeap and BTree types.

     Your solution should satisfy the functor properties:
     fmap id = id
     fmap ( f . g ) = fmap f . fmap g
-}
data BHeap a = BHeap [BTree a] deriving (Eq, Show, Read)
data BTree a = BNode Int a (BHeap a) deriving (Eq, Show, Read)

instance Functor BHeap where
  fmap f (BHeap []) = BHeap []
  fmap f (BHeap x) = BHeap (fmap f x)-- Change this

instance Functor BTree where
  fmap f (BNode) = B -- Change this
