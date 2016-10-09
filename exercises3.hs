-- Exercises 3

import Test.QuickCheck
import Data.List

-- 1 permutations

-- isPermutation xs ys checks whether xs is a permutation of ys
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation []     []     = True
isPermutation []     (y:ys) = False
isPermutation (x:xs) ys     = exists x ys && isPermutation xs (removeOnce x ys)

-- removeOnce x xs removes x from the list xs, but only once
removeOnce :: Eq a => a -> [a] -> [a]
removeOnce x []                 = []
removeOnce x (y:ys) | x == y    = ys
                    | otherwise = y : removeOnce x ys

-- backwards xs reverses the elements in the list xs
backwards :: [a] -> [a]
backwards []     = []
backwards (x:xs) = atBack x (backwards xs)
 where
  atBack x []     = [x]
  atBack x (y:ys) = y : atBack x ys

-- 2. Sorting

-- sorted xs checks whether the list xs is sorted
sorted :: Ord a => [a] -> Bool
sorted []       = True
sorted [x]      = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

-- Note: Why don't we have to check that x <= all elements in (y:ys)?

-- insert' x xs inserts x in the sorted list xs, at the right place
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert' x ys

-- isort xs produces the sorted version of the list xs
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert' x (isort xs)

prop_Sorted :: [Integer] -> Bool
prop_Sorted xs =
  sorted (isort xs)

prop_Permutation :: [Integer] -> Bool
prop_Permutation xs =
  isPermutation xs (isort xs)

-- 3. Avoiding Duplicates
-- duplicates xs checks if there are duplicates in the list xs
duplicates :: Eq a => [a] -> Bool
duplicates []     = False
duplicates (x:xs) = exists x xs || duplicates xs

-- exists x xs checks whether x exists as an element in the list xs
exists :: Eq a => a -> [a] -> Bool
exists x []     = False
exists x (y:ys) = x == y || exists x ys

-- Note: "exists" is the standard Haskell function "elem"

-- removeDuplicates xs returns the list xs with all duplicate elements removed
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : removeDuplicates (remove x xs)

-- remove x xs removes x from the list xs
remove :: Eq a => a -> [a] -> [a]
remove x []                 = []
remove x (y:ys) | x == y    = remove x ys
                | otherwise = y : remove x ys

-- No, the property does not guarantee this. A function always returning the
-- empty list would also satisfy the property!

-- Missing is a property that checks that no elements disappear:

prop_RemoveDuplicatesKeepsElements xs =
  allExist xs (removeDuplicates xs)

-- allExist xs ys checks whether all x in xs exist as an element in ys
allExist :: Eq a => [a] -> [a] -> Bool
allExist []     ys = True
allExist (x:xs) ys = exists x ys && allExist xs ys

-- 4. pascal's Triangle

pascal :: Int -> [Int]
pascal 1 = [1]
pascal n = [1] ++ [ x+y | (x,y) <- pairs (pascal (n-1)) ] ++ [1]
 where
   pairs (x:y:xs) = (x,y) : pairs (y:xs)
   pairs _ = []

-- the property is always true if function is correct
prop_Pascal n =
  n >= 1 ==> length (pascal n) == n

pascal2 :: Int -> [Int]
pascal2 n = case pascal2 n of
                1 -> [1]
                n -> [1] ++ [ x+y | (x,y) <- pairs (pascal2 (n-1)) ] ++ [1]
   where
    pairs (x:y:xs) = (x,y) : pairs (y:xs)
    pairs _ = []

  -- 5. Eratosthenes' sieve

  crossOut :: Int -> [Int] -> [Int]
  crossOut n xs = [ x | x <- xs, x `mod` n /= 0 ]

  sieve :: [Int] -> [Int]
  sieve []     = []
  sieve (n:ns) = n : sieve (crossOut n ns)

  primes1to100 = sieve [2..100]

  prop_Sieve n =
    and [ isPrime n | n <- sieve [2..n] ]
   where
   isPrime n = factors n == [1,n]
   factors n = [ k | k <- [1..n], n `mod` k == 0 ]

   -- 6. Number Games

   isPrime100 :: Int -> Bool
   isPrime100 n = n `elem` primes1to100

   isSumOf2Primes100 :: Int -> Bool
   isSumOf2Primes100 n =
     not (null [ (a,b)
               | a <- primes1to100
               , b <- primes1to100
               , n == a+b
               ])

   counterExamples :: [Int]
   counterExamples = [ n | n <- [4..100], even n, not (isSumOf2Primes100 n) ]

   prop_Goldbach =
     null counterExamples
