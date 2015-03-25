module A_Soln where
import Data.Monoid
{-
Problem

You receive a credit C at a local store and would like to buy two items. You first walk through the store and create a list L of all available items. From this list you would like to buy two items that add up to the entire value of the credit. The solution you provide will consist of the two integers indicating the positions of the items in your list (smaller number first).

Input

The first line of input gives the number of cases, N. N test cases follow. For each test case there will be:

One line containing the value C, the amount of credit you have at the store.
One line containing the value I, the number of items in the store.
One line containing a space separated list of I integers. Each integer P indicates the price of an item in the store.
Each test case will have exactly one solution.
Output

For each test case, output one line containing "Case #x: " followed by the indices of the two items whose price adds up to the store credit. The lower index should be output first.

Limits

5 ≤ C ≤ 1000
1 ≤ P ≤ 1000

Small dataset

N = 10
3 ≤ I ≤ 100

Large dataset

N = 50
3 ≤ I ≤ 2000

Sample


Input 

3
100
3
5 75 25
200
7
150 24 79 50 88 345 3
8
8
2 1 9 4 4 56 90 3

Output

Case #1: 2 3
Case #2: 1 4
Case #3: 4 5
-}

import System.IO
import Control.Monad
import Data.Maybe
import Text.Read
import Data.List

fnameIn = "A-small-practice.in"
fnameOut = "A-small-practice.out"

data Case = Case { cTot :: Int,
                   nItems :: Int,
                   cItems :: [Int]} deriving (Show, Eq)

main = do
  inh <- openFile fnameIn ReadMode
  --outh <- openFile fnameOut WriteMode
  inStr <- hGetContents inh
  let cases = processData inStr
  --hPutStr outh result
  print cases
  hClose inh
  --hClose outh

processData :: String -> [Case]
processData s = map caseC (triples d)
  where
    -- nCases = head (head c) 
    d = tail c
    c = readIntLists s
  
caseC :: [[Int]] -> Case
caseC s = Case {cTot = c, nItems = n, cItems = ci} where
  c = head (head s)
  n = head (s!!1)
  ci = s!!2

readIntLists :: String -> [[Int]]
readIntLists = map readInts . lines where
  readInts = map readInt . words
readInt x = read x :: Int

triples :: [a] -> [[a]]
triples = chunk 3
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y : chunk n y2 where
  (y, y2) = splitAt n xs

dropNth n v = (take n v) ++ (drop (n+1) v)
nth = flip (!!)
circShiftL v = (drop 1 v) ++ [(head v)]
-- -- adapted from Closest Pair
f c v = find (\x -> sum x == c) (pairs $ sort v) where
  pairs = chunk 2
-- -- FIXME : f 4 [-1, 4, 5, 10, 3] does not return exact sum pair = [-1, 5]
f' c v = p where
  pairss = chunk 2 $ sort $ filter (< c) v
  sumsTo x = c == sum x
  p = find sumsTo pairss

  

-- mkETr :: a -> Tr a
-- mkETr x = T E x E
-- data Tr a = E | T (Tr a) a (Tr a) deriving (Eq, Show)
-- member :: Ord a => a -> Tr a -> Bool
-- member _ E = False
-- member x (T a y b)
--   | x<y = member x a
--   | x>y = member x b
--   | otherwise = True
-- insTr x E = mkETr x
-- insTr x v@(T a y b) | x<y = T (insTr x a) y b
--                     | x>y = T a y (insTr x b)
--                     | otherwise = v
-- listToTr :: Ord a => [a] -> Tr a
-- listToTr v = foldr insTr E v

l0 = [-3, 2, 1,4,-5, 3]
-- t1, t1' :: Tr Int
-- t1 = listToTr l0
-- t1' = listToTr $ sort l0

-- f c v = t where
--   sv = sort $ filter (< c) v
--   t = listToTr sv

-- lTr, rTr :: Tr a -> Tr a
-- lTr (T a _ _) = a
-- rTr (T _ _ b) = b
-- vTr :: Tr a -> a
-- vTr (T _ x _) = x

                
-- lCh (T (T a x b) y c) = x
-- lCh _ = 0
-- findSumTr :: (Ord a, Num a) => a -> Tr a -> a
-- findSumTr c v@(T a x b) | x + l> c = findSumTr c a
--                         | x + l< c = findSumTr c b
--                         | otherwise = x where
--                            l = lCh v
-- findSumTr _ E = error "unreachable"

--findTr :: (a -> Bool) -> Tr a -> a
-- findTr f (T a x b) | f x = x
-- findSumTr s c = findTr (\x -> x + s == c) 

-- instance Monoid (Tr a) where
--   mempty = E
--   --mappend = 

-- a :: [[Int]]
-- a = [[1,2],[3,4],[5,6],[7,8],[9,10],[11,12]]
-- s :: String
-- s = "12\n10\n20 30 40\n"
-- dat = readIntLists s

-- c0 = Case {cTot = 295, nItems = 17, cItems = [678,227,764,37,956,982,118,212,177,597,519,968,866,121,771,343,561]}

-- main' f = do
--   cases <- withReadFile fnameIn f
--   print cases
--   return cases

-- withReadFile fname f = do
--   h <- openFile fname ReadMode
--   instr <- hGetContents h
--   let resu = f instr
--   return resu
--   hClose h

-- withHGetContents :: Handle -> (String -> a) -> IO a
-- withHGetContents h f = do
--   ins <- hGetContents h
--   return $ f ins

-- findIndices' :: (Num a, Enum a) => (a->Bool) -> [a] -> Maybe [a]
-- findIndices' _ [] = Nothing
-- findIndices' p xs = Just [i | (x,i) <- zip xs [0..], p x]


-- g c = ct `elem` pv where
--   pv = map (+ (head v)) (tail v)
--   ct = cTot c
--   v = sort $ cItems c

-- h c = find (== ct) v where
--   v = filter (< ct) (cItems c)
--   ct = cTot c

-- findSum :: Eq a => a -> [a] -> [Int]
-- findSum _ [] = []
-- findSum c v_ = findIndices (== c) (go v_) where
--   go c (v:vs) | (any ((map (+v) vs)) == c) = 

-- go c (v:vs) | (any ((map (+v) vs)) == c) = 

--h' (v:vs) ct = find (== ct) v_ where
  
{- I think this approach is still O(n^2), where
n is a subset of the input data -}
-- findij c vv@(v:vs) = (find w vv, find z vv) where
--   vvf = filter (< c) vv -- supposing every vv > 0
--   cmpSum c u us | (u+hus)==c = (u, hus)
--                 | otherwise = cmpSum c u (tail us)
--     where hus = head us
--   (w, z) = map (\a-> cmpSum c a (delete a vvf)) vvf


