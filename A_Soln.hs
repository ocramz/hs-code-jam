module A_Soln where

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

fnameIn = "A-small-practice.in"
fnameOut = "A-small-practice.out"

data Case = Case { cTot :: Int,
                   nItems :: Int,
                   cItems :: [Int]} deriving (Show, Eq)

main = do
  inh <- openFile fnameIn ReadMode
  --outh <- openFile fnameOut WriteMode
  inStr <- hGetContents inh
  --let result = processData inStr
  let result = readIntLists inStr
  --hPutStr outh result
  print result
  hClose inh
  --hClose outh

a :: [[Int]]
a = [[1,2],[3,4],[5,6],[7,8],[9,10],[11,12]]
s :: String
s = "12\n10\n20 30 40\n"

readInts = map readInt . words
readInt x = read x :: Int

readIntLists :: String -> [[Int]]
readIntLists = map readInts . lines

caseC :: [[Int]] -> Case
caseC s = Case {cTot = c, nItems = n, cItems = ci} where
  c = head (head s)
  n = head s!!1
  ci = s!!2

--processData :: String -> [Case]
-- processData s = map (caseC . triple) casesRaw where
--   dat = readIntLists s
--   --nCases = head dat
--   casesRaw = tail dat
--   triple = take 3

dat = readIntLists s


-- getLineInt :: Handle -> IO [Int]
-- getLineInt h = do
--   line <- hGetLine h
--   case readMaybe line of
--     Just x -> return $ readInts x
--     Nothing -> getLineInt h

    


