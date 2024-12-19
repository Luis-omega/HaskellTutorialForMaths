------
title: Advent of code 2024 problem 2.
date: 2024-11-17
tags: Tutorial, advent of code, 2024
------

This is still a draft!
===

\begin{code}
module Blogs.AOC2024_2 (solve1,solve2) where

import GHC.IO.IOMode (IOMode (ReadMode))
import System.IO (openFile)
import Text.Read (readMaybe)

newtype Report = Report [Int] deriving (Show)

data Direction = Increase | Decrease deriving (Show, Eq)

data Distance = One | Two | Three | Excessive deriving (Show, Eq)

parseReport :: String -> Maybe Report
parseReport input = Report <$> traverse readMaybe (words input)

parseReports :: String -> Maybe [Report]
parseReports input = traverse parseReport (lines input)

ints2Direction :: Int -> Int -> Direction
ints2Direction x y =
  if x < y
    then Increase
    else Decrease

ints2Distance :: Int -> Int -> Distance
ints2Distance x y
     | intDistance == 1 = One
     | intDistance == 2 = Two
     | intDistance == 3 = Three
     | otherwise = Excessive
  where
    intDistance = abs (x - y)

checkReportStart :: Report -> (Direction, Distance, Int, [Int])
checkReportStart (Report (x1 : x2 : tail)) =
  (ints2Direction x1 x2, ints2Distance x1 x2, x2, tail)
checkReportStart _ = error "Bad input!"

checkWith :: Direction -> Int -> [Int] -> Bool
checkWith direction _ [] = True
checkWith direction prev (next : tail) =
  let newDirection = ints2Direction prev next
      newDistance = ints2Distance prev next
   in direction == newDirection
        && (newDistance /= Excessive)
        && checkWith direction next tail

checkReport :: Report -> Bool
checkReport report =
  let (direction, distance, last, tail) = checkReportStart report
   in distance /= Excessive && checkWith direction last tail

countReports :: [Report] -> Int
countReports reports =
  foldr
    (\result acc -> if result then 1 + acc else acc)
    0
    (checkReport <$> reports)

extractOrFail :: Maybe a -> IO a
extractOrFail (Just x) = pure x
extractOrFail Nothing = error "Can't extract!"

solve1 :: IO ()
solve1 = do
  content <- readFile "AdventOfCode/Data/2024/2.txt"
  reports <- extractOrFail $ parseReports content
  let count = countReports reports
  -- the solution for my input! 585
  print count


removeItemN :: Int -> Report -> Report
removeItemN n (Report l) =
  let (start,end) = splitAt n l
  in
  case end of
    [] -> Report start
    (_:xs) -> Report (start++xs)

range :: Int -> [Int]
range n = if n<0 then [] else n : range (n-1)

len :: Report -> Int
len (Report x) = length x

tryRemovingN :: Report -> Int -> Bool
tryRemovingN report n =
  let newReport = removeItemN n report
      checked =  checkReport newReport
  in if checked then  checked else checked

checkReport2 :: Report -> Bool
checkReport2 report =
  let indexes = (reverse . range . len) report
  in
    any ((\ x y -> checkReport x || tryRemovingN x y) report) indexes


countReports2 :: [Report] -> Int
countReports2 reports =
  foldr
    (\result acc -> if result then 1 + acc else acc)
    0
    (checkReport2 <$> reports)


solve2 :: IO ()
solve2 = do
  content <- readFile "AdventOfCode/Data/2024/2.txt"
  reports <- extractOrFail $ parseReports content
  let count = countReports2 reports
  -- the solution for my input! 626
  print count

\end{code}


