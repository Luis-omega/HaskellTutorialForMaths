------
title: Advent of code 2024 problem 2.
date: 2024-11-17
tags: Tutorial, advent of code, 2024
------

\begin{code}
module Blogs.AOC2024_2 (solve) where

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
ints2Distance x y =
  let intDistance = abs (x - y)
   in if intDistance == 1
        then One
        else
          if intDistance == 2
            then Two
            else
              if intDistance == 3
                then Three
                else Excessive

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
   in if distance == Excessive then False else checkWith direction last tail

countReports :: [Report] -> Int
countReports reports =
  foldr
    (\result acc -> if result then 1 + acc else acc)
    0
    (checkReport <$> reports)

extractOrFail :: Maybe a -> IO a
extractOrFail (Just x) = pure x
extractOrFail Nothing = error "Can't extract!"

solve :: IO ()
solve = do
  content <- readFile "AdventOfCode/Data/2024/2_1.txt"
  reports <- extractOrFail $ parseReports content
  let count = countReports reports
  -- 585
  print count

\end{code}


