{-# LANGUAGE OverloadedStrings #-}

module Day3
    ( day3
    ) where

import Data.Char (isNumber)
import Control.Monad

day3 :: String -> String
day3 input = unlines $ map ($ input) [part1, part2]

type Point = (Int, Int)
type Grid = [(Point, Char)]

part1 :: String -> String
part1 input = show symbols
    where
        grid = [((r, c), e) | (r, row) <- zip [0..] $ lines input,
                              (c, e) <- zip [0..] row]
        symbols = filter (\(_, e) -> isSymbol e) grid

data ParseData = ParseData
    { number :: Int
    , start :: Point }

getNums :: String -> (ParseData, String)
getNums input = undefined


doParse :: String -> [(Int, String)]
doParse [] = []
doParse l = let (n, rst) = next l in if null n then [] else read n : doParse rst
    where
        next = span isNumber $ dropWhile $ not . isNumber

neighbors :: Grid -> (Int, Int) -> [(Int, Int)]
neighbors grid point = filter inBounds candidates
    where
        rows = length grid
        cols = length $ grid !! 0
        sumPair (x1,y1) (x2,y2) = (x1+x2, y1+y2)
        candidates = sumPair <$> [(0, 1), (1, 1), (1, 0), (1, -1), (-1, -1), (-1, 0), (-1, 1)] <*> pure point
        inBounds (r, c) = 0 <= r && r < rows && 0 <= c && c < cols




part2 :: String -> String
part2 _ = "0"

isSymbol :: Char -> Bool
isSymbol c = not $ isNumber c || c == '.'
