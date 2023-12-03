{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( day2
    ) where

import Data.List as L
import qualified Data.Text as T
import Data.Text.Read

day2 :: String -> String
day2 input = unlines [part1, part2]
    where   part1 = show . sum . map fst . filter (\(x, t) -> t) . zip [1..] . map solve . lines $ input
            part2 = show . sum . map solve2 . lines $ input


type Colors = (Int, Int, Int)

part1 :: Colors -> Bool
part1 (r, b, g) = r <= 12 && b <=  14 && g <=  13

solve :: String -> Bool
solve = all part1 . map countColors . T.splitOn "; " . T.pack . unwords . drop 2 . words

solve2 :: String -> Int
solve2 = prod . foldl1 maxSet . map countColors . T.splitOn "; " . T.pack . unwords . drop 2 . words
    where maxSet (r, b, g) (r', b', g') = (max r r', max b b', max g g')
          prod (r, b, g) = r * b * g

countColors :: T.Text -> Colors
countColors = sumColors . map parseColor . T.splitOn ", "
    where
        sumColors = foldl (\(r, b, g) (r', b', g') -> (r + r', b + b', g + g')) (0, 0, 0)
        unpackColor = read . T.unpack
        parseColor x = case (T.words x) of
            [y, "red"] -> (unpackColor y, 0, 0)
            [y, "blue"] -> (0, unpackColor y, 0)
            [y, "green"] -> (0, 0, unpackColor y)
            _ -> undefined
