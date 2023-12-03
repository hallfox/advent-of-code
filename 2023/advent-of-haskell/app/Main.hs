module Main (main) where

import Lib
import Day3
import Options.Applicative

data App = App
    { day :: Int
    }

aocOptions :: Parser App
aocOptions = App
    <$> option auto
        (  long "day"
        <> short 'd'
        <> metavar "DAY"
        <> help "The day to run input for"
        )

main :: IO ()
main = doSolution =<< execParser opts
    where
        opts = info (aocOptions <**> helper)
            ( fullDesc
            <> progDesc "Run the solution for DAY"
            <> header "advent-of-haskell - for solving Advent of Code" )

doSolution :: App -> IO ()
doSolution (App d) = interact $ case d of
    2 -> day2
    3 -> day3
    _ -> error "Day not done"
