module Main where

import Data.List (sortBy, groupBy, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)
import Text.ParserCombinators.Parsec

data Letter = Letter Int [Float] deriving (Show)

value :: Letter -> Int
value (Letter val _ ) = val

feature :: Letter -> [Float]
feature (Letter _ feat ) = feat

minkowskiDistance :: Float -> [Float] -> [Float] -> Float
minkowskiDistance m xs ps = (sum $ zipWith (\x p -> abs $ (x - p) ** m) xs ps) ** (1 / m)

kNearestNeighbor :: ([Float] -> [Float] -> Float) -> [Letter] -> Letter -> Int -> [Letter]
kNearestNeighbor distanceMetric trainingSet test k = take k $ sortBy compareByDistance trainingSet where
    compareByDistance p1 p2 = compare (dist p1) (dist p2)
    dist p = distanceMetric (feature p) (feature test)

vote :: (Letter -> Float) -> [Letter] -> Int
vote tieBreaker neighbors = breakTie $ takeWhileSameLength $ sortByLength $ groupByValue neighbors where
    takeWhileSameLength a = [l | l <- a, length l == length (head a)]
    sortByLength a = sortBy (comparing length) a
    groupByValue a = groupBy ((==) `on` value) a
    breakTie a = if length a == 1 then value $ head $ head a else value $ minimumBy (comparing tieBreaker) (concat a)

csvFile :: GenParser Char st [[String]]
csvFile =
    do result <- many line
       eof
       return result

line :: GenParser Char st [String]
line =
    do result <- cells
       _ <- eol
       return result

cells :: GenParser Char st [String]
cells =
    do first <- cellContent
       next <- remainingCells
       return (first : next)

remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)
    <|> (return [])

cellContent :: GenParser Char st String
cellContent =
    many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCsv :: String -> Either ParseError [[String]]
parseCsv input = parse csvFile ("unknown") input

toLetter :: [String] -> Letter
toLetter [] = error "empty letter"
toLetter (l:f) = Letter (read l :: Int) (map (\x -> read x :: Float) f)

main :: IO ()
main = do
    csvData <- readFile "../train.csv"
    print $ parseCsv (csvData ++ "\n")
