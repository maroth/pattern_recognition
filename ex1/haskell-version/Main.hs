module Main where

import Data.Function (on)
import Data.List (sortBy, groupBy, minimumBy)
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

toLetter :: [String] -> Letter
toLetter [] = error "empty list"
toLetter (l:f) = Letter (read l :: Int) (map (\x -> read x :: Float) f)

main :: IO ()
main = do
    trainFile <- readFile "../train.csv"
    testFile <- readFile "../test_short.csv"

    let parseFile = parseCsv where
        parseCsv = parse csvFile "unknown" 
        csvFile = sepBy line (string "\r\n")
        line = sepBy cell (char ',')
        cell =  many (noneOf "\n\r,")

    let csvTrainingSet = parseFile trainFile
    let csvTestSet = parseFile testFile

    case csvTrainingSet of 
        Left trainError  -> do putStrLn "Error parsing Training CSV"
                               print trainError
        Right train -> case csvTestSet of 
            Left testError -> do putStrLn "Error parsing Test CSV"
                                 print testError
            Right test -> do let distancemetric = minkowskiDistance 2
                             let testSet = [toLetter x | x <- test, x /= [""]]
                             let trainSet = [toLetter x | x <- train, x /= [""]]
                             let tieBreaker = (\l -> feature l !! 1)
                             let results = [value test == vote tieBreaker (kNearestNeighbor distancemetric trainSet test 5) | test <- testSet]
                             let correctResults = length (filter (\x -> x == True) results)
                             let totalResults = length results
                             let fractionCorrect = (fromIntegral correctResults) / (fromIntegral totalResults)
                             print $ show fractionCorrect
