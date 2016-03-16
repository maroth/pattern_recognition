module Main where

import Data.Function (on)
import Data.List (sortBy, groupBy, minimumBy)
import Data.Ord (comparing)
import Text.ParserCombinators.Parsec

--data type
data Letter = Letter Int [Float] deriving (Show)

value :: Letter -> Int
value (Letter val _ ) = val

feature :: Letter -> [Float]
feature (Letter _ feat ) = feat

--k nearest neighbor
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

calculate :: [Letter] -> [Letter] -> Int -> Float -> Double
calculate trainSet testSet k minkowskiParam = fractionCorrect where
    distancemetric = minkowskiDistance minkowskiParam
    tieBreaker = (\a b -> minkowskiDistance minkowskiParam (feature a) (feature b))
    isCorrect result testedLetter = (value testedLetter) == result
    doKnn test = vote (tieBreaker test) (kNearestNeighbor distancemetric trainSet test k)
    results = [isCorrect (doKnn test) test | test <- testSet]
    correctResults = length (filter (\x -> x == True) results)
    totalResults = length results
    fractionCorrect = (fromIntegral correctResults) / (fromIntegral totalResults)


--parsing
toLetter :: [String] -> Letter
toLetter [] = error "empty list"
toLetter (l:f) = Letter (read l :: Int) (map (\x -> read x :: Float) f)

main :: IO ()
main = do
    trainFile <- readFile "../train.csv"
    testFile <- readFile "../test.csv"

    let parseFile = parseCsv where
        parseCsv = parse csvFile "unknown" 
        csvFile = sepBy line (string "\r\n")
        line = sepBy cell (char ',')
        cell =  many (noneOf "\n\r,")

    let csvTrainingSet = parseFile trainFile
    let csvTestSet = parseFile testFile

    let welcome = "classification with K-Nearest Neighbor..."

    case csvTrainingSet of 
        Left trainError  -> do putStrLn "Error parsing Training CSV"
                               print trainError
        Right train -> case csvTestSet of 
            Left testError -> do putStrLn "Error parsing Test CSV"
                                 print testError
            Right test -> do let toSet csv = [toLetter x | x <- csv, x /= [""]]
                             let testSetSize = 40
                             let trainSetSize = length train
                             let showResult minkowskiParam k = "Accuracy for K = " ++ (show k) ++ " and Minkowski Parameter = " ++ (show minkowskiParam) ++ ": " ++ show (calculate (toSet (take trainSetSize train)) (toSet (take testSetSize test)) k minkowskiParam)
                             let resultStrings = [showResult minkowskiParam k | k <- [1, 3, 5, 10, 15], minkowskiParam <- [1, 2]]
                             let details = "Test set size: " ++ show testSetSize ++ ", Train set size: " ++ show trainSetSize
                             putStr $ unlines ([welcome, details] ++ resultStrings)
