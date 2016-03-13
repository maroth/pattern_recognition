module Main where

import Data.Function (on)
import Data.List (minimumBy, maximumBy, transpose, delete)
import Data.Ord (comparing)
import Text.ParserCombinators.Parsec

--data type
data Letter = Letter Int [Float] deriving (Show)

value :: Letter -> Int
value (Letter val _ ) = val

feature :: Letter -> [Float]
feature (Letter _ feat ) = feat

--Minkowski Distance (m=1:manhattan, m=2:euclidean)
minkowskiDistance :: Float -> [Float] -> [Float] -> Float
minkowskiDistance m xs ps = (sum $ zipWith (\x p -> abs $ (x - p) ** m) xs ps) ** (1 / m)

--K-Means Clustering
clusterCenter :: [Letter] -> [Float]
clusterCenter [] = []
clusterCenter items = map (normalize . sum) $ transpose $ map feature items where
                          normalize a = a / fromIntegral (length items)

nearestCluster :: [[Letter]] -> Letter -> [Letter] 
nearestCluster [] _ = []
nearestCluster (singleCluster:[]) _ = singleCluster
nearestCluster clusters letter = fst $ minimumBy (compare `on` snd) [(cluster, distanceTo cluster) | cluster <- clusters] where
                                     distanceTo cluster = minkowskiDistance 2 (feature letter) (clusterCenter cluster) 




--return a list of cluster centers using spanning cluster center selection
initialClusterCenters :: [Letter] -> Int -> [[Float]]
initialClusterCenters [] _ = []
initialClusterCenters letters numberOfClusters = firstCenter:remainingCenters where
    firstCenter = clusterCenter letters
    remainingCenters = nextClusterCenter [firstCenter] (map feature letters) (pred numberOfClusters)

--returns the list of cluster centers recursively
nextClusterCenter :: [[Float]] -> [[Float]] -> Int -> [[Float]]
nextClusterCenter [] _ _ = []
nextClusterCenter centers letters 0 = centers
nextClusterCenter (center:centers) letters additionalCenters = nextClusterCenter newCenters newLetters newAdditionalCenters where
    newCenters = newCenter:center:centers
    newLetters = delete newCenter letters
    newAdditionalCenters = pred additionalCenters
    newCenter = maxDistance letters center

--return the letter from a group with the largest distance to the passed letter
maxDistance :: [[Float]] -> [Float] -> [Float]
maxDistance [] _ = []
maxDistance (letter:[]) _ = letter
maxDistance letters fromLetter = fst $ maximumBy (compare `on` snd) lettersAndDistance where
    lettersAndDistance = [(letter, distanceTo letter) | letter <- letters] 
    distanceTo letter = minkowskiDistance 2 letter fromLetter


--parsing
toLetter :: [String] -> Letter
toLetter [] = error "empty list"
toLetter (l:f) = Letter (read l :: Int) (map (\x -> read x :: Float) f)

main :: IO ()
main = do
    dataSetFile <- readFile "../all.csv"

    let csvDataSet = parseCsv dataSetFile where
        parseCsv = parse csvFile "unknown" 
        csvFile = sepBy line (string "\r\n")
        line = sepBy cell (char ',')
        cell =  many (noneOf "\n\r,")

    let welcome = "Clustering with K-Means Clustering"

    case csvDataSet of 
        Left trainError -> do putStrLn "Error parsing CSV"
                              print trainError

        Right train -> do let toSet csv = [toLetter x | x <- csv, x /= [""]]
                          let dataSetSize = 40
                          let details = "Data set size: " ++ show dataSetSize 
                          putStr $ unlines ([welcome, details])
