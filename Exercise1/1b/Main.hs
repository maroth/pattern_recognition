module Main where

import Data.Function (on)
import Data.List (sortBy, groupBy, minimumBy, transpose)
import Data.Ord (comparing)
import Text.ParserCombinators.Parsec

--data type
data Letter = Letter Int [Float] deriving (Show)

value :: Letter -> Int
value (Letter val _ ) = val

feature :: Letter -> [Float]
feature (Letter _ feat ) = feat

data Cluster = Cluster [Letter] deriving (Show)

--k nearest neighbor
minkowskiDistance :: Float -> [Float] -> [Float] -> Float
minkowskiDistance m xs ps = (sum $ zipWith (\x p -> abs $ (x - p) ** m) xs ps) ** (1 / m)

--K-Means Clustering
clusterCenter :: Cluster -> [Float]
clusterCenter [] = []
clusterCenter items = map (normalize . sum) $ transpose $ map feature items where
                          normalize a = a / fromIntegral (length items)

nearestCluster :: [Cluster] -> Letter -> Cluster
nearestCluster [] _ = []
nearestCluster (singleCluster:[]) _ = singleCluster
nearestCluster clusters letter = fst $ minimumBy compare [(cluster, distanceTo cluster) | cluster <- clusters] where
                                     distanceTo cluster = minkowskiDistance 2 (feature letter) (clusterCenter cluster) 
                                     compare a b = compare (snd a) (snd b)


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
