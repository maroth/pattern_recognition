module Main where

import KMeansClustering 
import Text.ParserCombinators.Parsec

--parsing
toLetter :: [String] -> KMeansClustering.Letter
toLetter [] = error "empty list"
toLetter (l:f) = KMeansClustering.Letter (read l :: Int) (map (\x -> read x :: Float) f)

main :: IO ()
main = do
    dataSetFile <- readFile "../all_short.csv"

    let csvDataSet = parseCsv dataSetFile where
        parseCsv = parse csvFile "unknown" 
        csvFile = sepBy line (string "\r\n")
        line = sepBy cell (char ',')
        cell =  many (noneOf "\n\r,")


    case csvDataSet of 
            Left trainError -> do putStrLn "Error parsing CSV"
                                  print trainError
     
            Right dataSet -> do let toSet csv = [toLetter x | x <- csv, x /= [""]]
                                let dataSetSize = 20
                                let welcome = "Clustering with K-Means Clustering"
                                let details = "Data set size: " ++ show dataSetSize 
                                    
                                let toSet csv = [toLetter x | x <- csv, x /= [""]]

                                let clusters numberOfClusters numberOfIterations = KMeansClustering.calculate(toSet(take dataSetSize dataSet)) numberOfClusters numberOfIterations
                                let showCluster cluster = [value letter | letter <- cluster]
                                let showResult clusters = show $ concat [showCluster cluster | cluster <- clusters]
                                let resultStrings = [showResult $ clusters numberOfClusters numberOfIterations | numberOfClusters <- [5, 7, 9, 10, 12, 15], numberOfIterations <- [0, 1, 10, 50, 100]]

                                putStr $ unlines ([welcome, details] ++ resultStrings)
