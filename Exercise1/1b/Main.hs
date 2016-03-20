module Main where

import KMeansClustering 
import Text.ParserCombinators.Parsec

--parsing
toLetter :: [String] -> KMeansClustering.Letter
toLetter [] = error "empty list"
toLetter (l:f) = KMeansClustering.Letter (read l :: Int) (map (\x -> read x :: Float) f)

main :: IO ()
main = do
    dataSetFile <- readFile "../all.csv"

    let csvDataSet = parseCsv dataSetFile where
        parseCsv = parse csvFile "unknown" 
        csvFile = sepBy line (string "\r\n")
        line = sepBy cell (char ',')
        cell =  many (noneOf "\n\r,")


    case csvDataSet of 
            Left trainError -> do putStrLn "Error parsing CSV"
                                  print trainError
     
            Right dataSet -> do let toSet csv = [toLetter x | x <- csv, x /= [""]]
                                let dataSetSize = 30
                                let welcome = "Clustering with K-Means Clustering"
                                let details = "Data set size: " ++ show dataSetSize 
                                let describe k i = "Clusters: " ++ show k ++ " Iterations: " ++ show i ++ "   -->   "
                                let indexes clusters = "C-Index: " ++ show (c_index clusters) ++ " Goldman Kruskal Index: " ++ show (goldman_kruskal_index clusters)
                                    
                                let clusters k i = KMeansClustering.calculate(toSet(take dataSetSize dataSet)) k i  
                                let showCluster c = [value item | item <- c ]
                                let showResult resultClusters = show $ [showCluster item | item <- resultClusters]
                                let showIndexes k i resultClusters = (describe k i) ++ (indexes resultClusters) ++ "    Clusters: "++ (showResult resultClusters)
                                let resultStrings = [showIndexes k i $ clusters k i | k <- [5, 7, 9, 10, 12, 15], i <- [0, 1, 10, 50, 100]]

                                putStr $ unlines ([welcome, details] ++ resultStrings)
