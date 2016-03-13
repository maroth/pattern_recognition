module Main where

import KMeansClustering
import Text.ParserCombinators.Parsec

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
                          let dataSetSize = 4
                          let details = "Data set size: " ++ show dataSetSize 
                          putStr $ unlines ([welcome, details])
