module KMeansClustering.TestCIndex where

import Test.HUnit
import KMeansClustering.Internal
import KMeansClustering.CIndex
import Data.Function (on)
import System.Exit
import Data.List

sortClusters :: [[Letter]] -> [[Letter]]
sortClusters = sortBy (compare `on` head)

clusterSanityCheck = TestCase $ do
    let a1 = Letter 1 [1.1, 0.1]
    let a2 = Letter 1 [0.9, 0.05]
    let a3 = Letter 1 [0.95, 0.12]

    let b1 = Letter 2 [1.11, 0.95]
    let b2 = Letter 2 [1.2, 1.03]
    let b3 = Letter 2 [0.9, 1.1]

    let c1 = Letter 3 [0.1, 0.9]
    let c2 = Letter 3 [0.05, 1.1]
    let c3 = Letter 3 [0.2, 1.2]

    let d1 = Letter 3 [0.1, 0.05]
    let d2 = Letter 3 [0.04, 0.2]
    let d3 = Letter 3 [0.2, 0.1]

    let letters = [d3, c2, b1, a3, d2, c1, b3, a2, d1, c3, b2, a1]
    let result = sortClusters $ calculate letters 4 100
    assertEqual "sanity check 2 failed" (inSameCluster result [a1, a2, a3]) True
    assertEqual "sanity check 2 failed" (inSameCluster result [b1, b2, b3]) True
    assertEqual "sanity check 2 failed" (inSameCluster result [c1, c2, c3]) True
    assertEqual "sanity check 2 failed" (inSameCluster result [d1, d2, d3]) True


main :: IO()
main = do
    cs@(Counts _ _ errors fails) <- runTestTT $ TestList [
        clusterSanityCheck
        ]
    putStrLn (showCounts cs)
    if (errors > 0 || fails > 0)
        then exitFailure
        else exitSuccess

