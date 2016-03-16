module TestKMeansClustering where

import Test.HUnit
import System.Exit
import KMeansClustering.Internal
import Data.Function (on)
import System.Exit
import Data.List
import KMeansClustering.Internal

sortClusters :: [[Letter]] -> [[Letter]]
sortClusters = sortBy (compare `on` head)

nextClusterCenter_baseCase = TestCase $ do
    let a = [[1.0]]
    let result = nextClusterCenter a a 0
    assertEqual "single next cluster center incorrect" a result

nextClusterCenter_returnLastCenter = TestCase $ do
    let a = [1.0]
    let b = [2.0]
    let result = nextClusterCenter [a] [b] 1
    assertEqual "single next cluster center incorrect" [b, a] result

nextClusterCenter_returnTwoCenters = TestCase $ do
    let a = [1.0]
    let b = [2.0]
    let c = [3.0]
    let result = nextClusterCenter [a] [b, c] 2
    assertEqual "two more cluster centers incorrect" [b, c, a] result

initialClusterCenters_oneCenterReturned = TestCase $ do 
    let a = Letter 1 [1.0]
    let b = Letter 2 [2.0]
    let result = initialClusterCenters [a, b] 1
    assertEqual "single cluster center not returned correctly" [[1.5]] result

initialClusterCenters_threeCentersReturned = TestCase $ do 
    let a = Letter 1 [1.0]
    let b = Letter 2 [2.0]
    let c = Letter 3 [3.0]
    let result = initialClusterCenters [a, b, c] 3
    assertEqual "single cluster center not returned correctly" [[1.0], [3.0], [2.0]] result

makeClusters_numberOfClustersCorrect = TestCase $ do
    let a = Letter 1 [1.0, 1.0]
    let b = Letter 2 [2.0, 2.0]
    let c = Letter 3 [3.0, 3.0]
    let letters = [a, b, c]
    let centers = [[1.0, 1.0], [2.0, 2.0], [3.0, 3.0]]
    let result = makeClusters letters centers 
    assertEqual "number of clusters not correct" 3 (length result)

makeClusters_numberOfClustersCorrect2 = TestCase $ do
    let a = Letter 1 [1.0, 1.0]
    let b = Letter 2 [2.0, 2.0]
    let c = Letter 3 [3.0, 3.0]
    let letters = [a, b, c, a, b, c, a, b, c]
    let centers = [[1.0, 1.0], [2.0, 2.0], [3.0, 3.0]]
    let result = makeClusters letters centers 
    assertEqual "letters not matched to their correct cluster centers" 3 (length result)

nearestCluster_nearestClusterCorrect = TestCase $ do
    let centers = [[1.0], [5.0], [10.0]]
    let letter = Letter 1 [9.0]
    let result = nearestCluster centers letter
    assertEqual "wrong nearest cluster selected" [10.0] result

calculateWithZeroIterations_numberOfClustersCorrect = TestCase $ do
    let a = Letter 1 [1.0]
    let b = Letter 2 [2.0]
    let c = Letter 3 [3.0]
    let letters = [a, b, c]
    let result = calculate letters 2 0
    assertEqual "wrong number of clusters" 2 (length result)

calculateWithOneIteration_numberOfClustersCorrect = TestCase $ do
    let a = Letter 1 [1.0]
    let b = Letter 2 [2.0]
    let c = Letter 3 [3.0]
    let letters = [a, b, c]
    let result = calculate letters 2 1
    assertEqual "wrong number of clusters" 2 (length result)

calculateWithFiveIterations_numberOfClustersCorrect = TestCase $ do
    let a = Letter 1 [1.0, 1.0]
    let b = Letter 2 [2.0, 1.0]
    let c = Letter 3 [3.0, 1.0]
    let letters = [a, b, c, a, b, c, a, b, c, a, b, c]
    let result = calculate letters 4 5
    assertEqual "wrong number of clusters" 4 (length result)

clusterSanityCheck1 = TestCase $ do
    let a = Letter 1 [1.0, 1.0]
    let b = Letter 2 [2.0, 1.0]
    let c = Letter 3 [3.0, 1.0]
    let letters = [a, b, c, a, b, c, a, b, c]
    let result = sortClusters $ calculate letters 3 100
    assertEqual "sanity check 1 failed" [[a, a, a], [b, b, b], [c, c, c]] result


clusterSanityCheck2 = TestCase $ do
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


inSameCluster :: [[Letter]] -> [Letter] -> Bool
inSameCluster clusters letters = foldl (||) False [ contains cluster | cluster <- clusters] where 
    contains c = foldl (&&) True [ elem l c | l <- letters]


main :: IO()
main = do
    cs@(Counts _ _ errors fails) <- runTestTT $ TestList [
        initialClusterCenters_oneCenterReturned,
        initialClusterCenters_threeCentersReturned,
        makeClusters_numberOfClustersCorrect,
        makeClusters_numberOfClustersCorrect2,
        nextClusterCenter_baseCase,
        nextClusterCenter_returnLastCenter,
        nextClusterCenter_returnTwoCenters,
        nearestCluster_nearestClusterCorrect,
        calculateWithZeroIterations_numberOfClustersCorrect,
        calculateWithOneIteration_numberOfClustersCorrect,
        calculateWithFiveIterations_numberOfClustersCorrect,
        clusterSanityCheck1,
        clusterSanityCheck2
        ]
    putStrLn (showCounts cs)
    if (errors > 0 || fails > 0)
        then exitFailure
        else exitSuccess

