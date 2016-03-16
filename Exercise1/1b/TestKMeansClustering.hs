module TestKMeansClustering where

import Test.HUnit
import System.Exit
import KMeansClustering.Internal

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
    let result = calculate letters 3 100
    assertEqual "sanity check 1 failed" [[a, a, a], [b, b, b], [c, c, c]]



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
        calculateWithFiveIterations_numberOfClustersCorrect 
        ]
    putStrLn (showCounts cs)
    if (errors > 0 || fails > 0)
        then exitFailure
        else exitSuccess

