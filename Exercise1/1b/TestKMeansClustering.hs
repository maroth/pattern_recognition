module TestKMeansClustering where

import Test.HUnit
import System.Exit
import KMeansClustering
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


main :: IO()
main = do
    cs@(Counts _ _ errors fails) <- runTestTT $ TestList [
        initialClusterCenters_oneCenterReturned,
        nextClusterCenter_baseCase,
        nextClusterCenter_returnLastCenter,
        nextClusterCenter_returnTwoCenters
        ]
    putStrLn (showCounts cs)
    if (errors > 0 || fails > 0)
        then exitFailure
        else exitSuccess

