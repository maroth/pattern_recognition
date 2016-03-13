module KMeansClustering where 

import Data.Function (on)
import Data.List (minimumBy, maximumBy, groupBy, transpose, delete)

--data type
data Letter = Letter Int [Float] deriving (Show)

value :: Letter -> Int
value (Letter val _ ) = val

feature :: Letter -> [Float]
feature (Letter _ feat ) = feat

--K-Means Clustering
cluster :: [Letter] -> [[Float]] -> Int -> [[Letter]]
cluster letters clusterCenters 0 = makeClusters letters clusterCenters
cluster letters clusterCenters remainingIterations = cluster letters newClusterCenters (pred remainingIterations) where
    newClusters = makeClusters letters clusterCenters
    newClusterCenters = map clusterCenter newClusters

--Minkowski Distance (m=1:manhattan, m=2:euclidean)
minkowskiDistance :: Float -> [Float] -> [Float] -> Float
minkowskiDistance m xs ps = (sum $ zipWith (\x p -> abs $ (x - p) ** m) xs ps) ** (1 / m)

--Find the center of a cluster given by a list of letters
clusterCenter :: [Letter] -> [Float]
clusterCenter [] = []
clusterCenter items = map (normalize . sum) $ transpose $ map feature items where
    normalize a = a / fromIntegral (length items)

--Find the center of the nearest cluster given a letter and a list of cluster centers
nearestCluster :: [[Float]] -> Letter -> [Float]
nearestCluster [] _ = []
nearestCluster (singleCluster:[]) _ = singleCluster
nearestCluster clusterCenters letter = fst $ minimum [(clusterCenter, distanceTo clusterCenter) | clusterCenter <- clusterCenters] where
    distanceTo clusterCenter = minkowskiDistance 2 (feature letter) clusterCenter

--make clusters by assigning each letter to its closest cluster center
makeClusters :: [Letter] -> [[Float]] -> [[Letter]]
makeClusters letters clusterCenters = groupBy (\a b -> nearestCluster clusterCenters a == nearestCluster clusterCenters b) letters

--return a list of cluster centers using spanning cluster center selection
initialClusterCenters :: [Letter] -> Int -> [[Float]]
initialClusterCenters [] _ = []
initialClusterCenters letters numberOfClusters = remainingCenters where
    firstCenter = clusterCenter letters
    remainingCenters = nextClusterCenter [firstCenter] (map feature letters) (pred numberOfClusters)

--returns the list of cluster centers recursively
nextClusterCenter :: [[Float]] -> [[Float]] -> Int -> [[Float]]
nextClusterCenter [] _ _ = []
nextClusterCenter centers _ 0 = centers
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

