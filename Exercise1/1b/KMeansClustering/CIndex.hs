module KMeansClustering.CIndex where

import KMeansClustering.Internal
import Data.List (sort)

c :: [[Letter]] -> Letter -> Letter -> Int
c clusters x_i x_j 
    | inSameCluster clusters [x_i, x_j] = 1
    | otherwise = 0

d :: Letter -> Letter -> Float
d x_i x_j = minkowskiDistance 2 (feature x_i) (feature x_j)

combinations :: [[Letter]] -> [(Letter, Letter)]
combinations clusters = [((concat clusters) !! i, (concat clusters) !! j) | i <- [0..pred (length (concat clusters))], j <- [0..pred (length(concat clusters))], i < j]

gamma :: [[Letter]] -> Float
gamma clusters = sum [(fromIntegral (c clusters x_i x_j)) * (d x_i x_j) | (x_i, x_j) <- combinations clusters]

alpha :: [[Letter]] -> Int
alpha clusters =  sum [ c clusters x_i x_j |  (x_i, x_j) <- combinations clusters]

cmax :: [[Letter]] -> Int -> Float
cmax clusters a = sum $ take a $ reverse $ sort [d x_i x_j | (x_i, x_j) <- combinations clusters]

cmin :: [[Letter]] -> Int -> Float
cmin clusters a = sum $ take a $ sort [ d x_i x_j | (x_i, x_j) <- combinations clusters]

c_index :: [[Letter]] -> Float
c_index clusters = top / bottom where
    top = (gamma clusters) - (cmin clusters a) 
    bottom = (cmax clusters a) - (cmin clusters a)
    a = alpha clusters
