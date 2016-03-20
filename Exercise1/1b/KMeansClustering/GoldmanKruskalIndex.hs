module KMeansClustering.GoldmanKruskalIndex where

import KMeansClustering.Internal

cbar :: [[Letter]] -> Letter -> Letter -> Int
cbar clusters x_i x_j 
    | inSameCluster clusters [x_i, x_j] = 0
    | otherwise = 1

d :: Letter -> Letter -> Float
d x_i x_j = minkowskiDistance 2 (feature x_i) (feature x_j)

concordant :: [[Letter]] -> (Letter, Letter, Letter, Letter) -> Bool
concordant clusters (x_i, x_j, x_r, x_s)
    | d x_i x_j < d x_r x_s && cbar clusters x_i x_j < cbar clusters x_r x_s = True
    | d x_i x_j > d x_r x_s && cbar clusters x_i x_j > cbar clusters x_r x_s = True
    | otherwise = False

discordant :: [[Letter]] -> (Letter, Letter, Letter, Letter) -> Bool
discordant clusters (x_i, x_j, x_r, x_s)
    | d x_i x_j < d x_r x_s && cbar clusters x_i x_j > cbar clusters x_r x_s = True
    | d x_i x_j > d x_r x_s && cbar clusters x_i x_j < cbar clusters x_r x_s = True
    | otherwise = False

tuples :: [[Letter]] -> [(Letter, Letter, Letter, Letter)]
tuples clusters = [(a, b, c, d) | a <- letters, b <- letters, c <- letters, d <- letters, a /= b, c /= d, (a, b) /= (c, d)] where
    letters = concat clusters

splus :: [[Letter]] -> Int
splus clusters = length $ filter (==True) $ map (concordant clusters) (tuples clusters)

sminus :: [[Letter]] -> Int
sminus clusters = length $ filter (==True) $ map (discordant clusters) (tuples clusters)

goldman_kruskal_index :: [[Letter]] -> Float
goldman_kruskal_index clusters = (fromIntegral top) / (fromIntegral bottom) where
    top = (splus clusters) - (sminus clusters)
    bottom = (splus clusters) + (sminus clusters)
