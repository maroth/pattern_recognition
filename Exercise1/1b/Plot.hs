import Graphics.EasyPlot
import KMeansClustering.Internal

plotLetters clusters centers = plot X11 $ [Data2D [Title "DataSet", Color Red] [] [(((feature l) !! 0), ((feature l) !! 1)) | l <- clusters !! 0],
                                        Data2D [Title "", Color Blue] [] [(((feature l) !! 0), ((feature l) !! 1)) | l <- clusters !! 1],
                                        Data2D [Title "", Color Green] [] [(((feature l) !! 0), ((feature l) !! 1)) | l <- clusters !! 2],
                                        Data2D [Title "", Color Yellow] [] [(((feature l) !! 0), ((feature l) !! 1)) | l <- clusters !! 3],
                                        Data2D [Title "Initial Centers", Color Black] [] [(center !! 0, center !! 1) | center <- centers],
                                        Data2D [Title "Current Centers", Color Grey] [] [((clusterCenter c) !! 0, (clusterCenter c) !! 1) | c <- clusters]]

                                       


main :: IO ()
main = do
    -- let dataSet = [(Letter 1 [a, b]) | a <- [1..100], b <- [1..100]]

    let c1 = [(Letter 1 [a, b]) | a <- [20..30], b <- [40..50]]
    let c2 = [(Letter 1 [a, b]) | a <- [4..20], b <- [90..94]]
    let c3 = [(Letter 1 [a, b]) | a <- [50..55], b <- [1..15]]
    let c4 = [(Letter 1 [a, b]) | a <- [80..90], b <- [29..40]]
    
    let dataSet = c1 ++ c2 ++ c3 ++ c4

    let centers = [[25, 45], [10, 90], [59, 10], [90, 30]]
    let clusters i = cluster dataSet centers i
    --plotLetters (clusters 1)  centers
    --plotLetters (clusters 10)  centers
    --plotLetters (clusters 50)  centers
    plotLetters (clusters 100)  centers
    putStr "yeah"

