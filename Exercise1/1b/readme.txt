Pattern Recognition Exercise 1b - K-Means Clustering
====================================================

Scope
-----

I had decided to do Exercise 1a in Haskell, where the biggest problem was 
parsing the CSV data and understanding the IO monad and the separation of
pure Haskell code and functions with side effects. Since the data is the 
same in exercise 1b, I decided to continue to solve the exercised in Haskell, 
because I had already done the hard part.

I implemented K-Means Clustering, the C-Index and the Goodman-Kruskal-Index
as well as deterministic K-Means for initial cluster center selection.

The runtime of the program is extremenly long. The clustering itself takes
a long time already, and the indexes take even more time. Running the program
for the entire dataset of around 40k letters would take weeks at least.

I wrote some tests to verify that my code works correctly for simple cases, 
and was able to print 2D-plots to sanity check the clustering of simple data 
using two-dimensional features. Here, the algorithm seems to work as intended.

Using randomness in Haskell means having side-effects, so you need to use a 
monad to transfer the state of the random number generator between methods
that create random numbers. Since I still don't completely understand mondads,
I decided to implement the deterministic K-Means algorithm instead. This worked
rather well, but was not ideal for some test data sets.

As a termination criteria I simply used a fixed number of iterations. 

The optimization potential from exercise 1a remains: Vectors instead of lists
for the feature data, and parallelization. Parallelization should improve
performance by factor of 4 on a quad-core CPU, and using vectors instead of
lists should also bring an improvement by quite a bit, as they are of fixed 
size so the compiler can to all kinds of optimizations on the calculations.
I also believe that it would be more efficient to calculate the manhattan
distance directly rather than using the Minkowski distance formula, but I have
not verified that. Maybe the compiler optimizes this anyway.

The maximum data set I was willing to wait for to run had only the first 30
elements of the 40k data set. I am not satisfied with this result. I am sure
that using Python and NumPy/SciPy or R would have enabled me to get results
much quicker and so spend more time optimizing the actual clustering algorithm.

Doing this in Haskell was a very rewarding experience in any case, even though
I did spend a very long time on the exercises. I look forward to working in
Python for the group project, but will continue to use Haskell whenever 
feasible. It is truly a remarkable language, and even only understanding so 
little of it has made me fall hopelessly in love with it.

For example, the implementation of the Goodman-Kruskal-Index basically 
consisted of copying the mathematical definition of it. That was it. The code
is just the definition, and it worked pretty much the first time I ran it.
Amazing!


Design
------

I again used cabal to structure the application. Since this one seemed to be
a bit larger than 1a, I split the code into multiple files. The Mains.hs
parses the dataset and calls the clustering code, which itself lives in 
KMeansClustering/Internals.hs. There exists a public interface to the public
methods used in KMeansClustering.hs. To use my code as a library, it should
be imported rather than Internals.hs.

The indexes live in their own respective files in KMeansClustering/CIndex.hs
and KMeansClustering/GoodmanKruskalIndex.hs. Both of them expose a public
function that takes a list of list of letters (the clustering) and returns
the index value as a float.

I wrote a sanity test for the CIndex in KMeansClustering/TestCIndex.hs which
seems to work well. I wasn't able to write tests for Goodman-Kruskal due to
time constraints.

There are general tests for some of the internal methods of the K-Means-
Clustering in TestKMeansClustering.hs. 

All tests can be run by using "runhaskell <file>.hs". They use HUnit and will
return 0 if all tests pass and 1 otherwise.

In Plot.hs I used Graphics.EasyPlot to plot the results of some sanity checks
on 2D clustering data so I would validate that my implementation of the 
clustering is not obviously broken. It worked out quite well.

ex1.cabal again contains the dependencies and executable configuration for 
cabal.


Output on my Machine 
--------------------

The output displays the results of the program when run with the first 30 
entries of the dataset. This alone took multiple hours to run. I manually 
aligned the output for better reading.

It displays the two indexes as well as the actual letter values of the 
letters in the clusters.

markus@develop:~/Uni/pattern_recognition/Exercise1/1b$ cabal run
Preprocessing executable 'ex1' for ex1b-0.1.0.0...
[5 of 5] Compiling Main             ( Main.hs, dist/build/ex1/ex1-tmp/Main.o )
Linking dist/build/ex1/ex1 ...
Running ex1...
Clustering with K-Means Clustering
Data set size: 30
Clusters:  5 Iterations:   0   -->   C-Index: 0.34866557 Goldman Kruskal Index: 0.2851816    Clusters: [[1,4,0,7,3,1,1,7,6,7],[0],[0,0],[5,3,8,9,3,3,2,0,5,8,2,2,3,9],[1,6,9]]
Clusters:  5 Iterations:   1   -->   C-Index: 0.24844815 Goldman Kruskal Index: 0.5071329    Clusters: [[0],[0,0],[3,3,3,2,5,2,2,3],[1,4,0,7,3,1,7,8,6,7],[1,5,8,9,1,0,6,9,9]]
Clusters:  5 Iterations:  10   -->   C-Index: 0.24844815 Goldman Kruskal Index: 0.5071329    Clusters: [[0],[0,0],[3,3,3,2,5,2,2,3],[1,4,0,7,3,1,7,8,6,7],[1,5,8,9,1,0,6,9,9]]
Clusters:  5 Iterations:  50   -->   C-Index: 0.24844815 Goldman Kruskal Index: 0.5071329    Clusters: [[0],[0,0],[3,3,3,2,5,2,2,3],[1,4,0,7,3,1,7,8,6,7],[1,5,8,9,1,0,6,9,9]]
Clusters:  5 Iterations: 100   -->   C-Index: 0.24844815 Goldman Kruskal Index: 0.5071329    Clusters: [[0],[0,0],[3,3,3,2,5,2,2,3],[1,4,0,7,3,1,7,8,6,7],[1,5,8,9,1,0,6,9,9]]
Clusters:  7 Iterations:   0   -->   C-Index: 0.30290726 Goldman Kruskal Index: 0.40356344   Clusters: [[8,9],[1,4,0,7,3,1,1,7,6,7],[0],[0,5],[0],[5,3,9,3,3,2,0,8,2,2,3,9],[1,6]]
Clusters:  7 Iterations:   1   -->   C-Index: 0.28522837 Goldman Kruskal Index: 0.44732508   Clusters: [[8,9,9],[0],[0,0],[5],[5,3,3,3,2,0,8,2,2,3],[1,4,0,7,3,1,7,6,9,7],[1,1,6]]
Clusters:  7 Iterations:  10   -->   C-Index: 0.20254031 Goldman Kruskal Index: 0.5851411    Clusters: [[5,8,9,0,8,9,9],[4,0,3,7],[0],[0,0],[5],[3,3,3,2,2,2,3],[1,1,7,1,1,7,6,6]]
Clusters:  7 Iterations:  50   -->   C-Index: 0.20254031 Goldman Kruskal Index: 0.5851411    Clusters: [[5,8,9,0,8,9,9],[4,0,3,7],[0],[0,0],[5],[3,3,3,2,2,2,3],[1,1,7,1,1,7,6,6]]
Clusters:  7 Iterations: 100   -->   C-Index: 0.20254031 Goldman Kruskal Index: 0.5851411    Clusters: [[5,8,9,0,8,9,9],[4,0,3,7],[0],[0,0],[5],[3,3,3,2,2,2,3],[1,1,7,1,1,7,6,6]]
Clusters:  9 Iterations:   0   -->   C-Index: 0.2270237  Goldman Kruskal Index: 0.5675269    Clusters: [[1,7,5,9,1,7,8,9,9,7],[8],[1,4,0,3,1,6],[0],[0],[0],[5],[3,3,3,2,0,2,2,3],[6]]
Clusters:  9 Iterations:   1   -->   C-Index: 0.17169864 Goldman Kruskal Index: 0.6413877    Clusters: [[8],[1,7,5,9,1,1,7,8,9,9,7],[0],[0],[0],[5],[3,3,3,2,2,2,3],[1,4,0,3,0,6],[6]]
Clusters:  9 Iterations:  10   -->   C-Index: 0.15384313 Goldman Kruskal Index: 0.6701031    Clusters: [[8],[1,1,7,5,9,1,1,7,8,9,9,7],[0],[0],[0],[5],[3,3,3,2,2,2,3],[4,0,3,0,6],[6]]
Clusters:  9 Iterations:  50   -->   C-Index: 0.15384313 Goldman Kruskal Index: 0.6701031    Clusters: [[8],[1,1,7,5,9,1,1,7,8,9,9,7],[0],[0],[0],[5],[3,3,3,2,2,2,3],[4,0,3,0,6],[6]]
Clusters:  9 Iterations: 100   -->   C-Index: 0.15384313 Goldman Kruskal Index: 0.6701031    Clusters: [[8],[1,1,7,5,9,1,1,7,8,9,9,7],[0],[0],[0],[5],[3,3,3,2,2,2,3],[4,0,3,0,6],[6]]
Clusters: 10 Iterations:   0   -->   C-Index: 0.22975428 Goldman Kruskal Index: 0.60609996   Clusters: [[7,3,7],[1,5,9,1,7,8,9,9],[8],[1,4,0,3,1,6],[0],[0],[0],[5],[3,3,2,0,2,2,3],[6]]
Clusters: 10 Iterations:   1   -->   C-Index: 0.19529878 Goldman Kruskal Index: 0.63569474   Clusters: [[8],[7,3,7],[1,5,9,1,1,7,8,9,9],[0],[0],[0],[5],[3,3,2,0,2,2,3],[1,4,0,3,6],[6]]
Clusters: 10 Iterations:  10   -->   C-Index: 0.19529878 Goldman Kruskal Index: 0.63569474   Clusters: [[8],[7,3,7],[1,5,9,1,1,7,8,9,9],[0],[0],[0],[5],[3,3,2,0,2,2,3],[1,4,0,3,6],[6]]
Clusters: 10 Iterations:  50   -->   C-Index: 0.19529878 Goldman Kruskal Index: 0.63569474   Clusters: [[8],[7,3,7],[1,5,9,1,1,7,8,9,9],[0],[0],[0],[5],[3,3,2,0,2,2,3],[1,4,0,3,6],[6]]
Clusters: 10 Iterations: 100   -->   C-Index: 0.19529878 Goldman Kruskal Index: 0.63569474   Clusters: [[8],[7,3,7],[1,5,9,1,1,7,8,9,9],[0],[0],[0],[5],[3,3,2,0,2,2,3],[1,4,0,3,6],[6]]
Clusters: 12 Iterations:   0   -->   C-Index: 0.1974865  Goldman Kruskal Index: 0.6917598    Clusters: [[7,3,7],[1,5,9,1,7,8,9,9],[8],[4,0,3,1],[0,6],[1,3,3,2,2,2,3],[0],[0],[0],[5],[6]]
Clusters: 12 Iterations:   1   -->   C-Index: 0.15562518 Goldman Kruskal Index: 0.72843665   Clusters: [[8],[7,3,7],[1,5,9,1,1,7,8,9,9],[4,0,3],[0],[0],[0],[5],[1,3,3,2,2,2,3],[0,6],[6]]
Clusters: 12 Iterations:  10   -->   C-Index: 0.15562518 Goldman Kruskal Index: 0.72843665   Clusters: [[8],[7,3,7],[1,5,9,1,1,7,8,9,9],[4,0,3],[0],[0],[0],[5],[1,3,3,2,2,2,3],[0,6],[6]]
Clusters: 12 Iterations:  50   -->   C-Index: 0.15562518 Goldman Kruskal Index: 0.72843665   Clusters: [[8],[7,3,7],[1,5,9,1,1,7,8,9,9],[4,0,3],[0],[0],[0],[5],[1,3,3,2,2,2,3],[0,6],[6]]
Clusters: 12 Iterations: 100   -->   C-Index: 0.15562518 Goldman Kruskal Index: 0.72843665   Clusters: [[8],[7,3,7],[1,5,9,1,1,7,8,9,9],[4,0,3],[0],[0],[0],[5],[1,3,3,2,2,2,3],[0,6],[6]]
Clusters: 15 Iterations:   0   -->   C-Index: 0.18473804 Goldman Kruskal Index: 0.7583377    Clusters: [[7,7],[1,5,9,1,7,8,9,9],[8],[2],[4,0,3,1],[3],[0],[1,3,3,2,2,3],[0],[0],[0],[5],[6],[6]]
Clusters: 15 Iterations:   1   -->   C-Index: 0.1277592  Goldman Kruskal Index: 0.83394605   Clusters: [[7,7],[8],[2,2],[3],[0],[1,5,9,1,1,7,8,9,9],[4,0,3],[0],[0],[0],[5],[1,3,3,2,3],[6],[6]]
Clusters: 15 Iterations:  10   -->   C-Index: 0.11877146 Goldman Kruskal Index: 0.8409644    Clusters: [[7,7],[8],[3],[0],[1,5,9,1,1,7,8,9,9],[4,0,3],[2,2,2],[0],[0],[0],[5],[1,3,3,3],[6],[6]]
Clusters: 15 Iterations:  50   -->   C-Index: 0.11877146 Goldman Kruskal Index: 0.8409644    Clusters: [[7,7],[8],[3],[0],[1,5,9,1,1,7,8,9,9],[4,0,3],[2,2,2],[0],[0],[0],[5],[1,3,3,3],[6],[6]]
Clusters: 15 Iterations: 100   -->   C-Index: 0.11877146 Goldman Kruskal Index: 0.8409644    Clusters: [[7,7],[8],[3],[0],[1,5,9,1,1,7,8,9,9],[4,0,3],[2,2,2],[0],[0],[0],[5],[1,3,3,3],[6],[6]]



Interpretation
--------------

The clustering does not work very well with the datset size of 30 and the pixel
values as the feature vector. Also, increasing the amount of iterations does
not seem to increase clustering quality. I suspect this is because the number
of samples is so small relative to the number of clusters. 

I wish I had (much) more time to improve the program performance-wise and to 
see if this would improve with larger dataset sizes. I also think that it might
be possible to use better metrics of cluster quality given that we know the 
correct value for each of the features. And we might optimize this drastically
by using better beginning cluster centers. I would also like to use randomness
to experiment with different cluster centers rather than only using 
deterministic K-Means clustering.


