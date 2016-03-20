Pattern Recognition Exercise 1a - K-Nearest Neighbor
====================================================

Usage
----

setup: 
$ cabal sandbox init

build: 
$ cabal install
$ cabal build

run:
$ cabal run

Datasets must be in ../train.csv and ../test.csv

Scope
-----

As the exercise stated, I wrote a program that reads the test dataset and train
dataset and performs the K-Means classification algorithm on the test dataset
using the train dataset as a reference.

I started writing the program in Python, but the decided that this would be a
good opportunity to learn the Haskell programming language. The language turned
out to be a beautiful fit to describe the actual KNN-Algorithm. However, since
I did not understand the IO-Monad and algebraic data types at the beginning
of the project, it turned out to be hard to actually parse the CSV data, feed 
it into the KNN-algorithm and print out the results.

I ended up learning the structure of the IO monad in Haskell, and implementing
my own simple CSV parser. An interesting problem in parsing the CSV was that
the test and train dataset files lack a newline at the end of their last line.

I fully implemented KNN, but the runtime is very slow. I never ran the program
for the full test dataset, as it would take multiple days to run. There are 
some obvious optimizations that I could make to my program to make it run
faster:

    -Use Vectors of fixed size for the feature vector rather than lists
    -Parallelize the calculations to use multiple cores

In the spirit of the exercise, I should also have implemented the condensing
and editing algorithms. This would have reduced accuracy but drastically 
improved the runtime of the program. Due to lack of time, I was unable to
implement them.


Design
------

I used cabal to structure the Haskell application. Thus, the structure and
dependencies of the program are listed in ex1.cabal, and the program itself is
implemented in Main.hs. Main.hs has the mikowskiDistance, knearestNeighbor, 
vote, and calculate methods that actually do the KNN classification. To use 
KNN on a dataset and return the fraction of correct results, call the calculate
method with the parameters of training set, test set, k and the minkowski 
parameter for the distance metric used. It will return the fraction of correct 
classifications.

Also in Main.hs is the data type used to describe the letters in the test set.
It is called Letter and has the data constructor Letter Int [Float], which
takes the actual value of the letter as the first Int parameter and the feature
vector as the second. If offers the accessors value and letter to retrieve 
those values respectively.

In the main method of Main.js, the datasets are read in and parsed. They are
looked for in "../train.csv" and "../test.csv" respectively. They are then
parsed with the method parseFile, which performs simple CSV parsing. This might
not work on a Windows machine, as I use "\r\n" for line separators. The 
toLetter method can then traslate a CSV line into the algebraic datatype for 
letter. After handling the error cases of parsing errors, the bottom most part
of Main.hs applies KNN and prints the results.

Due to the very long runtime of the program, I only use a subset of the test 
data so I can actually see output in a reasonable time. I can configure this
in the testSetSize parameter. I have currently set this to 40, so only 40
letters are classified. They are however classified against the entire training
set. This could also be changed by modifying the trainSetSize value.

Output on my Machine
--------------------

markus@develop:~/Uni/pattern_recognition/Exercise1/1a$ cabal run
Resolving dependencies...
Configuring ex1a-0.1.0.0...
Preprocessing executable 'ex1a' for ex1a-0.1.0.0...
Running ex1a...
classification with K-Nearest Neighbor...
Test set size: 40, Train set size: 26999
Accuracy for K = 1 and Minkowski Parameter = 1.0: 0.975
Accuracy for K = 1 and Minkowski Parameter = 2.0: 0.975
Accuracy for K = 3 and Minkowski Parameter = 1.0: 0.95
Accuracy for K = 3 and Minkowski Parameter = 2.0: 0.975
Accuracy for K = 5 and Minkowski Parameter = 1.0: 0.925
Accuracy for K = 5 and Minkowski Parameter = 2.0: 0.925
Accuracy for K = 10 and Minkowski Parameter = 1.0: 0.825
Accuracy for K = 10 and Minkowski Parameter = 2.0: 0.825
Accuracy for K = 15 and Minkowski Parameter = 1.0: 0.675
Accuracy for K = 15 and Minkowski Parameter = 2.0: 0.75


Interpretation
--------------

The accuracy seems to be best for a small K, and the distance metric used does
not seem to make a big difference. Larger K values lead to a decreased accuracy.




