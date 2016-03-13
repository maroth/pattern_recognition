from decimal import Decimal
from math import *

#minkowski distance implementation from http://dataaspirant.com/2015/04/11/five-most-popular-similarity-measures-implementation-in-python/
def nth_root(value, n_root):
	root_value = 1 / float(n_root)
	return round (Decimal(value) ** Decimal(root_value), 3)
 
def minkowski_distance(x, y, p_value):
	return nth_root(sum(pow(abs(a - b), p_value) for a, b in zip(x, y)), p_value)

def manhattan_distance(a, b):
	return minkowski_distance(a, b, 1)

def geometric_distance(a, b):
	return minkowski_distance(a, b, 2)

		
