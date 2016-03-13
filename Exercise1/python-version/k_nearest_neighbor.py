import unittest
import operator
from math import *
from collections import defaultdict

class KNearestNeighbor:
	class Neighbor:
		def __init__(self, dataPoint, distance):
			self.dataPoint = dataPoint
			self.distance = distance

	def __init__(self, origin, distance, k):
		self.origin = origin
		self.distance = distance
		self.k = k
		self.nearest_neighbors = []

	def check(self, dataPoint):
		def add(dataPoint, distance):
			neighbor = KNearestNeighbor.Neighbor(dataPoint, distance)
			self.nearest_neighbors.append(neighbor)
			self.nearest_neighbors = sorted(self.nearest_neighbors, key=lambda neighbor: neighbor.distance, reverse=True)
			if len(self.nearest_neighbors) > self.k:
				self.nearest_neighbors = self.nearest_neighbors[1:]

		distance = self.distance(self.origin, dataPoint)
		if len(self.nearest_neighbors) < self.k or distance < self.nearest_neighbors[0].distance:
			add(dataPoint, distance)



class KNearestNeighborTest(unittest.TestCase):
	def test_single_neighbor(self):
		origin = DataPoint([1, 1], 1)
		distance = lambda a, b: abs(a.value - b.value)
		testee = KNearestNeighbor(origin, distance, 2)
		neighbor1 = DataPoint([1, 1], 2)
		testee.check(neighbor1)
		self.assertEqual(testee.nearest_neighbors[0].dataPoint, neighbor1)

	def test_max_k_neighbors(self):
		origin = DataPoint([1, 1], 1)
		distance = lambda a, b: abs(a.value - b.value)
		testee = KNearestNeighbor(origin, distance, 2)
		neighbor1 = DataPoint([1, 1], 2)
		testee.check(neighbor1)
		testee.check(neighbor1)
		testee.check(neighbor1)
		self.assertEqual(len(testee.nearest_neighbors), 2)

	def test_further_neighbor_not_added(self):
		origin = DataPoint([1, 1], 1)
		distance = lambda a, b: abs(a.value - b.value)
		testee = KNearestNeighbor(origin, distance, 1)
		neighbor1 = DataPoint([1, 1], 2)
		neighbor2 = DataPoint([1, 1], 100)
		testee.check(neighbor1)
		testee.check(neighbor2)
		self.assertEqual(testee.nearest_neighbors[0].dataPoint, neighbor1)

	def test_closer_neighbor_replaces_further_neighbor(self):
		origin = DataPoint([1, 1], 1)
		distance = lambda a, b: abs(a.value - b.value)
		testee = KNearestNeighbor(origin, distance, 1)
		neighbor1 = DataPoint([1, 1], 2)
		neighbor2 = DataPoint([1, 1], 100)
		testee.check(neighbor2)
		testee.check(neighbor1)
		self.assertEqual(testee.nearest_neighbors[0].dataPoint, neighbor1)

		
