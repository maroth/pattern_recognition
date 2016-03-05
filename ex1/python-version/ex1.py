import csv
from distance import manhattan_distance, geometric_distance
from datapoint import DataPoint
from k_nearest_neighbor import KNearestNeighbor
from math import *


def load_dataset(filename):
	print("loading file " + filename + "...")
	with open(filename, 'rb') as csv_file:
		return [DataPoint(item[1:], item[0]) for item in csv.reader(csv_file, delimiter=',')]

training_set = load_dataset('train.csv')
test_set = load_dataset('test.csv')

for test_item in test_set:
	knn = KNearestNeighbor(test_item, manhattan_distance, 3)
	for training_item in training_set:
		knn.check(training_item)

	correct = test_item.value == closest_neighbor.value
	if not correct:
		test_item.show()
		closest_neighbor.show()

