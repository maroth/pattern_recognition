import csv
import matplotlib.pyplot as plot
import numpy as np

size = 28

class DataPoint:
	def __init__(self, array, value=None):
		self.value = value
		self.bitmap = [[0 for x in range(size)] for x in range(size)]
		self.feature_array = [int(item) for item in array]
		data_item_index = 0
		for row_index in range(size):
			for col_index in range(size):
				self.bitmap[row_index][col_index] = float(array[data_item_index])
				data_item_index += 1
		
	def show(self):
		plot.imshow(self.bitmap)
		plot.title("value: " + self.value)
		plot.gray()
		plot.show()

		
def load_dataset(filename):
	with open(filename, 'rb') as csv_file:
		return [DataPoint(item[1:], item[0]) for item in csv.reader(csv_file, delimiter=',')]

def manhattan_distance(a, b):
	sum = 0
	for index in range(len(a)):
		sum += abs(a[index] - b[index])
	return sum
		
training_set = load_dataset('train.csv')
test_set = load_dataset('test.csv')

for test_item in test_set:
	min_distance = 99999999
	closest_neighbor = None
	for training_item in training_set:
		distance = manhattan_distance(test_item.feature_array, training_item.feature_array)
		if (distance < min_distance):
			min_distance= distance
			closest_neighbor = training_item
	test_item.show()
	closest_neighbor.show()	
	
	

