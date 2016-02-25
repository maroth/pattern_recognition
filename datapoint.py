import matplotlib.pyplot as plot
from math import *

class DataPoint:
	def __init__(self, array, value):
		size = int(sqrt(len(array)))
		self.value = value
		self.feature_array = [int(item) for item in array]
		self.bitmap = [[0 for x in range(size)] for x in range(size)]
		data_item_index = 0
		for row_index in range(size):
			for col_index in range(size):
				self.bitmap[row_index][col_index] = float(self.feature_array[data_item_index])
				data_item_index += 1
		
	def show(self):
		plot.imshow(self.bitmap)
		plot.title("value: " + self.value)
		plot.gray()
		plot.show()

