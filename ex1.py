import csv
import matplotlib.pyplot as plot
import numpy as np

size = 28

with open('train.csv', 'rb') as train_file:
	train_data = csv.reader(train_file, delimiter=',')
	for data_item in train_data:
		data_item_bitmap = [[0 for x in range(size)] for x in range(size)]
		data_item_index = 0
		for row_index in range(size):
			for col_index in range(size):
				data_item_bitmap[row_index][col_index] = float(data_item[data_item_index + 1])
				data_item_index += 1
				
		print(data_item[0])
		plot.imshow(data_item_bitmap)
		plot.gray()
		plot.show()


