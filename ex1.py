import csv
with open('train.csv', 'rb') as train_file:
	train_data = csv.reader(train_file, delimiter=',')
	for row in train_data:
		print ", ".join(row)
