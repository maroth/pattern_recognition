
import unittest

def vote(nearest_neighbors):
	values = defaultdict(int)
	for neighbor in nearest_neighbors:
		values[neighbor.value] += 1
	votes = sorted(values, key=values.get, reverse=True)
	winners = [vote for vote in votes if vote.
		

class VoteTest(unittest.TestCase):

	def test(self):
		n1 = :
		nearest_neighbors = 

		
