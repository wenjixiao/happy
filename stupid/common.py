import pb.msg_pb2 as pb
import logging

def validate(v):
	return v >= -9 and v <= 9

def validatePoint(point):
	"type(point) = wx.Point"
	return point.x >= -9 and point.x <= 9 and point.y >= -9 and point.y <= 9

def otherColor(color):
	return pb.Color.BLACK if color == pb.Color.WHITE else pb.Color.WHITE

# ---------------------------------------------------------

class Point:
	def __init__(self,x,y):
		self.x = x
		self.y = y

	def __eq__(self,obj):
		return True if self.x == obj.x and self.y == obj.y else False

	def __hash__(self):
		return hash((self.x,self.y))

	def __str__(self):
		return "({},{})".format(self.x,self.y)

	def neighbors(self):
		s = set()
		if validate(self.x-1):
			s.append(Point(self.x-1,self.y))
		if validate(self.x+1):
			s.append(Point(self.x+1,self.y))
		if validate(self.y-1):
			s.append(Point(self.x,self.y-1))
		if validate(self.y+1):
			s.append(Point(self.x,self.y+1))
		return s

class Block:
	def __init__(self,color,point):
		self.color = color
		self.points = set()
		self.points.add(point)

	def hasPoint(self,point):
		return point in self.points

	def addPoint(self,point):
		self.points.add(point)

	def removePoint(self,point):
		self.points.discard(point)

	def isNeighbor(self,point):
		return len(point.neighbors() & self.points) > 0

def canPutStone(liveStones,stone):
	"因为有禁入点，即使是空点，也未必能下子。"
	return True

def moveLive2Dead(liveStones,deadStones):
	"此函数调用在新下一子之后，需把死子挪到deadStones，如果有的话。"
	lastStone = liveStones[len(liveStones)-1]
	lastColor = lastStone.color
	# lastPoint = Point(lastStone.x,lastStone.y)

	yourColor = otherColor(lastColor)

	point2color = {}
	for stone in liveStones:
		point2color[Point(stone.x,stone.y)]=stone.color

	yourPoints = []
	for point,color in point2color:
		if color == yourColor:
			yourPoints.append(point)

	yourBlocks = []
	for point in yourPoints:
		addPointToBlocks(yourColor,point,yourBlocks)

	for block in yourBlocks:
		if countGas(block,point2color) == 0:
			for point in block.points:
				stone = getStone(block.color,point)
				liveStones.remove(stone)
				deadStones.append(stone)

def getStone(color,point):
	stone = pb.Stone()
	stone.color = color
	stone.x = point.x
	stone.y = point.y
	return stone

def countGas(block,point2color):
	blockAndBorder = set()
	for point in block.points:
		blockAndBorder = blockAndBorder | point.neighbors()

	borderPoints = blockAndBorder - block.points
	
	occupy = 0
	for point in borderPoints:
		color = point2color[point]
		if color != block.color:
			occupy += 1

	return len(borderPoints) - occupy

def addPointToBlocks(color,point,blocks):
	"如果这个point，和任何block都不相连，就新建一个block，再加到blocks中。"
	for block in blocks:
		if block.isNeighbor(point):
			block.addPoint(point)
			return
	# point not in any block
	blocks.append(Block(color,point))

def parseStones(stones):
	"把stones分成两个队列，活的和死的。stones必须是有序的。"
	liveStones = []
	deadStones = []

	for stone in stones:
		if canPutStone(liveStones,stone):
			liveStones.append(stone)
			moveLive2Dead(liveStones,deadStones)

	return (liveStones,deadStones)

# ---------------------------------------------------------

if __name__ == '__main__':
	p1 = Point(3,3)
	p2 = Point(3,3)
	s1 = set()
	s1.add(p1)
	s1.add(p2)
	for p in s1:
		print(p)
