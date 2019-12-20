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
			s.add(Point(self.x-1,self.y))
		if validate(self.x+1):
			s.add(Point(self.x+1,self.y))
		if validate(self.y-1):
			s.add(Point(self.x,self.y-1))
		if validate(self.y+1):
			s.add(Point(self.x,self.y+1))
		return s

class Block:
	def __init__(self,color,point):
		self.color = color
		self.points = set()
		self.points.add(point)

	def isNeighbor(self,point):
		return len(point.neighbors() & self.points) > 0

def canPutStone(liveStones,stone):
	"因为有禁入点，即使是空点，也未必能下子。"
	return True

def moveLive2Dead(liveStones,deadStones):
	"此函数调用在新下一子之后，需把死子挪到deadStones，如果有的话。"
	lastStone = liveStones[len(liveStones)-1]
	lastColor = lastStone.color

	# 我下，当然不能我死，肯定是你死
	yourColor = otherColor(lastColor)

	# 把stones，翻译成point->color的全局map。
	point2color = {}
	for stone in liveStones:
		point2color[Point(stone.x,stone.y)]=stone.color

	# 所有是你颜色的点
	yourPoints = []
	for point,color in point2color.items():
		if color == yourColor:
			yourPoints.append(point)

	# 把你所有的点，分成block
	yourBlocks = []
	for point in yourPoints:
		addPointToBlocks(yourColor,point,yourBlocks)

	# 计算你每个block的气数
	for block in yourBlocks:
		if countGas(block,point2color) == 0:
			for point in block.points:
				# 这个操作手法有些变态，不过有用
				stone = pb.Stone()
				stone.color = block.color
				stone.x = point.x
				stone.y = point.y

				liveStones.remove(stone)
				deadStones.append(stone)

def addPointToBlocks(color,point,blocks):
	"如果这个point，和任何block都不相连，就新建一个block，再加到blocks中。"
	n = 0
	for block in blocks:
		if block.isNeighbor(point):
			block.points.add(point)
			n += 1
	if n == 0:
		blocks.append(Block(color,point))
	if n > 1:
		# point被加入到了两个以上的block中，需要合并一下block
		myblocks = []
		myblock = Block(color,point)
		for block in blocks:
			if point in block.points:
				myblock.points = myblock.points | block.points
			else:
				myblocks.append(block)
		myblocks.append(myblock)
		blocks = myblocks

def countGas(block,point2color):
	'''
	知道block，知道每个点的颜色，足矣！
	1，得到block和周边的所有点
	2，从上面结果，去除掉block的点
	3，剩下周边的点。从周边的点中，去除被对方占用的点
	4，剩下周边没被占用的点，即当前气数
	'''
	blockAndBorder = set()
	for point in block.points:
		blockAndBorder = blockAndBorder | point.neighbors()

	borderPoints = blockAndBorder - block.points
	occupy = 0
	for point in borderPoints:
		if point in point2color and point2color[point] != block.color:
			occupy += 1

	return len(borderPoints) - occupy

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
