import wx
import pb.msg_pb2 as pb
from common import validatePoint,otherColor,parseStones,Point
import itertools

class BoardPane(wx.Panel):
	def __init__(self,parent,gameFrame):
		super(BoardPane,self).__init__(parent)
		# self.SetBackgroundColour("#FFC0CB")
		self.gameFrame = gameFrame
		self.analyze = True
		self.color2points = None
		self.Bind(wx.EVT_PAINT,self.OnPaint)
		self.Bind(wx.EVT_SIZE,self.OnSize)
		self.Bind(wx.EVT_LEFT_DOWN, self.OnLeftDown)

	def getGame(self):
		return self.gameFrame.game

	def getStones(self):
		return self.getGame().stones
# ---------------------------------------------------------
	def getSize(self):
		width,height = self.GetClientSize()
		halfWidth,halfHeight = int(width/2),int(height/2)
		radius = int(min(width,height)/(20*2))
		unit = radius * 2
		return (halfWidth,halfHeight,radius)

	def user2dev(self,userPoint):
		''' 
			devX = userX+0.5*width
			devY = 0.5*height-userY
		'''
		halfWidth,halfHeight,radius = self.getSize()
		unit = radius * 2

		# 这里和公式不同的是，乘以了一个unit！
		# 以一个格子的长度为一个缩放单位，协调用户坐标和设备坐标。
		devX = userPoint.x*unit + halfWidth
		devY = halfHeight - userPoint.y*unit
		return wx.Point(devX,devY)

	def dev2user(self,devPoint):
		'''
			userX = devX - halfWidth
			userY = halfHeight - devY
		'''
		halfWidth,halfHeight,radius = self.getSize()
		unit = radius * 2

		# 这里得到的是一个浮点数，也许-1.75，也许5.32，等等。
		# 找到和浮点数相近的整数，即用户坐标。
		shangX = (devPoint.x - halfWidth) / unit
		shangY = (halfHeight - devPoint.y) / unit

		return wx.Point(round(shangX),round(shangY))
# ---------------------------------------------------------
	def OnSize(self,event):
		self.Refresh()

	def OnLeftDown(self,e):
		userPoint = self.dev2user(e.GetPosition())
		if validatePoint(userPoint):
			if self.gameFrame.myClockPane().isRunning():
				stone = pb.Stone()
				stone.color = self.gameFrame.myColor()
				stone.x = userPoint.x
				stone.y = userPoint.y
				self.gameFrame.putStone(stone)

	def OnPaint(self,event):
		dc = wx.PaintDC(self)

		_,_,radius = self.getSize()

		# draw 19*19 lines
		for i in range(-9,10):
			dc.DrawLine(self.user2dev(wx.Point(-9,i)),self.user2dev(wx.Point(9,i)))
			dc.DrawLine(self.user2dev(wx.Point(i,-9)),self.user2dev(wx.Point(i,9)))

		# draw 9 stars
		for (x,y) in itertools.product([-6,0,6],repeat=2):
			dc.SetBrush(wx.Brush("#FF7F00"))
			dc.DrawCircle(self.user2dev(wx.Point(x,y)),3)

		# draw stones
		liveStones,deadStones = parseStones(self.getStones())
		for stone in liveStones:
			color = wx.BLACK if stone.color == pb.Color.BLACK else wx.WHITE
			dc.SetBrush(wx.Brush(color))
			dc.DrawCircle(self.user2dev(wx.Point(stone.x,stone.y)),radius)

		# analyze
		if self.analyze:
# ---------------------------------------------------------
			def getStoneAt(x,y):
				for stone in liveStones:
					if stone.x == x and stone.y == y:
						return stone
				return None

			def getPoints(stone,directXY,isAscend):
				"return [(x,y,hasStone,color),...]"
				points = [(stone.x,stone.y,True,stone.color)]

				initXY = stone.x if directXY else stone.y
				numsXY = range(initXY+1,10,1) if isAscend else range(initXY-1,-10,-1)

				if len(numsXY) == 0:
					return points

				for i in numsXY:
					x,y = (i,stone.y) if directXY else (stone.x,i)
					theStone = getStoneAt(x,y)
					if theStone:
						tp = (x,y,True,theStone.color)
					else:
						tp = (x,y,False,0)
					points.append(tp)
				return points

			def selectPoints(points):
				origin = points[0]

				my = []
				other = []
				for i,t in enumerate(points):
					if t[2]:
						if t[3] != origin[3]:
							other.append(i)
						else:
							my.append(i)

				if len(other) == 0:
					return points
				else:
					otherFirst = other[0]
					my1 = list(filter(lambda i: i < otherFirst,my))
					lastIndex = my1[-1]
					return points[:lastIndex+1]
# ---------------------------------------------------------
			self.color2points={pb.Color.BLACK:set(),pb.Color.WHITE:set()}

			for stone in liveStones:
				t1 = selectPoints(getPoints(stone,True,False))
				t2 = selectPoints(getPoints(stone,True,True))
				t3 = selectPoints(getPoints(stone,False,False))
				t4 = selectPoints(getPoints(stone,False,True))

				for x,y,hasStone,color in itertools.chain(t1,t2,t3,t4):
					self.color2points[stone.color].add(Point(x,y))

					mycolor = '#CC7F32' if stone.color == pb.Color.BLACK else '#238E23'
					dc.SetBrush(wx.Brush(mycolor))
					dc.DrawCircle(self.user2dev(wx.Point(x,y)),3)

			sharePoints = self.color2points[pb.Color.BLACK] & self.color2points[pb.Color.WHITE]
			for p in sharePoints:
				dc.SetBrush(wx.Brush('#B000FF'))
				dc.DrawCircle(self.user2dev(wx.Point(p.x,p.y)),3)
# ---------------------------------------------------------
	def addStone(self,stone):
		self.getStones().append(stone)
		self.Refresh()

	def addStones(self,stones):
		self.getStones().extend(stones)
		self.Refresh()

	def getNextColor(self):
		stones = self.getStones()
		c = len(stones)
		if c == 0:
			return pb.Color.BLACK
		else:
			lastStone = stones[c-1]
			return otherColor(lastStone.color)

	def getColorCount(self):
		"b:33,w:22,share:3"
		b = "B:" + str(len(self.color2points[pb.Color.BLACK]))
		w = "W:" + str(len(self.color2points[pb.Color.WHITE]))
		sharePoints = self.color2points[pb.Color.BLACK] & self.color2points[pb.Color.WHITE]
		share = "share:" + str(len(sharePoints))
		return ','.join([b,w,share])

# ---------------------------------------------------------
class MainFrame(wx.Frame):
	def __init__(self, parent, title):
		super(MainFrame, self).__init__(parent, title=title, size=(600, 600))
		
		self.board = BoardPane(self,None)
		self.button = wx.Button(self,label="hello")
		self.button.Bind(wx.EVT_BUTTON,self.OnClick)

		vbox = wx.BoxSizer(wx.VERTICAL)
		vbox.Add(self.board,proportion=1,flag=wx.EXPAND)
		vbox.Add(self.button,flag=wx.EXPAND)
		self.SetSizer(vbox)

		self.Centre()
		self.Show()

	def OnClick(self,event):
		stone = pb.Stone()
		stone.x = 3
		stone.y = 3
		stone.color = pb.Color.BLACK
		self.board.addStone(stone)

if __name__ == '__main__':
	app = wx.App()
	MainFrame(None,title="hello")
	app.MainLoop()
