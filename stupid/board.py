import wx
import pb.msg_pb2 as pb
# import logging
from common import otherColor
from itertools import product

class BoardPane(wx.Panel):
	def __init__(self,parent,gameFrame):
		super(BoardPane,self).__init__(parent)
		self.SetBackgroundColour("#FFC0CB")
		self.gameFrame = gameFrame
		self.Bind(wx.EVT_PAINT,self.OnPaint)
		self.Bind(wx.EVT_SIZE,self.OnSize)
		self.stones = []

	def getGame(self):
		return self.gameFrame.game

	def getStones(self):
		return self.getGame().stones

	def OnSize(self,event):
		self.Refresh()

	def OnPaint(self,event):
		dc = wx.PaintDC(self)

		width,height = self.GetClientSize()
		halfWidth,halfHeight = int(width/2),int(height/2)
		radius = int(min(width,height)/(20*2))
		unit = radius * 2

		def user2dev(userPoint):
			"devX = userX+0.5*width,devY = 0.5*height-userY"
			devX = userPoint.x*unit + halfWidth
			devY = halfHeight - userPoint.y*unit
			return wx.Point(devX,devY)

		def dev2user(devPoint):
			"userX = devX - halfWidth,userY = halfHeight - devY"
			userX = (devPoint.x - halfWidth)/unit
			userY = (halfHeight - devPoint.y)/unit
			return wx.Point(userX,userY)

		# draw 19*19 lines
		for i in range(-9,10):
			dc.DrawLine(user2dev(wx.Point(-9,i)),user2dev(wx.Point(9,i)))
			dc.DrawLine(user2dev(wx.Point(i,-9)),user2dev(wx.Point(i,9)))

		# draw 9 stars
		for (x,y) in product([-6,0,6],repeat=2):
			dc.DrawCircle(user2dev(wx.Point(x,y)),int(radius/3))

	def addStone(self,stone):
		# self.getStones().append(stone)
		self.stones.append(stone)
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

# ---------------------------------------------------------
class MainFrame(wx.Frame):
	def __init__(self, parent, title):
		super(MainFrame, self).__init__(parent, title=title, size=(600, 600))
		vbox = wx.BoxSizer(wx.VERTICAL)
		self.board = BoardPane(self,None)
		self.button = wx.Button(self,label="hello")
		self.button.Bind(wx.EVT_BUTTON,self.OnClick)
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
