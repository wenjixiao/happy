import wx
import pb.msg_pb2 as pb
import logging

class PlayGame(wx.Frame):
	def __init__(self,parent,game):
		super(PlayGame, self).__init__(parent, size=(400, 300))
		self.game = game
		self.SetTitle("***"+self.GetParent().player.pid+"/"+str(self.game.gid)+"***")

		self.timer = wx.Timer(self)
		self.Bind(wx.EVT_TIMER, self.OnTimer, self.timer)

		panel = wx.Panel(self)

		self.clock_text = wx.StaticText(panel,label="***my clock data***")
		
		self.output_text = wx.TextCtrl(panel, style = wx.TE_MULTILINE | wx.HSCROLL)
		self.input_text = wx.TextCtrl(panel,style=wx.TE_PROCESS_ENTER)
		self.input_text.Bind(wx.EVT_TEXT_ENTER,self.OnMyAction)

		hbox = wx.BoxSizer()
		hbox.Add(self.clock_text,flag=wx.ALIGN_CENTER)

		vbox = wx.BoxSizer(wx.VERTICAL)
		vbox.Add(hbox,proportion=0,flag=wx.ALIGN_CENTER)
		vbox.Add(self.output_text,proportion=1, flag=wx.EXPAND | wx.ALL,border=4)
		vbox.Add(self.input_text,flag=wx.EXPAND)

		panel.SetSizer(vbox)

		self.updateView()
		self.Show()
		self.checkStart()

	def canPutStone(self):
		return self.game.state == pb.State.READY or self.game.state == pb.State.RUNNING

	def checkStart(self):
		if self.canPutStone() and self.isMyTurn():
			self.startMyClock()

	def startMyClock(self):
		self.timer.Start(1000)

	def stopMyClock(self):
		self.myClock().duMiao = self.game.proto.duMiao
		self.timer.Stop()

	def updateView(self):
		self.output_text.SetValue(str(self.game.stones))

	def getOtherColor(self,color):
		return pb.Color.BLACK if color == pb.Color.WHITE else pb.Color.WHITE

	def getNextColor(self):
		c = len(self.game.stones) 
		if c == 0:
			return pb.Color.BLACK
		else:
			lastStone = self.game.stones[c-1]
			return self.getOtherColor(lastStone.color)

	def getMyColor(self):
		return pb.Color.BLACK if self.game.players[self.game.blackIndex] == self.myPlayer() else pb.Color.WHITE

	def addStone(self,stone):
		self.game.stones.append(stone)
		self.updateView()

	def putStone(self,stone):
		if self.isMyTurn():
			self.stopMyClock()
			self.addStone(stone)

			msg = pb.Msg()
			msg.type = pb.MsgType.HAND
			msg.hand.gid = self.game.gid
			msg.hand.stone.CopyFrom(stone)
			self.send_msg(msg)
		else:
			self.addStone(stone)
			self.checkStart()

	def myPlayer(self):
		return self.GetParent().player

	def myIndex(self):
		players = self.game.players
		for i,p in enumerate(players):
			if self.myPlayer() == p:
				return i

	def myClock(self):
		return self.game.clocks[self.myIndex()]

	def isMyTurn(self):
		return self.getNextColor() == self.getMyColor()

	def OnMyAction(self,event):
		myline = self.input_text.GetValue().split()
		cmd = myline[0]
		if cmd == "stone":
			if self.canPutStone() and self.isMyTurn():
				stone = pb.Stone()
				stone.color = self.getMyColor()
				stone.x = int(myline[1])
				stone.y = int(myline[2])
				self.putStone(stone)

	def send_msg(self,msg):
		self.GetParent().send_msg(msg)

	def iamTimeout(self):
		logging.info("timeout:")
		self.stopMyClock()

		self.game.state = pb.State.ENDED
		self.game.result.winner = self.getOtherColor(self.getMyColor())
		self.game.result.endType = pb.EndType.TIMEOUT

		# tell server i timeout and gameover
		pass

	def updateClock(self):
		clock = self.myClock()
		s = str(clock.baoLiu)+"--"+str(clock.ciShu)+"--"+str(clock.duMiao)
		self.clock_text.SetLabel(s)

	def OnTimer(self, evt):
		clock = self.myClock()
		if clock.baoLiu != 0:
			clock.baoLiu -= 1
		elif duMiao != 0:
			clock.duMiao -= 1
		elif ciShu != 0:
			clock.ciShu -= 1
			clock.duMiao = self.game.proto.meiCi
		else:
			# timeout
			self.iamTimeout()

		self.updateClock()
