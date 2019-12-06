import wx
import pb.msg_pb2 as pb
import logging

class PlayGame(wx.Frame):
	def __init__(self,parent,game):
		super(PlayGame, self).__init__(parent, size=(300, 800))
		self.game = game
		self.SetTitle("***"+self.GetParent().player.pid+"/"+str(self.game.gid)+"***")
		panel = wx.Panel(self)
		self.head_text = wx.StaticText(panel,label="***game data***")
		self.output_text = wx.TextCtrl(panel, style = wx.TE_MULTILINE | wx.HSCROLL)
		self.input_text = wx.TextCtrl(panel,style=wx.TE_PROCESS_ENTER)
		self.input_text.Bind(wx.EVT_TEXT_ENTER,self.OnMyAction)
		
		self.timer = wx.Timer(self)
		self.Bind(wx.EVT_TIMER, self.OnTimer, self.timer)
		# self.timer.Start(1000) # 1 second interval

		hbox = wx.BoxSizer()
		hbox.Add(self.head_text,flag=wx.ALIGN_CENTER)

		vbox = wx.BoxSizer(wx.VERTICAL)
		vbox.Add(hbox,proportion=0,flag=wx.ALIGN_CENTER)
		vbox.Add(self.output_text,proportion=1, flag=wx.EXPAND | wx.ALL,border=4)

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
		self.output_text.SetValue(str(game.stones))

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
		return pb.Color.BLACK if self.players[self.game.blackIndex] == self.myPlayer() else pb.Color.WHITE

	def putStone(self,stone):
		if self.isMyTurn():
			self.stopMyClock()
			self.game.stones.append(stone)
			self.updateView()

			msg = pb.Msg()
			msg.type = pb.MsgType.HAND
			msg.hand.gid = self.game.gid
			msg.hand.stone.CopyFrom(stone)
			self.send_msg(hand)
		else:
			self.game.stones.append(stone)
			self.updateView()
			self.checkStart()

	def myPlayer(self):
		return self.GetParent().player

	def myIndex(self):
		return self.game.players.index(self.myPlayer())

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
