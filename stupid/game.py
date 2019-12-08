import wx
import pb.msg_pb2 as pb
import logging

def otherColor(color):
	return pb.Color.BLACK if color == pb.Color.WHITE else pb.Color.WHITE

class PlayerPane(wx.Panel):
	def __init__(self,parent,game,player,clock):
		super(PlayerPane,self).__init__(parent)
		self.game = game
		self.player = player
		self.pidSt = wx.StaticText(self,label = self.getPidStr())
		self.levelSt = wx.StaticText(self,label = self.getLevelStr())
		self.clockPane = ClockPane(self,game,clock)

		vbox = wx.BoxSizer(wx.VERTICAL)
		vbox.Add(self.pidSt,flag = wx.ALIGN_CENTER)
		vbox.Add(self.levelSt,flag = wx.ALIGN_CENTER)
		vbox.Add(self.clockPane,flag = wx.EXPAND)

		self.SetSizer(vbox)

	def getPidStr(self):
		return str(self.player.pid)

	def getLevelStr(self):
		return str(self.player.level)

class ClockPane(wx.Panel):
	def __init__(self,parent,game,clock):
		super(ClockPane,self).__init__(parent)
		self.game = game
		self.clock = clock
		self.timer = wx.Timer(self)
		self.Bind(wx.EVT_TIMER, self.onClick, self.timer)

		self.baoLiuSt = wx.StaticText(self,label = self.getPaoLiuStr())
		self.ciShuMeiCiSt = wx.StaticText(self,label = self.getCiShuMeiCiStr())
		self.duMiaoSt = wx.StaticText(self,label = self.getDuMiaoStr())

		vbox = wx.BoxSizer(wx.VERTICAL)
		vbox.Add(self.baoLiuSt,flag = wx.ALIGN_CENTER)
		vbox.Add(self.ciShuMeiCiSt,flag = wx.ALIGN_CENTER)
		vbox.Add(self.duMiaoSt,flag = wx.ALIGN_CENTER)
		self.SetSizer(vbox)

	def getPaoLiuStr(self):
		return str(self.clock.baoLiu)

	def getCiShuMeiCiStr(self):
		return str(self.clock.ciShu)+"*"+str(self.game.proto.meiCi)

	def getDuMiaoStr(self):
		return str(self.clock.duMiao)

	def resetDuMiao(self):
		self.clock.duMiao = self.game.proto.duMiao

	def resetMeiCi(self):
		self.clock.duMiao = self.game.proto.meiCi

	def updateView(self):
		self.baoLiuSt.SetLabel(self.getPaoLiuStr())
		self.ciShuMeiCiSt.SetLabel(self.getCiShuMeiCiStr())
		self.duMiaoSt.SetLabel(self.getDuMiaoStr())

	def onClick(self,event):
		if self.clock.baoLiu != 0:
			self.clock.baoLiu -= 1
		elif self.clock.duMiao != 0:
			self.clock.duMiao -= 1
		elif self.clock.ciShu != 0:
			self.clock.ciShu -= 1
			# parent is game
			self.resetMeiCi()
		else:
			# timeout
			self.iamTimeout()
		self.updateView()

	def start(self):
		self.timer.Start(1000)

	def stop(self):
		self.timer.Stop()
		self.resetDuMiao()

class BoardPane(wx.Panel):
	def __init__(self,parent,game):
		super(BoardPane,self).__init__(parent)
		vbox = wx.BoxSizer(wx.VERTICAL)
		self.game = game
		self.outputText = wx.TextCtrl(self, style = wx.TE_MULTILINE | wx.HSCROLL)
		vbox.Add(self.outputText,proportion=1,flag = wx.EXPAND)
		self.SetSizer(vbox)

	def updateView(self):
		self.outputText.SetValue(str(self.game.stones))

	def addStone(self,stone):
		self.game.stones.append(stone)
		self.updateView()

	def getNextColor(self):
		c = len(self.game.stones) 
		if c == 0:
			return pb.Color.BLACK
		else:
			lastStone = self.game.stones[c-1]
			return otherColor(lastStone.color)

class PlayGame(wx.Frame):
	def __init__(self,parent,game):
		super(PlayGame, self).__init__(parent, size=(400, 300))
		self.game = game
		self.SetTitle("***"+self.GetParent().player.pid+"/"+str(self.game.gid)+"***")

		panel = wx.Panel(self)

		self.inputText = wx.TextCtrl(panel,style=wx.TE_PROCESS_ENTER)
		self.inputText.Bind(wx.EVT_TEXT_ENTER,self.onMyAction)

		hbox = wx.BoxSizer()
		self.playerPanes = [PlayerPane(panel,self.game,p,c) for p,c in zip(self.game.players,self.game.clocks)]
		for playerPane in self.playerPanes:
			hbox.Add(playerPane)

		vbox = wx.BoxSizer(wx.VERTICAL)
		vbox.Add(hbox,proportion=0,flag=wx.ALIGN_CENTER)
		self.boardPane = BoardPane(panel,self.game)
		vbox.Add(self.boardPane,proportion=1, flag=wx.EXPAND)
		vbox.Add(self.inputText,proportion=0,flag=wx.EXPAND)

		panel.SetSizer(vbox)

		self.Show()
		self.checkStart()

	def canPutStone(self):
		return self.game.state == pb.State.READY or self.game.state == pb.State.RUNNING

	def startMyClock(self):
		for playerPane in self.playerPanes:
			if playerPane.player == self.myPlayer():
				playerPane.clockPane.start()

	def stopMyClock(self):
		for playerPane in self.playerPanes:
			if playerPane.player == self.myPlayer():
				playerPane.clockPane.stop()

	def checkStart(self):
		if self.canPutStone() and self.isMyTurn():
			self.startMyClock()

	def myColor(self):
		return pb.Color.BLACK if self.game.players[self.game.blackIndex] == self.myPlayer() else pb.Color.WHITE

	def putStone(self,stone):
		if self.isMyTurn():
			self.boardPane.addStone(stone)
			self.stopMyClock()

			msg = pb.Msg()
			msg.type = pb.MsgType.HAND
			msg.hand.gid = self.game.gid
			msg.hand.stone.CopyFrom(stone)
			self.send_msg(msg)
		else:
			self.boardPane.addStone(stone)
			self.checkStart()

	def myPlayer(self):
		return self.GetParent().player

	def isMyTurn(self):
		return self.boardPane.getNextColor() == self.myColor()

	def onMyAction(self,event):
		myline = self.inputText.GetValue().split()
		cmd = myline[0]
		if cmd == "stone":
			if self.canPutStone() and self.isMyTurn():
				stone = pb.Stone()
				stone.color = self.myColor()
				stone.x = int(myline[1])
				stone.y = int(myline[2])
				self.putStone(stone)

	def send_msg(self,msg):
		self.GetParent().send_msg(msg)

	def iamTimeout(self):
		logging.info("timeout:")

		self.game.state = pb.State.ENDED
		self.game.result.winner = otherColor(self.myColor())
		self.game.result.endType = pb.EndType.TIMEOUT

		# tell server i timeout and gameover
		pass
