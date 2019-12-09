import wx
import pb.msg_pb2 as pb
import logging

def otherColor(color):
	return pb.Color.BLACK if color == pb.Color.WHITE else pb.Color.WHITE

class PlayerPane(wx.Panel):
	def __init__(self,parent,gameFrame,player,clock):
		super(PlayerPane,self).__init__(parent)
		self.gameFrame = gameFrame
		self.player = player
		self.pidSt = wx.StaticText(self,label = self.getPidStr())
		self.levelSt = wx.StaticText(self,label = self.getLevelStr())
		self.clockPane = ClockPane(self,self.gameFrame,clock)

		grid = wx.FlexGridSizer(rows=1,cols=3,vgap=2,hgap=2)
		grid.Add(self.pidSt,proportion=1,flag = wx.ALIGN_CENTER)
		grid.Add(self.levelSt,proportion=1,flag = wx.ALIGN_CENTER)
		grid.Add(self.clockPane,proportion=2,flag = wx.EXPAND)

		grid.AddGrowableCol(0, 1)
		grid.AddGrowableCol(1, 1)
		grid.AddGrowableCol(2, 1)

		self.SetSizer(grid)

	def getPidStr(self):
		return str(self.player.pid)

	def getLevelStr(self):
		return str(self.player.level)

class ClockPane(wx.Panel):
	def __init__(self,parent,gameFrame,clock):
		super(ClockPane,self).__init__(parent)
		self.gameFrame = gameFrame
		self.clock = clock
		self.timer = wx.Timer(self)
		self.Bind(wx.EVT_TIMER, self.onClick, self.timer)

		self.baoLiuSt = wx.StaticText(self,label = self.getPaoLiuStr())
		self.ciShuMeiCiSt = wx.StaticText(self,label = self.getCiShuMeiCiStr())
		self.duMiaoSt = wx.StaticText(self,label = self.getDuMiaoStr())

		grid = wx.GridSizer(rows=1,cols=3,vgap=2,hgap=2)
		grid.Add(self.baoLiuSt,proportion=0,flag = wx.ALIGN_CENTER)
		grid.Add(self.ciShuMeiCiSt,proportion=0,flag = wx.ALIGN_CENTER)
		grid.Add(self.duMiaoSt,proportion=0,flag = wx.ALIGN_CENTER)
		self.SetSizer(grid)

	def getPaoLiuStr(self):
		return str(self.clock.baoLiu)

	def getCiShuMeiCiStr(self):
		return str(self.gameFrame.game.proto.meiCi)+"*"+str(self.clock.ciShu)

	def getDuMiaoStr(self):
		return str(self.clock.duMiao)

	def resetDuMiao(self):
		self.clock.duMiao = self.gameFrame.game.proto.duMiao

	def resetMeiCi(self):
		self.clock.duMiao = self.gameFrame.game.proto.meiCi

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
			self.gameFrame.iamTimeout()

		self.updateView()

	def start(self):
		self.timer.Start(1000)

	def stop(self):
		self.timer.Stop()
		self.resetDuMiao()

class BoardPane(wx.Panel):
	def __init__(self,parent,gameFrame):
		super(BoardPane,self).__init__(parent)
		vbox = wx.BoxSizer(wx.VERTICAL)
		self.gameFrame = gameFrame
		self.outputText = wx.TextCtrl(self, style = wx.TE_MULTILINE | wx.HSCROLL)
		vbox.Add(self.outputText,proportion=1,flag = wx.EXPAND)
		self.SetSizer(vbox)

	def getGame(self):
		return self.gameFrame.game

	def updateView(self):
		self.outputText.SetValue(str(self.getGame()))

	def addStone(self,stone):
		self.getGame().stones.append(stone)
		self.updateView()

	def getNextColor(self):
		c = len(self.getGame().stones)
		if c == 0:
			return pb.Color.BLACK
		else:
			lastStone = self.getGame().stones[c-1]
			return otherColor(lastStone.color)

class GameFrame(wx.Frame):
	def __init__(self,parent,game):
		super(GameFrame, self).__init__(parent, size=(400, 600))
		self.game = game
		self.SetTitle("***"+self.GetParent().player.pid+"/"+str(self.game.gid)+"***")

		panel = wx.Panel(self)

		self.inputText = wx.TextCtrl(panel,style=wx.TE_PROCESS_ENTER)
		self.inputText.Bind(wx.EVT_TEXT_ENTER,self.onMyAction)

		self.playerPanes = [PlayerPane(panel,self,p,c) for p,c in zip(self.game.players,self.game.clocks)]
		self.boardPane = BoardPane(panel,self)
		
		vbox = wx.BoxSizer(wx.VERTICAL)

		for playerPane in self.playerPanes:
			vbox.Add(playerPane,proportion=0,flag=wx.EXPAND)

		vbox.Add(self.boardPane,proportion=1, flag=wx.EXPAND)
		vbox.Add(self.inputText,proportion=0,flag=wx.EXPAND)

		panel.SetSizer(vbox)

		self.Show()
		self.checkStart()

	def canPutStone(self):
		return self.game.state == pb.State.RUNNING

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
		if self.game.state != pb.State.RUNNING:
			return

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

		elif cmd == "admit":
			result = pb.Result()
			result.winner = otherColor(self.myColor())
			result.endType = pb.EndType.ADMIT

			self.gameover(result)

			msg = pb.Msg()
			msg.type = pb.MsgType.GAME_OVER
			msg.gameOver.gid = self.game.gid
			msg.gameOver.result.CopyFrom(result)

			self.send_msg(msg)

		elif cmd == "count":
			msg = pb.Msg()
			msg.type = pb.MsgType.COUNT_REQUEST
			msg.countRequest.gid = self.game.gid
			self.send_msg(msg)

	def send_msg(self,msg):
		self.GetParent().send_msg(msg)

	def countRequest(self):
		dialog = wx.MessageDialog(self, 'Are you want to count?', 'Question', 
        	wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
		result = dialog.ShowModal()

		isAgree = True if result == wx.ID_YES else False

		msg1 = pb.Msg()
		msg1.type = pb.MsgType.COUNT_REQUEST_ANSWER
		msg1.countRequestAnswer.gid = self.game.gid
		msg1.countRequestAnswer.agree = isAgree
		self.send_msg(msg1)

		if isAgree:
			self.beginCount()

	def beginCount(self):
		logging.info("---letscount invoked---")

	def gameover(self,result):
		self.stopMyClock()
		self.game.state = pb.State.ENDED
		self.game.result.CopyFrom(result)

		self.boardPane.updateView()

	def iamTimeout(self):
		myresult = pb.Result()
		myresult.winner = otherColor(self.myColor())
		myresult.endType = pb.EndType.TIMEOUT

		self.gameover(myresult)

		msg = pb.Msg()
		msg.type = pb.MsgType.GAME_OVER
		msg.gameOver.gid = self.game.gid
		msg.gameOver.result.CopyFrom(myresult)

		self.send_msg(msg)

