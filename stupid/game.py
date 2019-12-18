import wx
import pb.msg_pb2 as pb
import logging
from board import BoardPane
from common import otherColor


class PlayersPane(wx.Panel):
	def __init__(self,parent,gameFrame):
		super(PlayersPane,self).__init__(parent)
		self.gameFrame = gameFrame
		self.guide = {}
		# self.SetBackgroundColour(wx.BLUE)

		grid = wx.GridBagSizer(3,2)

		for col,(player,clock) in enumerate(zip(self.getGame().players,self.getGame().clocks)):
			pidSt = wx.StaticText(self,label = self.getPidStr(player))
			levelSt = wx.StaticText(self,label = self.getLevelStr(player))
			clockPane = ClockPane(self,self.gameFrame,clock)

			grid.Add(pidSt,pos=(0,col),flag=wx.ALIGN_CENTER)
			grid.Add(levelSt,pos=(1,col),flag=wx.ALIGN_CENTER)
			grid.Add(clockPane,pos=(2,col),flag=wx.EXPAND)

			self.guide[player.pid] = clockPane

		# fuck! I always forget set the grid's growable property
		grid.AddGrowableCol(0)
		grid.AddGrowableCol(1)

		self.SetSizer(grid)

	def getGame(self):
		return self.gameFrame.game

	def getPidStr(self,player):
		return str(player.pid)

	def getLevelStr(self,player):
		return str(player.level)

	def setClock(self,pid,clock):
		self.guide[pid].setClock(clock)


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

		vbox = wx.BoxSizer(wx.VERTICAL)
		vbox.Add(self.baoLiuSt,flag = wx.ALIGN_CENTER)
		vbox.Add(self.ciShuMeiCiSt,flag = wx.ALIGN_CENTER)
		vbox.Add(self.duMiaoSt,flag = wx.ALIGN_CENTER)
		self.SetSizer(vbox)

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
		self.needSendClock()

	def setClock(self,clock):
		self.clock = clock
		self.updateView()

	def needSendClock(self):
		# 如果有保留时间，一分钟发送一个通知
		# 如果没有保留时间，每十秒发送一个通知，最后十秒，一秒发送一个
		if self.clock.baoLiu > 0:
			if self.clock.baoLiu % 60 == 0:
				self.sendClockNotify()
		elif self.clock.duMiao > 10:
			if self.clock.duMiao % 10 == 0:
				self.sendClockNotify()
		else:			
			self.sendClockNotify()

	def sendClockNotify(self):
		msg = pb.Msg()
		msg.type = pb.MsgType.CLOCK_NOTIFY
		msg.clockNotify.gid = self.gameFrame.game.gid
		msg.clockNotify.pid = self.gameFrame.myPlayer().pid
		msg.clockNotify.clock.CopyFrom(self.clock)
		self.gameFrame.sendMsg(msg)

	def isRunning(self):
		return self.timer.IsRunning()

	def start(self):
		self.timer.Start(1000)

	def paused(self):
		"stop和paused是有区别的！stop要重置一下读秒的！"
		if self.timer.IsRunning():
			self.timer.Stop()

	def stop(self):
		"仿棋钟设计，每次stop，重置读秒！"
		if self.timer.IsRunning():
			self.timer.Stop()
			self.resetDuMiao()


class GameFrame(wx.Frame):

	COUNTING = 60*2

	def __init__(self,parent,game):
		super(GameFrame, self).__init__(parent, size=(400, 600))
		self.game = game
		self.deadStones = []
		self.count = self.COUNTING
		self.SetTitle("***"+self.GetParent().player.pid+"/"+str(self.game.gid)+"***")

		panel = wx.Panel(self)

		self.inputText = wx.TextCtrl(panel,style=wx.TE_PROCESS_ENTER)
		self.inputText.Bind(wx.EVT_TEXT_ENTER,self.onMyAction)

		self.playersPane = PlayersPane(panel,self)
		self.boardPane = BoardPane(panel,self)
		
		vbox = wx.BoxSizer(wx.VERTICAL)
		vbox.Add(self.playersPane,proportion=0,flag=wx.EXPAND)
		vbox.Add(self.boardPane,proportion=1, flag=wx.EXPAND)
		vbox.Add(self.inputText,proportion=0,flag=wx.EXPAND)
		panel.SetSizer(vbox)

		self.Show()
		self.countTimer = wx.Timer(self)
		self.Bind(wx.EVT_TIMER, self.onCount, self.countTimer)

		self.checkStart()
# ---------------------------------------------------------
	def checkStart(self):
		"所谓start，要么启动时钟，要么启动倒数！"
		if self.game.state == pb.State.RUNNING and self.isMyTurn():
			self.myClockPane().start()
		# 断线重连后，会得到一个game，但是此时未必可以开始，要看game.state是不是running。
		# 如果是broken，那么说明还没有到可以运行的条件。
		# 此时，需要启动倒数。
		# 如果两个人都断线了，就算了；一个人断线，就不能让另一个人等太久！
		if self.game.state == pb.State.BROKEN:
			self.startCountDown()

	def myClockPane(self):
		return self.playersPane.guide[self.myPlayer().pid]

	def myColor(self):
		return pb.Color.BLACK if self.game.players[self.game.blackIndex] == self.myPlayer() else pb.Color.WHITE

	def myPlayer(self):
		return self.GetParent().player

	def isMyTurn(self):
		return self.boardPane.getNextColor() == self.myColor()

	def sendMsg(self,msg):
		self.GetParent().sendMsg(msg)

	def onMyAction(self,event):
		myline = self.inputText.GetValue().split()
		cmd = myline[0]
		if cmd == "stone":
			if self.myClockPane().isRunning():
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

			self.sendMsg(msg)

		elif cmd == "count":
			self.doPaused()

			msg = pb.Msg()
			msg.type = pb.MsgType.COUNT_REQUEST
			msg.countRequest.gid = self.game.gid
			self.sendMsg(msg)

		elif cmd == "myturn":
			logging.info(self.isMyTurn())

# ---------------------------------------------------------			
	def startCountDown(self):
		if not self.countTimer.IsRunning():
			self.countTimer.Start(1000)

	def stopCountDown(self):
		if self.countTimer.IsRunning():
			self.countTimer.Stop()
# ---------------------------------------------------------
	def clockNotify(self,pid,clock):
		"收到对面的时间更新"
		self.playersPane.setClock(pid,clock)

	def putStone(self,stone):
		"下一个子，到棋盘上，这个子可能是你下的，也可能是对面下的"
		if self.game.state != pb.State.RUNNING:
			return
		if self.isMyTurn():
			self.boardPane.addStone(stone)
			self.myClockPane().stop()

			msg = pb.Msg()
			msg.type = pb.MsgType.HAND
			msg.hand.gid = self.game.gid
			msg.hand.stone.CopyFrom(stone)
			self.sendMsg(msg)
		else:
			self.boardPane.addStone(stone)
			self.checkStart()
# ---------------------------------------------------------
	def countRequest(self):
		"对面发出的数子请求"
		dialog = wx.MessageDialog(self, 'Are you want to count?', 'Question', 
        	wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
		result = dialog.ShowModal()

		isAgree = True if result == wx.ID_YES else False

		msg1 = pb.Msg()
		msg1.type = pb.MsgType.COUNT_REQUEST_ANSWER
		msg1.countRequestAnswer.gid = self.game.gid
		msg1.countRequestAnswer.agree = isAgree
		self.sendMsg(msg1)

		if isAgree:
			self.doPaused()
			self.selectDeadStones()

	def selectDeadStones(self):
		"我要选死子了"
		logging.info("selectDeadStones invoked")

	def deadStones(self,addOrRemove,stones):
		"对面选了死子，或者选错取消某些死子"
		if addOrRemove:
			self.deadStones.extend(stones)
		else:
			for stone in stones:
				self.deadStones.remove(stone)

	def confirmDead(self):
		"死的都选完了，可以结算了"
		gameResult = pb.Result()
		gameResult.endType = pb.EndType.COUNT
		gameResult.winner,gameResult.mount = self.computePoints()
		# 弹出对话框，问一下是否同意结果。发消息给对面，以确定下一步。

		words = "color:"+gameResult.winner+",mount:"+gameResult.mount
		dialog = wx.MessageDialog(self, words, 'Agree?', wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
		result = dialog.ShowModal()

		msg = pb.Msg()
		msg.type = pb.MsgType.COUNT_RESULT
		msg.countResult.gid = self.game.gid
		msg.countResult.result.CopyFrom(gameResult)
		msg.countResult.agree = True if result == wx.ID_YES else False

		self.sendMsg(msg)

	def computePoints(self):
		return (pb.Color.BLACK,1.5)

# ---------------------------------------------------------
	def gameover(self,result):
		"以这个result结束game"
		self.myClockPane().stop()
		self.game.state = pb.State.ENDED
		self.game.result.CopyFrom(result)

		self.boardPane.updateView()

	def onCount(self,event):
		"我等两分钟，对面超时，我就胜"
		self.count -= 1

		if self.count > 10:
			if self.count % 10 == 0:
				logging.info("countdown 10:{}".format(self.count))
		elif self.count >= 0 and self.count <= 10:
			logging.info("countdown 1:{}".format(self.count))
		else:
			self.countOverflow()

	def countOverflow(self):
		"对面超时了"
		self.stopCountDown()

		myresult = pb.Result()
		myresult.winner = self.myColor()
		myresult.endType = pb.EndType.LINEBROKEN

		self.gameover(myresult)

		msg = pb.Msg()
		msg.type = pb.MsgType.GAME_OVER
		msg.gameOver.gid = self.game.gid
		msg.gameOver.result.CopyFrom(myresult)

		self.sendMsg(msg)
# ---------------------------------------------------------
	def lineBroken(self):
		"对面断线了。linebroken是要倒计时的！"
		self.game.state = pb.State.BROKEN
		if self.isMyTurn():
			self.myClockPane().paused()
		self.startCountDown()

	def comeback(self):
		"收到comeback,意味着万事俱备，可以开始了"
		if self.game.state == pb.State.BROKEN:
			self.stopCountDown()
			self.game.state = pb.State.RUNNING
			self.checkStart()
# ---------------------------------------------------------
	def doPaused(self):
		"暂停当前game"
		self.game.state = pb.State.PAUSED
		if self.isMyTurn():
			self.myClockPane().paused()

	def doContinue(self):
		"断线，申请数目之后，都需要再开始一下。申请数目会使game暂停。"
		if self.game.state == pb.State.PAUSED:
			self.game.state = pb.State.RUNNING
			self.checkStart()
# ---------------------------------------------------------
	def iamTimeout(self):
		"我超时了，输了，棋局结束了"
		myresult = pb.Result()
		myresult.winner = otherColor(self.myColor())
		myresult.endType = pb.EndType.TIMEOUT

		self.gameover(myresult)

		msg = pb.Msg()
		msg.type = pb.MsgType.GAME_OVER
		msg.gameOver.gid = self.game.gid
		msg.gameOver.result.CopyFrom(myresult)

		self.sendMsg(msg)

# ---------------------------------------------------------