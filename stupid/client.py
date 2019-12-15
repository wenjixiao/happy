import logging
import wx
import pb.msg_pb2 as pb
from proto_dialog import ProtoDialog
from game import GameFrame
from basic import AsyncThread

logging.basicConfig(level = logging.DEBUG)


# Bind(event, handler, source=None, id=wx.ID_ANY, id2=wx.ID_ANY)
class BasicClient(wx.Frame):
	def __init__(self, parent, title):
		super(BasicClient, self).__init__(parent, title=title, size=(600, 400))
		self.init()
		self.init_ui()
		self.init_async()
		self.Centre()
		self.Show()
		self.async_thread.connect()

	def init(self):
		self.player = None
		self.gameFrames = []

	def init_async(self):
		self.async_thread = AsyncThread(self)
		self.async_thread.start()
		
	def init_ui(self):
		panel = wx.Panel(self)
		vbox = wx.BoxSizer(wx.VERTICAL)

		self.input_text = wx.TextCtrl(panel,style=wx.TE_PROCESS_ENTER)
		self.output_text = wx.TextCtrl(panel, style = wx.TE_MULTILINE | wx.HSCROLL)
		self.run_button = wx.Button(panel,label = 'Run')
		
		self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)
		self.Bind(wx.EVT_BUTTON,self.on_run_button,source=self.run_button)
		self.Bind(wx.EVT_TEXT_ENTER,self.on_run_button,source=self.input_text)
	
		hbox = wx.BoxSizer()
		hbox.Add(self.input_text,proportion=1,flag=wx.EXPAND)
		hbox.Add(self.run_button,proportion=0,flag=wx.LEFT)
		
		vbox = wx.BoxSizer(wx.VERTICAL)
		vbox.Add(hbox,proportion=0,flag=wx.EXPAND | wx.ALL)
		vbox.Add(self.output_text,proportion=1, flag=wx.EXPAND | wx.LEFT | wx.BOTTOM | wx.RIGHT)
		
		panel.SetSizer(vbox)

	def OnCloseWindow(self,event):
		self.async_thread.dis_connect()
		self.Destroy()

	def on_run_button(self,event):
		self.output_text.Clear()

		myline = self.input_text.GetValue().split()
		cmd = myline[0]
		
		if cmd == "login":
			msg = pb.Msg()
			msg.type = pb.MsgType.LOGIN
			msg.login.pid = myline[1]
			msg.login.passwd = myline[2]
			self.send_msg(msg)

		elif cmd == "invite":
			dialog = ProtoDialog(self,None)
			result = dialog.ShowModal()
			if result == ProtoDialog.OK:
				msg = pb.Msg()
				msg.type = pb.MsgType.INVITE
				msg.invite.pid = myline[1]
				msg.invite.proto.CopyFrom(dialog.getProto())
				self.send_msg(msg)
			dialog.Destroy()

		elif cmd == "logout":
			msg = pb.Msg()
			msg.type = pb.MsgType.LOGOUT
			self.send_msg(msg)

		elif cmd == "data":
			msg = pb.Msg()
			msg.type = pb.MsgType.DATA
			self.send_msg(msg)

		self.input_text.Clear()


	def msg_received(self,msg):
		logging.info("=========received==========")
		logging.info(msg)

		if msg.type == pb.MsgType.LOGIN_OK:
			self.player = msg.loginOk.player
			self.SetTitle("----{}----".format(self.player.pid))

		elif msg.type == pb.MsgType.INVITE:
			# show dialog,get yes or no,or change the proto to resend invite msg again
			# if yes or no,send a invite_answer msg
			dialog = ProtoDialog(self,msg.invite.proto)
			result = dialog.ShowModal()
			if result == ProtoDialog.CHANGE:
				# resend a invite to pid,use the changed proto
				msg.invite.proto.CopyFrom(dialog.getProto())
				self.send_msg(msg)
			else:
				# not change,agree or refuse
				msg1 = pb.Msg()
				msg1.type = pb.MsgType.INVITE_ANSWER
				msg1.inviteAnswer.agree = True if result == ProtoDialog.OK else False
				msg1.inviteAnswer.pid = msg.invite.pid
				msg1.inviteAnswer.proto.CopyFrom(msg.invite.proto)
				self.send_msg(msg1)
			dialog.Destroy()

		elif msg.type == pb.MsgType.INVITE_ANSWER:
			logging.info("invite answer: isagree = {}".format(msg.inviteAnswer.isAgree))

		elif msg.type == pb.MsgType.GAME:
			# create a window and set the game in
			self.gameFrames.append(GameFrame(self,msg.game))

		elif msg.type == pb.MsgType.HAND:
			self.withGameFrame(msg.hand.gid,lambda gf: gf.putStone(msg.hand.stone))

		elif msg.type == pb.MsgType.GAME_OVER:
			if msg.gameOver.result.endType == pb.EndType.COUNT:
				# we get the count result,we also need to ask,if he agree the count result
				words = 'Are you agree the count result of gameover?'
				dialog = wx.MessageDialog(self, words, 'Question', wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
				result = dialog.ShowModal()

				msg1 = pb.Msg()
				msg1.type = pb.MsgType.COUNT_RESULT_ANSWER
				msg1.countResultAnswer.gid = msg.gameOver.gid
				msg1.countResultAnswer.result.CopyFrom(msg.gameOver.result)
				msg1.countResultAnswer.agree = True if result == wx.ID_YES else False
				self.send_msg(msg1)
			else:
				# timeout or admit
				self.withGameFrame(msg.gameOver.gid,lambda gf: gf.gameover(msg.gameOver.result))

		elif msg.type == pb.MsgType.COUNT_REQUEST:
			self.withGameFrame(msg.countRequest.gid,lambda gf: gf.countRequest())

		elif msg.type == pb.MsgType.COUNT_REQUEST_ANSWER:
			# the next thing is select dead stones,because the other player has agree
			cra = msg.countRequestAnswer
			def myfun(gf):
				if cra.agree:
					gf.selectDeadStones()
				else:
					gf.doContinue()
			self.withGameFrame(cra.gid,myfun)
		
		elif msg.type == pb.MsgType.DO_CONTINUE:
			self.withGameFrame(msg.doContinue.gid,lambda gf: gf.doContinue())

		elif msg.type == pb.MsgType.CLOCK_NOTIFY:
			cn = msg.clockNotify
			self.withGameFrame(cn.gid,lambda gf: gf.clockNotify(cn.pid,cn.clock))

		elif msg.type == pb.MsgType.LINE_BROKEN:
			self.withGameFrame(msg.lineBroken.gid,lambda gf: gf.lineBroken())
			
		elif msg.type == pb.MsgType.STATE_CHANGED:
			self.withGameFrame(msg.stateChanged.gid,lambda gf: gf.stateChanged(msg.stateChanged.State))

	def withGameFrame(self,gid,myfun):
		for gameFrame in self.gameFrames:
			if gameFrame.game.gid == gid:
				myfun(gameFrame)

	def send_msg(self,msg):
		if not self.async_thread.transport.is_closing():
			self.async_thread.send_msg(msg)
		else:
			wx.MessageBox("connect is broken!")

	def connection_made_callback(self):
		logging.debug("connection made")
		
	def connection_lost_callback(self,exc):
		logging.debug("connection lost: %s\n" % exc)


if __name__ == '__main__':
	app = wx.App()
	BasicClient(None, title='*** basic client ***')
	app.MainLoop()