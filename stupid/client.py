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
			
		else:
			logging.info("***no that command!***\n")
			
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
				msg1.inviteAnswer.isAgree = True if result == ProtoDialog.OK else False
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
			for gameFrame in self.gameFrames:
				if gameFrame.game.gid == msg.hand.gid:
					gameFrame.putStone(msg.hand.stone)

		elif msg.type == pb.MsgType.END_GAME:
			# request to count
			if msg.endGame.result.entType == pb.EndType.NORMAL:
				# show yes or no dialog,if yes,the game's state trans to counting
				pass
			else:
				# timeout or admit
				for gameFrame in self.gameFrames:
					if gameFrame.game.gid == msg.endGame.gid:
						gameFrame.gameover(msg.endGame.result)
				
		else:
			pass
		self.output_text.SetValue(str(msg))
	
	def send_msg(self,msg):
		if not self.async_thread.transport.is_closing():
			self.async_thread.send_msg(msg)
		else:
			wx.MessageBox("connect is broken!")

	def connection_made_callback(self):
		logging.debug("----connection made----")
		
	def connection_lost_callback(self,exc):
		logging.debug("connection lost @ %s" % exc)


if __name__ == '__main__':
	app = wx.App()
	BasicClient(None, title='*** basic client ***')
	app.MainLoop()