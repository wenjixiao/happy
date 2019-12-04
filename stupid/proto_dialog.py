import wx
import pb.msg_pb2 as pb

whoFirsts = [pb.WhoFirst.YOU,pb.WhoFirst.ME,pb.WhoFirst.RANDOM]

class ProtoDialog(wx.Dialog):
	OK = 1
	CHANGE = 2
	CANCEL = 3
	@staticmethod
	def defaultProto():
		proto = pb.Proto()
		proto.rangZi = 0
		proto.tieMu = 6.5
		proto.whoFirst = pb.WhoFirst.RANDOM
		proto.clock.baoLiu = 20*60
		proto.clock.duMiao = 30
		proto.clock.ciShu = 3
		proto.clock.meiCi = 60
		return proto

	def getProto(self):
		proto = pb.Proto()
		proto.rangZi = int(self.rangZiTc.GetValue())
		proto.tieMu = float(self.tieMuTc.GetValue())
		proto.whoFirst = whoFirsts[self.whoFirstTc.GetSelection()]
		proto.clock.baoLiu = int(self.baoLiuTc.GetValue())
		proto.clock.duMiao = int(self.duMiaoTc.GetValue())
		proto.clock.ciShu = int(self.ciShuTc.GetValue())
		proto.clock.meiCi = int(self.meiCiTc.GetValue())
		return proto

	def setProto(self,proto):
		self.rangZiTc.SetValue(str(proto.rangZi))
		self.tieMuTc.SetValue(str(proto.tieMu))
		self.whoFirstTc.SetSelection(whoFirsts.index(proto.whoFirst))
		self.baoLiuTc.SetValue(str(proto.clock.baoLiu))
		self.duMiaoTc.SetValue(str(proto.clock.duMiao))
		self.ciShuTc.SetValue(str(proto.clock.ciShu))
		self.meiCiTc.SetValue(str(proto.clock.meiCi))

	def __init__(self,parent,proto):
		wx.Dialog.__init__(self, parent, -1,size=(300,300))

		if proto == None:
			self.SetTitle("***set invite proto***")
		else:
			self.SetTitle("***received invite proto***")

		grid = wx.GridBagSizer(0,0)

		panel = wx.Panel(self)

		protoSt = wx.StaticText(panel,label="*proto*")

		rangZiSt = wx.StaticText(panel,label="rangZi")
		self.rangZiTc = wx.TextCtrl(panel)

		tieMuSt = wx.StaticText(panel,label="tieMu")
		self.tieMuTc = wx.TextCtrl(panel)

		whoFirstSt = wx.StaticText(panel,label="whoFirst")
		self.whoFirstTc = wx.Choice(panel,choices=['YOU','ME','RANDOM'])
		# self.whoFirstTc.SetSelection(2)

		clockSt = wx.StaticText(panel,label="*clock*")

		baoLiuSt = wx.StaticText(panel,label="baoLiu")
		self.baoLiuTc = wx.TextCtrl(panel)

		duMiaoSt = wx.StaticText(panel,label="duMiao")
		self.duMiaoTc = wx.TextCtrl(panel)

		ciShuSt = wx.StaticText(panel,label="ciShu")
		self.ciShuTc = wx.TextCtrl(panel)

		meiCiSt = wx.StaticText(panel,label="meiCi")
		self.meiCiTc = wx.TextCtrl(panel)

		grid.Add(protoSt,pos=(0,0),span=(1,2),flag=wx.ALIGN_CENTER)
		grid.Add(rangZiSt,pos=(1,0),flag=wx.EXPAND)
		grid.Add(self.rangZiTc,pos=(1,1),flag=wx.EXPAND)
		grid.Add(tieMuSt,pos=(2,0),flag=wx.EXPAND)
		grid.Add(self.tieMuTc,pos=(2,1),flag=wx.EXPAND)
		grid.Add(whoFirstSt,pos=(3,0),flag=wx.EXPAND)
		grid.Add(self.whoFirstTc,pos=(3,1),flag=wx.EXPAND)
		grid.Add(clockSt,pos=(4,0),span=(1,2),flag=wx.ALIGN_CENTER)
		grid.Add(baoLiuSt,pos=(5,0),flag=wx.EXPAND)
		grid.Add(self.baoLiuTc,pos=(5,1),flag=wx.EXPAND)
		grid.Add(duMiaoSt,pos=(6,0),flag=wx.EXPAND)
		grid.Add(self.duMiaoTc,pos=(6,1),flag=wx.EXPAND)
		grid.Add(ciShuSt,pos=(7,0),flag=wx.EXPAND)
		grid.Add(self.ciShuTc,pos=(7,1),flag=wx.EXPAND)
		grid.Add(meiCiSt,pos=(8,0),flag=wx.EXPAND)
		grid.Add(self.meiCiTc,pos=(8,1),flag=wx.EXPAND)

				
		self.okButton = wx.Button(panel, -1, "OK")
		self.okButton.SetDefault()
		if proto != None:
			self.changeButton = wx.Button(panel,-1,"Change")
		self.cancelButton = wx.Button(panel, -1, "Cancel")

		hbox = wx.BoxSizer()
		hbox.Add(self.okButton)
		if proto != None:
			hbox.Add(self.changeButton)
		hbox.Add(self.cancelButton)

		grid.Add(hbox,pos=(9,0),span=(1,2),flag=wx.EXPAND)

		myproto = self.defaultProto() if proto == None else proto

		self.setProto(myproto)

		panel.SetSizer(grid)

		self.Bind(wx.EVT_BUTTON,self.onClick)

	def onClick(self,event):
		objId = event.GetEventObject().GetId()
		if objId == self.okButton.GetId():
			result = ProtoDialog.OK
		elif objId == self.cancelButton.GetId():
			result = ProtoDialog.CANCEL
		else:
			result = ProtoDialog.CHANGE
		self.EndModal(result)

class TestFrame(wx.Frame):
	def __init__(self):
		wx.Frame.__init__(self,None,-1,'Study dialog of wxpython')
		panel=wx.Panel(self)
		button=wx.Button(panel,-1,"Open dialog!")
		button.Bind(wx.EVT_BUTTON,self.OnOpenDialog)
	
	def OnOpenDialog(self,evt):
		dialog = ProtoDialog(self,None)
		# dialog = ProtoDialog(self,ProtoDialog.defaultProto())
		result = dialog.ShowModal()

		print("--------------------")
		print(result)
			
		dialog.Destroy()
		
if __name__=='__main__':
	app = wx.App()
	frame=TestFrame()
	frame.Show()

	app.MainLoop()

