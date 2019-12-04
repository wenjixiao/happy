import wx
import pb.msg_pb2 as pb

whoFirsts = [pb.WhoFirst.YOU,pb.WhoFirst.ME,pb.WhoFirst.RANDOM]

class ProtoDialog(wx.Dialog):
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
        pass
    def __init__(self,parent):
        wx.Dialog.__init__(self, parent, -1, '***set proto***',size=(300,300))

        grid = wx.GridBagSizer(0,0)

        panel = wx.Panel(self)

        protoSt = wx.StaticText(panel,label="*proto*")

        rangZiSt = wx.StaticText(panel,label="rangZi")
        self.rangZiTc = wx.TextCtrl(panel,value='0')

        tieMuSt = wx.StaticText(panel,label="tieMu")
        self.tieMuTc = wx.TextCtrl(panel,value='6.5')

        whoFirstSt = wx.StaticText(panel,label="whoFirst")
        self.whoFirstTc = wx.Choice(panel,choices=['YOU','ME','RANDOM'])
        self.whoFirstTc.SetSelection(2)

        clockSt = wx.StaticText(panel,label="*clock*")

        baoLiuSt = wx.StaticText(panel,label="baoLiu")
        self.baoLiuTc = wx.TextCtrl(panel,value='1200')

        duMiaoSt = wx.StaticText(panel,label="duMiao")
        self.duMiaoTc = wx.TextCtrl(panel,value='30')

        ciShuSt = wx.StaticText(panel,label="ciShu")
        self.ciShuTc = wx.TextCtrl(panel,value='3')

        meiCiSt = wx.StaticText(panel,label="meiCi")
        self.meiCiTc = wx.TextCtrl(panel,value='60')

        okButton = wx.Button(panel, wx.ID_OK, "OK")
        okButton.SetDefault()
        cancelButton = wx.Button(panel, wx.ID_CANCEL, "Cancel")

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
        grid.Add(okButton,pos=(9,0),flag=wx.EXPAND)
        grid.Add(cancelButton,pos=(9,1),flag=wx.EXPAND)

        panel.SetSizer(grid)

class TestFrame(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self,None,-1,'Study dialog of wxpython')
        panel=wx.Panel(self)
        button=wx.Button(panel,-1,"Open dialog!")
        button.Bind(wx.EVT_BUTTON,self.OnOpenDialog)
    
    def OnOpenDialog(self,evt):
        dialog = ProtoDialog(self)
        result = dialog.ShowModal()

        if result == wx.ID_OK:
            print("Ok")
            print(dialog.getProto())
        else:
            print("Cancel")
            
        dialog.Destroy()
        
if __name__=='__main__':
    app = wx.App()
    frame=TestFrame()
    frame.Show()

    app.MainLoop()

