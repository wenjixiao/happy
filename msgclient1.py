import wx

# Bind(event, handler, source=None, id=wx.ID_ANY, id2=wx.ID_ANY)
class WeiqiClient(wx.Frame):
    def __init__(self, parent, title):
        super(WeiqiClient, self).__init__(parent, title=title, size=(600, 400))
        self.InitUI()
        self.Centre()
        self.Show()
    def InitUI(self):
        panel = wx.Panel(self)
        vbox = wx.BoxSizer(wx.VERTICAL)

        self.input_text = wx.TextCtrl(panel,style=wx.TE_PROCESS_ENTER)
        self.output_text = wx.TextCtrl(panel,style = wx.TE_MULTILINE | wx.HSCROLL)
        self.run_button = wx.Button(panel,label = 'Run')
        
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)
        self.Bind(wx.EVT_BUTTON,self.on_run_button,source=self.run_button)
        self.Bind(wx.EVT_TEXT_ENTER,self.on_run_button,source=self.input_text)
    
        hbox = wx.BoxSizer()
        hbox.Add(self.input_text,proportion=1,flag=wx.EXPAND)
        hbox.Add(self.run_button,proportion=0,flag=wx.LEFT)
        
        vbox = wx.BoxSizer(wx.VERTICAL)
        vbox.Add(hbox,proportion=0,flag=wx.EXPAND | wx.ALL)
        vbox.Add(self.output_text,proportion=1,flag=wx.EXPAND | wx.LEFT | wx.BOTTOM | wx.RIGHT)
        
        panel.SetSizer(vbox)

    def on_run_button(self,event):
        myinput = self.input_text.GetValue()
        self.input_text.Clear()
        self.output_text.AppendText("\n")
        self.output_text.AppendText(myinput)
        
    def OnCloseWindow(self,event):
        self.Destroy()

if __name__ == '__main__':
    app = wx.App()
    WeiqiClient(None, title='*** weiqi client ***')
    app.MainLoop()