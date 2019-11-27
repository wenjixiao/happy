import wx
import threading
import asyncio
import msgprotocol
import message_pb2 as message

class MsgClientProtocol(msgprotocol.MsgProtocol):
    def __init__(self,on_con_lost,ui_obj):
        msgprotocol.MsgProtocol.__init__(self)
        self.on_con_lost = on_con_lost
        self.ui_obj = ui_obj
    # override
    def connection_made(self,transport):
        msgprotocol.MsgProtocol.connection_made(self,transport)
        wx.CallAfter(self.ui_obj.connection_made)
    # override
    def connection_lost(self,exc):
        msgprotocol.MsgProtocol.connection_lost(self,exc)
        self.on_con_lost.set_result(True)
        wx.CallAfter(self.ui_obj.connection_lost,exc)
    
    def process_msg(self,bin):
        msg = message.Msg()
        msg.ParseFromString(bin)
        print(msg)
        wx.CallAfter(self.ui_obj.receive_msg,msg)

class AsyncThread(threading.Thread):
    def __init__(self,ui_obj):
        threading.Thread.__init__(self)
        self.ui_obj = ui_obj
    
    async def inner_send_msg(self,msg):
        print("inner_send_msg runned")
        self.transport.write(msgprotocol.packMsg(msg.SerializeToString()))
        
    def send_msg(self,msg):
        print("in send_msg")
        coro = self.inner_send_msg(msg)
        asyncio.run_coroutine_threadsafe(coro,self.loop)
        
    async def connect(self):
        print("async thread begin ...")
        on_con_lost = self.loop.create_future()
        self.transport,self.protocol = await self.loop.create_connection(
            lambda: MsgClientProtocol(on_con_lost,self.ui_obj),'127.0.0.1',5678)
        try:
            await on_con_lost
        finally:
            transport.close()
        print("async run ended")
            
    def run(self):
        self.loop = asyncio.new_event_loop()
        asyncio.set_event_loop(self.loop)
        self.loop.create_task(self.connect())
        self.loop.run_forever()

# Bind(event, handler, source=None, id=wx.ID_ANY, id2=wx.ID_ANY)
class WeiqiClient(wx.Frame):
    def __init__(self, parent, title):
        super(WeiqiClient, self).__init__(parent, title=title, size=(600, 400))
        self.InitUI()
        self.async_init()
        self.Centre()
        self.Show()
        
    def async_init(self):
        self.async_thread = AsyncThread(self)
        self.async_thread.start()
        
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
        myline = self.input_text.GetValue().split()
        cmd = myline[0]
        
        if cmd == "logout":
            msg = message.Msg()
            msg.type = message.MsgType.LOGOUT
            self.async_thread.send_msg(msg)
            transport.close()
            
        elif cmd == "login":
            msg = message.Msg()
            msg.type = message.MsgType.LOGIN
            msg.login.pid = myline[1]
            msg.login.passwd = myline[2]
            
            self.async_thread.send_msg(msg)
            # transport.write(msgprotocol.packMsg(msg.SerializeToString()))
            
        elif cmd == "info":
            # print(protocol.transport)
            msg = message.Msg()
            msg.type = message.MsgType.INFO
            # print("transport is closed? ",transport.is_closing())
            self.async_thread.send_msg(msg)
            # transport.write(msgprotocol.packMsg(msg.SerializeToString()))
            
        else:
            print("***no that command!***\n")
        
    def receive_msg(self,msg):
        print("received msg: %s" % msg)
        self.input_text.Clear()
        self.output_text.AppendText("\n")
        self.output_text.AppendText(str(msg))
    
    def connection_made(self):
        print("connection made")
        
    def connection_lost(self,exc):
        print("connection lost @ %s" % exc)
    
    def OnCloseWindow(self,event):
        self.Destroy()

if __name__ == '__main__':
    app = wx.App()
    WeiqiClient(None, title='*** weiqi client ***')
    app.MainLoop()