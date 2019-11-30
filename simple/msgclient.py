import asyncio
import logging
import message_pb2 as message
import msgprotocol
import threading
import wx

logging.basicConfig(level = logging.DEBUG)

class MsgClientProtocol(msgprotocol.MsgProtocol):
    def __init__(self,ui_obj):
        msgprotocol.MsgProtocol.__init__(self)
        self.ui_obj = ui_obj
        
    # override
    def connection_made(self,transport):
        msgprotocol.MsgProtocol.connection_made(self,transport)
        wx.CallAfter(self.ui_obj.connection_made_callback)

    # override
    def connection_lost(self,exc):
        msgprotocol.MsgProtocol.connection_lost(self,exc)
        wx.CallAfter(self.ui_obj.connection_lost_callback,exc)
    
    def process_msg(self,bin):
        msg = message.Msg()
        msg.ParseFromString(bin)
        wx.CallAfter(self.ui_obj.receive_msg,msg)

class AsyncThread(threading.Thread):
    def __init__(self,ui_obj):
        threading.Thread.__init__(self)
        self.ui_obj = ui_obj
    
    async def inner_send_msg(self,msg):
        logging.debug("inner_send_msg runned")
        self.transport.write(msgprotocol.packMsg(msg.SerializeToString()))
        
    def send_msg(self,msg):
        logging.debug("in send_msg")
        coro = self.inner_send_msg(msg)
        asyncio.run_coroutine_threadsafe(coro,self.loop)
        
    async def connect(self):
        self.transport,self.protocol = await self.loop.create_connection(
            lambda: MsgClientProtocol(self.ui_obj),'127.0.0.1',5678)
        
    async def dis_connect(self):
        self.transport.close()
        
    def invoke_connect(self):
        asyncio.run_coroutine_threadsafe(self.connect(),self.loop)
        
    def invoke_dis_connect(self):
        asyncio.run_coroutine_threadsafe(self.dis_connect(),self.loop)
            
    def run(self):
        self.loop = asyncio.new_event_loop()
        asyncio.set_event_loop(self.loop)
        self.loop.run_forever()

# Bind(event, handler, source=None, id=wx.ID_ANY, id2=wx.ID_ANY)
class WeiqiClient(wx.Frame):
    def __init__(self, parent, title):
        super(WeiqiClient, self).__init__(parent, title=title, size=(600, 400))
        self.init_data()
        self.init_ui()
        self.async_init()
        self.Centre()
        self.Show()
        self.async_thread.invoke_connect()

    def init_data(self):
        self.player = None
        self.players = []
        self.games = []
        
    def async_init(self):
        self.async_thread = AsyncThread(self)
        self.async_thread.start()
        
    def init_ui(self):
        panel = wx.Panel(self)
        vbox = wx.BoxSizer(wx.VERTICAL)

        self.input_text = wx.TextCtrl(panel,style=wx.TE_PROCESS_ENTER)
        self.output_text = wx.TextCtrl(panel,
            style = wx.TE_MULTILINE | wx.HSCROLL)
        self.run_button = wx.Button(panel,label = 'Run')
        
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)
        self.Bind(wx.EVT_BUTTON,self.on_run_button,source=self.run_button)
        self.Bind(wx.EVT_TEXT_ENTER,self.on_run_button,source=self.input_text)
    
        hbox = wx.BoxSizer()
        hbox.Add(self.input_text,proportion=1,flag=wx.EXPAND)
        hbox.Add(self.run_button,proportion=0,flag=wx.LEFT)
        
        vbox = wx.BoxSizer(wx.VERTICAL)
        vbox.Add(hbox,proportion=0,flag=wx.EXPAND | wx.ALL)
        vbox.Add(self.output_text,proportion=1,
            flag=wx.EXPAND | wx.LEFT | wx.BOTTOM | wx.RIGHT)
        
        panel.SetSizer(vbox)

    def OnCloseWindow(self,event):
        self.async_thread.invoke_dis_connect()
        self.Destroy()

    def on_run_button(self,event):
        self.output_text.Clear()
        myline = self.input_text.GetValue().split()
        cmd = myline[0]
        
        if cmd == "client_data":
            self.output_text.Clear()
            self.output_text.AppendText("----")
            self.output_text.AppendText("\n")
            self.output_text.AppendText(str(self.player))
            self.output_text.AppendText("\n")
            self.output_text.AppendText(str(self.players))
            self.output_text.AppendText("\n")
            self.output_text.AppendText(str(self.games))
            self.output_text.AppendText("\n")
            self.output_text.AppendText("----")

        elif cmd == "server_data":
            msg = message.Msg()
            msg.type = message.MsgType.DATA
            self.send_msg(msg)

        elif cmd == "login":
            msg = message.Msg()
            msg.type = message.MsgType.LOGIN
            msg.login.pid = myline[1]
            msg.login.passwd = myline[2]
            self.send_msg(msg)
            
        elif cmd == "logout":
            self.init_data()
            msg = message.Msg()
            msg.type = message.MsgType.LOGOUT
            self.send_msg(msg)
            
        else:
            logging.info("***no that command!***\n")
            
        self.input_text.Clear()
        
    def receive_msg(self,msg):
        if msg.type == message.MsgType.LOGIN_OK:
            self.player = msg.login_ok.player
            self.players = msg.login_ok.data.players
            self.games = msg.login_ok.data.games
            self.output_text.SetValue(str(msg))
        elif msg.type == message.MsgType.LOGIN_FAIL:
            wx.MessageBox("login failed")
        elif msg.type == message.MsgType.DATA:
            self.output_text.SetValue(str(msg))
    
    def connection_made_callback(self):
        logging.debug("----connection made----")
        
    def connection_lost_callback(self,exc):
        logging.debug("connection lost @ %s" % exc)
    
    def send_msg(self,msg):
        if not self.async_thread.transport.is_closing():
            self.async_thread.send_msg(msg)
        else:
            wx.MessageBox("connect is broken!")
            
if __name__ == '__main__':
    app = wx.App()
    WeiqiClient(None, title='*** weiqi client ***')
    app.MainLoop()