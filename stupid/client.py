import asyncio
import logging
import threading
import wx
import pb.msg_pb2 as pb
import struct

logging.basicConfig(level = logging.DEBUG)

host = '127.0.0.1'
port = 5678

class MsgProtocol(asyncio.Protocol):
    def __init__(self,uiObj):
        asyncio.Protocol.__init__(self)
        self.bufSize = 1024
        self.buf = bytes()
        self.headSize = 4

        self.uiObj = uiObj

    def connection_made(self,transport):
        self.transport = transport
        wx.CallAfter(self.uiObj.connection_made_callback)

    def send_msg(self,msg):
        self.transport.write(self.add_head(msg.SerializeToString()))

    @staticmethod
    def add_head(bin):
        header = struct.pack('I',len(bin))
        return header+bin

    def data_received(self,data):
        self.buf += data
        if len(self.buf) < self.headSize:
            logging.debug("dataSize < headSize!")
            return
        bodySize, = struct.unpack('<I',self.buf[:self.headSize])
        logging.debug("bodySize={}".format(bodySize))
        if len(self.buf) < self.headSize+bodySize:
            logging.debug("message data not enougth!")
            return
        bin = self.buf[self.headSize:self.headSize+bodySize]

        msg = pb.Msg()
        msg.ParseFromString(bin)
        wx.CallAfter(self.uiObj.msg_received,msg)

        self.buf = self.buf[self.headSize+bodySize:]

    def connection_lost(self,exc):
        logging.debug("connection lost exception:{}".format(exc))
        if exc is not None:
            logging.info("---exit EXCEPTION---")
            self.buf = bytes()
        else:
            logging.info("---exit normal---")
        wx.CallAfter(self.uiObj.connection_lost_callback,exc)

class AsyncThread(threading.Thread):
    def __init__(self,uiObj):
        threading.Thread.__init__(self)
        self.uiObj = uiObj

    async def inner_send_msg(self,msg):
        logging.debug("*coro* inner_send_msg runned")
        self.protocol.send_msg(msg)
        
    def send_msg(self,msg):
        logging.debug("in send_msg")
        coro = self.inner_send_msg(msg)
        asyncio.run_coroutine_threadsafe(coro,self.loop)
        
    async def inner_connect(self):
        self.transport,self.protocol = await self.loop.create_connection(lambda: MsgProtocol(self.uiObj),host,port)

    async def inner_dis_connect(self):
        self.transport.close()
        
    def connect(self):
        asyncio.run_coroutine_threadsafe(self.inner_connect(),self.loop)
        
    def dis_connect(self):
        asyncio.run_coroutine_threadsafe(self.inner_dis_connect(),self.loop)

    def run(self):
        self.loop = asyncio.new_event_loop()
        asyncio.set_event_loop(self.loop)
        self.loop.run_forever()

# Bind(event, handler, source=None, id=wx.ID_ANY, id2=wx.ID_ANY)
class BasicClient(wx.Frame):
    def __init__(self, parent, title):
        super(BasicClient, self).__init__(parent, title=title, size=(600, 400))
        self.init_ui()
        self.init_async()
        self.Centre()
        self.Show()
        
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
        self.Destroy()

    def on_run_button(self,event):
        self.output_text.Clear()

        myline = self.input_text.GetValue().split()
        cmd = myline[0]
        
        if cmd == "connect":
            self.async_thread.connect()

        elif cmd == "dis_connect":
            self.async_thread.dis_connect()

        elif cmd == "login":
            msg = pb.Msg()
            msg.type = pb.MsgType.LOGIN
            msg.login.pid = myline[1]
            msg.login.passwd = myline[2]
            self.send_msg(msg)

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