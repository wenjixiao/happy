import asyncio
import logging
import threading
import struct
import pb.msg_pb2 as pb
import wx

host = '127.0.0.1'
port = 5678

def add_head(bin):
	header = struct.pack('I',len(bin))
	return header+bin

class MsgProtocol(asyncio.Protocol):
	def __init__(self,uiObj):
		asyncio.Protocol.__init__(self)
		self.buf = bytes()
		self.headSize = 4

		self.uiObj = uiObj

	def send_msg(self,msg):
		self.transport.write(add_head(msg.SerializeToString()))

	def connection_made(self,transport):
		self.transport = transport
		wx.CallAfter(self.uiObj.connection_made_callback)

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
		else:
			logging.info("---exit normal---")
		wx.CallAfter(self.uiObj.connection_lost_callback,exc)

class AsyncThread(threading.Thread):
	def __init__(self,uiObj):
		threading.Thread.__init__(self)
		self.setDaemon(True)
		self.uiObj = uiObj

	async def inner_send_msg(self,msg):
		logging.debug("*coro* inner_send_msg runned")
		self.protocol.send_msg(msg)
		
	async def inner_connect(self):
		self.transport,self.protocol = await self.loop.create_connection(lambda: MsgProtocol(self.uiObj),host,port)

	async def inner_dis_connect(self):
		self.transport.close()
# ---------------------------------------------------------
	def send_msg(self,msg):
		logging.debug("in send_msg")
		coro = self.inner_send_msg(msg)
		asyncio.run_coroutine_threadsafe(coro,self.loop)

	def connect(self):
		asyncio.run_coroutine_threadsafe(self.inner_connect(),self.loop)
		
	def dis_connect(self):
		asyncio.run_coroutine_threadsafe(self.inner_dis_connect(),self.loop)
# ---------------------------------------------------------
	def run(self):
		self.loop = asyncio.new_event_loop()
		asyncio.set_event_loop(self.loop)
		self.loop.run_forever()
