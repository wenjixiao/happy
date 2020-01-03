from queue import Queue
from msg_util import *
from threading import Thread,Event
import time
import asyncio

msgs = Queue()

class MsgClientProtocol(MsgProtocol):
	def __init__(self,on_con_lost):
		super(MsgClientProtocol,self).__init__()
		self.on_con_lost = on_con_lost
		self.player = None

	def process_msg(self,msg):
		print("----------------------------")
		print(msg)
		msgs.put(msg)

	def connection_lost(self,exc):
		self.on_con_lost.set_result(True)

def checkMsg(protocol):
	while not msgs.empty():
		msg = msgs.get()
		processMsg(protocol,msg)

def processMsg(protocol,msg):
	mt = msg['type']

	if mt == 'loginOk':
		p = msg['player']
		protocol.player = Player(p['pid'],age=p['age'])

	elif mt == 'say':
		print("{}: {}".format(msg['fromPid'],msg['content']))

def getPrompt(protocol):
	return protocol.player.pid+'>' if protocol.player else '>'

def cmdLoop(protocol):
	while True:
		line = input(getPrompt(protocol))

		if line == '':
			checkMsg(protocol)
			continue

		cmd,*params = line.split()

		if cmd == "exit":
			protocol.transport.close()
			break

		elif cmd == "login":
			pid,passwd = params
			msg = {'type':'login','pid':pid,'passwd':passwd}
			protocol.send_msg(msg)

		elif cmd == "logout":
			msg = {'type':'logout'}
			protocol.send_msg(msg)

		elif cmd == "say":
			print(params)
			toPid,content = params
			msg = {'type':'say','toPid':toPid,'content':content}
			protocol.send_msg(msg)

async def main():
	msgs = []

	loop = asyncio.get_running_loop()

	on_con_lost = loop.create_future()

	transport,protocol = await loop.create_connection(lambda: MsgClientProtocol(on_con_lost),'127.0.0.1',20000)

	cmdLoop(protocol)
	# protocol.send_msg({'type':'login','pid':'wen','passwd':'123'})
	# checkMsg(protocol)
	try:
		await on_con_lost
	finally:
		transport.close()

if __name__ == '__main__':
	asyncio.run(main())
	print("---------main exit------------")



# class AsyncThread(Thread):
# 	def __init__(self):
# 		super(AsyncThread,self).__init__()
# 		self.started = Event()
# 		self.setDaemon(True)

# 	async def inner_connect(self):
# 		print("9999999999999999")
# 		self.transport,self.protocol = await self.loop.create_connection(lambda: MsgClientProtocol(),'127.0.0.1',20000)

# 	async def inner_send_msg(self,msg):
# 		self.transport.write(addHeader(encodeMsg(msg)))

# 	async def inner_disconnect(self):
# 		self.transport.close()

# 	def connect(self):
# 		asyncio.run_coroutine_threadsafe(self.inner_connect(),self.loop)

# 	def send_msg(self,msg):
# 		asyncio.run_coroutine_threadsafe(self.inner_send_msg(msg),self.loop)

# 	def disconnect(self):
# 		asyncio.run_coroutine_threadsafe(self.inner_disconnect(),self.loop)

# 	def stop(self):
# 		self.loop.call_soon_threadsafe(self.loop.stop)

# 	def run(self):
# 		self.loop = asyncio.new_event_loop()
# 		asyncio.set_event_loop(self.loop)
# 		self.started.set()
# 		self.loop.run_forever()


# class Client:
# 	def __init__(self):
# 		self.msgs = Queue()
# 		self.player = None
# 		self.at = AsyncThread()
# 		self.at.start()

# 	def getPrompt(self):
# 		return self.player.pid+'>' if self.player else '>'

# 	def cmdLoop(self):
# # -----------------------------------------------------------------------------
# 		while True:
# 			line = input(self.getPrompt())

# 			if line == '':
# 				self.checkMsgs()
# 				continue

# 			cmd,*params = line.split()

# 			if cmd == "exit":
# 				self.at.disconnect()
# 				break

# 			elif cmd == "conn":
# 				self.at.connect()

# 			elif cmd == "login":
# 				pid,passwd = params
# 				msg = {'type':'login','pid':pid,'passwd':passwd}
# 				self.at.send_msg(msg)

# 			elif cmd == "logout":
# 				msg = {'type':'logout'}
# 				self.at.send_msg(msg)

# 			elif cmd == "say":
# 				toPid,content = params
# 				msg = {'type':'say','toPid':toPid,'content':content}
# 				self.at.send_msg(msg)

# 			time.sleep(1)
# 			self.checkMsgs()
# # -----------------------------------------------------------------------------						

# 	def checkMsgs(self):
# 		print("----check msgs----")
# 		while not self.msgs.empty():
# 			self.processMsg(self.msgs.get())

# 	def processMsg(self,msg):
# 		mt = msg['type']

# 		if mt == 'loginOk':
# 			p = msg['player']
# 			self.player = Player(p['pid'],age=p['age'])

# 		elif mt == 'say':
# 			print("{}: {}".format(msg['fromPid'],msg['content']))

