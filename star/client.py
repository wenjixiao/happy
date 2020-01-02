from socket import *
from msg_util import *
from queue import Queue
from threading import Thread

class Client:
	def __init__(self):
		self.msgs = Queue()
		self.player = None
		self.sock = None

	def getMsg(self):
		try:
			for msg in readMsg(self.sock):
				self.msgs.put(msg)
		except Exception as e:
			print(e)

	def getPrompt(self):
		return self.player.pid+'>' if self.player else '>'

	def cmdLoop(self):
		try:
			addr = ('localhost',20000)
			self.sock = socket(AF_INET,SOCK_STREAM)
			self.sock.connect(addr)

			Thread(target=self.getMsg,daemon=True).start()
# -----------------------------------------------------------------------------
			while True:
				cmd,*params = input(self.getPrompt()).split()

				if cmd == "exit":
					self.sock.shutdown(SHUT_RDWR)
					break

				elif cmd == "login":
					pid,passwd = params
					msg = {'type':'login','pid':pid,'passwd':passwd}
					writeMsg(self.sock,msg)

				elif cmd == "logout":
					msg = {'type':'logout'}
					writeMsg(self.sock,msg)

				elif cmd == "say":
					print(params)
					toPid,content = params
					msg = {'type':'say','toPid':toPid,'content':content}
					writeMsg(self.sock,msg)

				elif cmd == "get":
					if self.msgs.empty():
						print("msgs is empty!")
					else:
						self.processMsg(self.msgs.get())
# -----------------------------------------------------------------------------						
		finally:
			self.sock.close()

	def processMsg(self,msg):
		mt = msg['type']

		if mt == 'loginOk':
			p = msg['player']
			self.player = Player(p['pid'],age=p['age'])

		elif mt == 'say':
			print("{}: {}".format(msg['fromPid'],msg['content']))

if __name__ == '__main__':
	client = Client()
	client.cmdLoop()
	print("---------main exit------------")
