from socket import *
from msg_util import *
from queue import Queue
from threading import Thread
import time

# -----------------------------------------------------------------------------
def readMsg(sock):
	"maybe *BLOCK*,when sock recv no data"
	HeadSize = 4
	buf = bytes()
	while True:
		data = sock.recv(8192)
		if not data:
			break
		buf += data

		while True:
			if len(buf) < HeadSize:
				break
			bodySize, = struct.unpack('<I',buf[:HeadSize])
			if len(buf) < HeadSize+bodySize:
				break
			bin = buf[HeadSize:HeadSize+bodySize]
			yield decodeMsg(bin)
			buf = buf[HeadSize+bodySize:]

def writeMsg(sock,msg):
	sock.sendall(addHeader(encodeMsg(msg)))
# -----------------------------------------------------------------------------
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

			while True:
				line = input(self.getPrompt())

				if line == '':
					self.checkMsgs()
					continue

				cmd,*params = line.split()

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

				time.sleep(1)
				self.checkMsgs()
		finally:
			self.sock.close()

	def checkMsgs(self):
		print("----check msgs----")
		while not self.msgs.empty():
			self.processMsg(self.msgs.get())

	def processMsg(self,msg):
		mt = msg['type']

		if mt == 'loginOk':
			p = msg['player']
			self.player = Player(p['pid'],age=p['age'])

		elif mt == 'say':
			print("{}: {}".format(msg['fromPid'],msg['content']))
# -----------------------------------------------------------------------------
if __name__ == '__main__':
	client = Client()
	client.cmdLoop()
	print("---------main exit------------")
