from concurrent.futures import ThreadPoolExecutor
from msg_util import *
from socket import *
from threading import Lock

class Session:
	def __init__(self,sock,player):
		self.sock = sock
		self.player = player

	def __eq__(self,other):
		return self.player == other.player

	def __hash__(self):
		return hash(self.player)

	def __str__(self):
		return "session(pid={})".format(self.player.pid)

class Context:
	def __init__(self):
		self.players = [Player('wen',passwd='123',age=40),Player('zhong',passwd='456',age=10)]
		self.sessions = set()
		self.lock = Lock()

	def getPlayer(self,pid,passwd):
		for player in self.players:
			if player.pid == pid and player.passwd == passwd:
				return player
		return None

	def add(self,session):
		with self.lock:
			self.sessions.add(session)

	def remove(self,session):
		with self.lock:
			self.sessions.remove(session)

	def getSession(self,pid):
		for session in self.sessions:
			if session.player.pid == pid:
				return session
		return None
# -----------------------------------------------------------------------------
def processMsg(context,session,addr,msg):
	print(addr,msg)

	if msg['type'] == 'login':
		player = context.getPlayer(msg['pid'],msg['passwd'])
		if player:
			session.player = player
			context.add(session)
			loginOk = {'type':'loginOk','player':{'pid':player.pid,'age':player.age}}
			writeMsg(session.sock,loginOk)
		else:
			print("***no that player!***")

	elif msg['type'] == 'logout':
		context.remove(session)

	elif msg['type'] == 'say':
		toSession = context.getSession(msg['toPid'])
		del msg['toPid']
		msg['fromPid'] = session.player.pid
		writeMsg(toSession.sock,msg)

def handleConn(context,sock,addr):
	session = Session(sock,None)
	try:
		for msg in readMsg(sock):
			processMsg(context,session,addr,msg)
		print("----exit normal----")
	except Exception as e:
		print("****exit exception****")
		print(e)
	finally:
		sock.close()

def listenAndServ():
	try:
		context = Context()

		pool = ThreadPoolExecutor(10)
		addr = ('localhost',20000)
		servSock = socket(AF_INET,SOCK_STREAM)
		servSock.bind(addr)
		servSock.listen(5)

		while True:
			sock,addr = servSock.accept()
			print('Got connection from', addr,sock)
			pool.submit(handleConn,context,sock,addr)

	finally:
		servSock.close()

def main():
	listenAndServ()

if __name__ == '__main__':
	main()