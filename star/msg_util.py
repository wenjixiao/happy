import json
import struct

HeadSize = 4

class Player:
	def __init__(self,pid,**props):
		"pid,passwd,age"
		self.pid = pid
		self.__dict__.update(props)
		
	def __eq__(self,other):
		return self.pid == other.pid

	def __hash__(self):
		return hash(self.pid)

	def __str__(self):
		return "player(pid={},age={})".format(self.pid,self.age)
# -----------------------------------------------------------------------------
def addHeader(bin):
	header = struct.pack('I',len(bin))
	return header+bin

def decodeMsg(bin):
	return json.loads(bin.decode())

def encodeMsg(msg):
	return json.dumps(msg).encode()

def readMsg(sock):
	"maybe *BLOCK*,when sock recv no data"
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

