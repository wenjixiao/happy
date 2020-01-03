import json
import struct
import asyncio



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
# -----------------------------------------------------------------------------