import json
import struct
import msg_pb2 as pb

HeadSize = 4

def addHeader(bin):
	header = struct.pack('I',len(bin))
	return header+bin

# def decodeMsg(bin):
# 	return json.loads(bin.decode())

# def encodeMsg(msg):
# 	return json.dumps(msg).encode()

def decodeMsg(bin):
	stone = pb.Stone()
	stone.ParseFromString(bin)
	return stone

def encodeMsg(stone):
	return stone.SerializeToString()

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

