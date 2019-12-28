from socket import *
from msg_util import *
from queue import Queue
from threading import Thread
import msg_pb2 as pb

def readingMsg(sock,msgs):
	try:
		for msg in readMsg(sock):
			msgs.put(msg)
	except Exception as e:
		print(e)

def letsCmd():
	'''
	'''
	msgs = Queue()

	addr = ('localhost',20000)
	sock = socket(AF_INET,SOCK_STREAM)
	sock.connect(addr)

	Thread(target=readingMsg,args=(sock,msgs)).start()

	try:
		while True:
			cmd = input(">").strip()

			if cmd == "exit":
				sock.shutdown(SHUT_RDWR)
				break

			elif cmd == "stone":
				stone = pb.Stone()
				stone.x = 1
				stone.y = 2
				stone.color = pb.Color.BLACK
				print(stone)
				writeMsg(sock,stone)
				print("stone sended")

			elif cmd == "get":
				while not msgs.empty():
					msg = msgs.get()
					print(msg)

	finally:
		sock.close()

def main():
	letsCmd()
	print("---------main exit------------")

if __name__ == '__main__':
	main()
