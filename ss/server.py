
from msg_util import *
from socket import *

def processMsg(sock,msg):
	print(msg)
	writeMsg(sock,msg)

def listen():
	try:
		addr = ('localhost',20000)
		servSock = socket(AF_INET,SOCK_STREAM)
		servSock.bind(addr)
		servSock.listen(5)

		while True:
			cliSock,addr = servSock.accept()
			print('Got connection from', addr)
			try:
				for msg in readMsg(cliSock):
					processMsg(cliSock,msg)
				print("----exit normal----")
			except Exception as e:
				print(e)
			finally:
				cliSock.close()
	finally:
		servSock.close()

def main():
	listen()

if __name__ == '__main__':
	main()