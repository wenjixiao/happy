from socket import *
from msg_util import *

def main():
	try:
		addr = ('localhost',20000)
		sock = socket(AF_INET,SOCK_STREAM)
		sock.connect(addr)
		d = {'name':'wenjixiao','age':20}
		writeMsg(sock,d)
		g = readMsg(sock)
		print(next(g))
	finally:
		sock.close()

if __name__ == '__main__':
	main()
