from socket import *
from msg_util import *
from queue import Queue
from threading import Thread
import time


# -----------------------------------------------------------------------------
def read_msg(sock):
	"""maybe *BLOCK*,when sock recv no data"""
	head_size = 4
	buf = bytes()
	while True:
		data = sock.recv(8192)
		if not data:
			break
		buf += data

		while True:
			if len(buf) < head_size:
				break
			body_size, = struct.unpack('<I', buf[:head_size])
			if len(buf) < head_size + body_size:
				break
			bin_data = buf[head_size:head_size + body_size]
			yield decode_msg(bin_data)
			buf = buf[head_size + body_size:]


def write_msg(sock, msg):
	sock.sendall(add_header(encode_msg(msg)))


# -----------------------------------------------------------------------------
class Client:
	def __init__(self):
		self.msgs = Queue()
		self.player = None
		self.sock = None

	def get_msg(self):
		"invoke by msg reading thread"
		try:
			for msg in read_msg(self.sock):
				self.msgs.put(msg)
		except Exception as e:
			print(e)

	def get_prompt(self):
		return self.player.pid + '>' if self.player else '>'

	def cmd_loop(self):
		try:
			addr = ('localhost', 20000)
			self.sock = socket(AF_INET, SOCK_STREAM)
			self.sock.connect(addr)

			Thread(target=self.get_msg, daemon=True).start()

			while True:
				line = input(self.get_prompt())

				if line == '':
					self.check_msgs()
					continue

				cmd, *params = line.split()

				if cmd == "exit":
					self.sock.shutdown(SHUT_RDWR)
					break

				elif cmd == "login":
					pid, passwd = params
					msg = {'type': 'login', 'pid': pid, 'passwd': passwd}
					write_msg(self.sock, msg)

				elif cmd == "logout":
					msg = {'type': 'logout'}
					write_msg(self.sock, msg)

				elif cmd == "say":
					print(params)
					to_pid, content = params
					msg = {'type': 'say', 'toPid': to_pid, 'content': content}
					write_msg(self.sock, msg)

				time.sleep(1)
				self.check_msgs()
		finally:
			self.sock.close()

	def check_msgs(self):
		print("----check msgs----")
		while not self.msgs.empty():
			self.process_msg(self.msgs.get())

	def process_msg(self, msg):
		mt = msg['type']

		if mt == 'loginOk':
			p = msg['player']
			self.player = Player(p['pid'], age=p['age'])

		elif mt == 'say':
			print("{}: {}".format(msg['fromPid'], msg['content']))


# -----------------------------------------------------------------------------
if __name__ == '__main__':
	client = Client()
	client.cmd_loop()
	print("---------main exit------------")
