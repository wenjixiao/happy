from socket import *
from msg_util import *
from queue import Queue
from threading import Thread
import time
import pb.msg_pb2 as pb



def write_msg(sock, msg):
	sock.sendall(add_header(msg.SerializeToString()))


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

			msg = pb.Msg()
			msg.ParseFromString(bin_data)
			yield msg

			buf = buf[head_size + body_size:]


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
					pid, password = params

					msg = pb.Msg()
					msg.login.pid = pid
					msg.login.password = password

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
		print("get msg: ",msg)

# -----------------------------------------------------------------------------
if __name__ == '__main__':
	client = Client()
	client.cmd_loop()
	print("---------main exit------------")
