import json
import struct


class Player:
	def __init__(self, pid, **props):
		"""pid,passwd,age"""
		self.pid = pid
		self.__dict__.update(props)

	def __eq__(self, other):
		return self.pid == other.pid

	def __hash__(self):
		return hash(self.pid)

	def __str__(self):
		return "player(pid={},age={})".format(self.pid, self.age)


# -----------------------------------------------------------------------------


def add_header(data):
	header = struct.pack('I', len(data))
	return header + data


def decode_msg(data):
	return json.loads(data.decode())


def encode_msg(msg):
	return json.dumps(msg).encode()

# -----------------------------------------------------------------------------
