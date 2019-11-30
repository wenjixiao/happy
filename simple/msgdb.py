import sqlite3
import message_pb2 as message

conn = sqlite3.connect("my.db")

def init_db():
	c = conn.cursor()
	c.execute('''create table players(pid text,passwd text,level text) ''')
	players = [ ('wen','123','3d'), ('zhong','999','1k'), ('cat','555','9p') ]
	c.executemany('''insert into players VALUES (?,?,?) ''',players)
	c.close()
	conn.commit()

def get_player(pid,passwd):
	c = conn.cursor()
	c.execute('''select pid,level,passwd from players where pid=? and passwd=?''',(pid,passwd))
	p = c.fetchone()
	c.close()

	if p is not None:
		player = message.Player()
		player.pid = p[0]
		player.level = p[1]
		player.passwd = p[2]
		return player
	else:
		return None

def close_db():
	conn.close()

