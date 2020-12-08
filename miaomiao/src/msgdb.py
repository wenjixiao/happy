import sqlite3
import message_pb2 as message

conn = sqlite3.connect("my.db")

def init_db():
	c = conn.cursor()
	c.execute('''create table users(name text,level text) ''')
	users = [ ('wen','3d'), ('zhong','1k')]
	c.executemany('''insert into users VALUES (?,?) ''',users)
	c.close()
	conn.commit()

def get_user(name):
	c = conn.cursor()
	c.execute('''select name,level from users where name=?''',(name,))
	p = c.fetchone()
	c.close()

	if p is not None:
		user = message.User()
		user.name = p[0]
		user.level = p[1]
		return user
	else:
		return None

def close_db():
	conn.close()

