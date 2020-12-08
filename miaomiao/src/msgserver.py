import asyncio
import logging
import message_pb2 as message
import msgprotocol
import msgdb
import linebroken

logging.basicConfig(level = logging.DEBUG)

# protocol has attribute named user,and than use user's name as key ,make a map
username_protocols = {}
# main entity of the server
live_id_games = {}
dead_id_games = {}
# -------------------------------------
send_user_msg(user,msg):
	username_protocols[user.name].send_msg(msg)

send_users_msg(users,msg):
	map(lambda user: send_user_msg(user,msg),users)

# -------------------------------------
def end_game(game,result):
	game.state = message.State.stopped
	game.result.CopyFrom(result)
	
	msg = message.Msg()
	msg.type = message.MsgType.TGameOver
	msg.game_over.game_id = game.id
	msg.game_over.result.CopyFrom(result)
	
	del live_id_games[game.id]
	dead_id_games[game.id] = game
	# tell broken timer,the game is dead
	linebroken.broken_timer.game_over(game)
	
	send_users_msg(game.users,msg)
	send_users_msg(game.watchers,msg)
# -------------------------------------
def game_other_user(users,user):
	assert len(users) == 2
	return users[0] == user? users[1] : users[0]
	
class MsgServerProtocol(msgprotocol.MsgProtocol):
    def __init__(self):
        msgprotocol.MsgProtocol.__init__(self)
        self.user = None

    # override
    def connection_lost(self,exc):
        logging.debug("connection losted")
        msgprotocol.MsgProtocol.connection_lost(self,exc)
        if exc is not None:
        	logging.info("---exit EXCEPTION---")
			if self.user is not None:
				linebroken.user_line_broken(user)
    # override
    def process_msg(self,msg):
        logging.debug(msg)
        
        # login 
        if msg.type == message.MsgType.TLogin:
            user = msgdb.get_user(msg.login.name)
            if user is not None:
            	# found the user from users db
                logging.debug("---login ok!---")
        		# tag protocol,who am i        
                self.user = user
                # global manage protocols
                username_protocols[self.user.name] = protocol
                #if i am line broken
                if user in broken_users:
                	linebroken.user_come_back(user)
                #-----------------------------------
                msg = message.Msg()
                msg.type = message.MsgType.TLoginOk
                self.send_msg(msg)
            else:
            	# the user not in db
                logging.debug("---login error!---")
                msg = message.Msg()
                msg.type = message.MsgType.TLoginFail
                self.send_msg(msg)
        else:
        	# no that msg!
            logging.info("not support the type msg now")
    
async def main():
    loop = asyncio.get_running_loop()
    
    server = await loop.create_server(
        lambda: MsgServerProtocol(), '127.0.0.1', 5678)
    
    async with server:
        await server.serve_forever()
        
asyncio.run(main())