import msgserver
import brokentimer

# line broken users
broken_users = []
broken_timer = brokentimer.BrokenTimer()

def user_line_broken(user):
	# first,when line broken,protocol is useless
	del msgserver.username_protocols[user.name]
	broken_users.append(user)
	
	for game in msgserver.live_id_games.values():
		game_users = game.users
		#as player
		if user in game_users:
			other_user = msgserver.game_other_user(game_users,user)
			if other_user not in broken_users:
				if not game.line_broken:
					game.line_broken = True
					# first,tell other user the game line broken!
					msg = message.Msg()
					msg.type = message.MsgType.TLineBroken
					msg.line_broken.game_id = game.id
					msgserver.send_user_msg(other_user,msg)
				else:
					# already linebroken,nothing to do another
					pass
			# line broken count down
			broken_timer.add_game_user(game,user)
		#as watcher
		if user in game.watchers:
			# wather line broken
			game.watchers.remove(user)
			# wacher leave msg
			msg = message.Msg()
			msg.type = message.MsgType.TWatcherLeave
			msg.watcher_leave.game_id = game.id
			msg.watcher_leave.user = user
			msgserver.send_users_msg(game.users + game.watchers,msg)
			
def user_come_back(user):
	# line broken user come back.
	# some game paused for the user line broken,we should rerun it
	broken_users.remove(user)
	broken_timer.remove_user(user)
	
	for game in msgserver.live_id_games.values():
		game_users = game.users
		if user in game_users:
			# first,give the game data to the comeback user
			msg = message.Msg()
			msg.type = message.MsgType.TGameData
			msg.game_data.game = game
			# now,the game is linebroken!
			msgserver.send_user_msg(user,msg)
			# can we restart the game? it's about other_user
			other_user = msgserver.game_other_user(game_users,user) 
			if other_user not in broken_users:
				# i come back,and you not line broken,so we can start again
				assert game.line_broken == True
				game.line_broken = False
				msg = message.Msg()
				msg.type = message.MsgType.TComeBack
				msg.come_back.game_id = game.id
				msgserver.send_users_msg(game_users,msg)
			else:
				# means other_user linebroken too,and not login now. nothing to do!
				pass