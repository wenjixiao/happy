import asyncio

class BrokenTimer:
	timeout = 10
	def __init__(self):
		self.timer_running = False
		self.game_user_timeout = []
	def add_game_user(self,game,user):
		self.game_user_timeout.append((game,user,timeout))
		if not self.timer_running:
			self.start_timer(1)
		
	def remove_user(self,user):
		self.game_user_timeout = filter(lambda u,_,_ : u != user,self.game_user_timeout)
	
	def game_over(self,game):
		self.game_user_timeout = filter(lambda g,_,_ : g != game,self.game_user_timeout)
		
	def countdown(self):
		gu = map(lambda g,u,t : (g,u,t-1),self.game_user_timeout)
		self.game_user_timeout = filter(lambda g,u,t : t > 0,gu)
		for game,user,t in self.game_user_timeout:
			msg = message.Msg()
			msg.type = message.MsgType.TCountdown
			msg.countdown.game_id = game.id
			msg.countdown.user = user
			msgserver.send_user_msg(user,msg)
			msgserver.send_users_msg(game.watchers,msg)
		for game,user,_ in filter(lambda g,u,t: t <= 0,gu):
			other_user = msgserver.game_other_user(game,user)
			result = message.Result()
			result.end_type  = message.EndType.time_out
			result.winner = msgserver.game_user_color(game,other_user)
			msgserver.end_game(game,result)
	
	def one_round_run(self,future):
		self.countdown()
		
	async def delay(self,time):
		await asyncio.sleep(time)
		
	async def start_timer(self,time):
		self.timer_running = True
		while len(self.game_user_timeout) > 0:
			future= asyncio.ensure_future(self.delay(time))
			future.add_done_callback(self.one_round_run)
			await future
		self.timer_running = False