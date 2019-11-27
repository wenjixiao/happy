import asyncio
import msgprotocol
import message_pb2 as message

players = []
games = []

class MsgServerProtocol(msgprotocol.MsgProtocol):
    def __init__(self):
        msgprotocol.MsgProtocol.__init__(self)
        self.player = None
        
    # override
    def connection_made(self,transport):
        msgprotocol.MsgProtocol.connection_made(self,transport)
        pass
    
    # override
    def connection_lost(self,exc):
        msgprotocol.MsgProtocol.connection_lost(self,exc)
        pass

    def process_msg(self,bin):
        msg = message.Msg()
        msg.ParseFromString(bin)
        print("--------")
        print(msg)
        
        # login 
        if msg.type == message.MsgType.LOGIN:
            player = message.Player()
            player.pid = msg.login.pid
            player.passwd = msg.login.passwd
            
            if player not in players:
                players.append(player)
            
            self.player = player
        # logout
        elif msg.type == message.MsgType.LOGOUT:
            players.remove(self.player)
        # info
        elif msg.type == message.MsgType.INFO:
            msg = message.Msg()
            msg.type = message.MsgType.PLAYERS_AND_GAMES
            for player in players:
                msg.players_and_games.players.add().CopyFrom(player)
            for game in games:
                msg.players_and_games.games.add().CopyFrom(game)
                
            self.transport.write(msgprotocol.packMsg(msg.SerializeToString()))
    
    def connection_lost(self,exc):
        msgprotocol.MsgProtocol.connection_lost(self,exc)
    
async def main():
    loop = asyncio.get_running_loop()
    
    server = await loop.create_server(
        lambda: MsgServerProtocol(), '127.0.0.1', 5678)
    
    async with server:
        await server.serve_forever()
        
asyncio.run(main())