import asyncio
import logging
import message_pb2 as message
import msgprotocol

logging.basicConfig(level = logging.DEBUG)

players_and_transports = []
games = []

class MsgServerProtocol(msgprotocol.MsgProtocol):
    def __init__(self):
        msgprotocol.MsgProtocol.__init__(self)
        self.player = None
        
    def leave(self):
        global players_and_transports
        players_and_transports = [t[0] for t in players_and_transports if t[0] != self.player]
        self.player = None

    # override
    def connection_made(self,transport):
        msgprotocol.MsgProtocol.connection_made(self,transport)
    
    # override
    def connection_lost(self,exc):
        msgprotocol.MsgProtocol.connection_lost(self,exc)
        if exc is not None:
            self.leave()
            
    def process_msg(self,bin):
        msg = message.Msg()
        msg.ParseFromString(bin)
        
        logging.debug(msg)
        
        # login 
        if msg.type == message.MsgType.LOGIN:
            player = message.Player()
            player.pid = msg.login.pid
            player.passwd = msg.login.passwd
            
            players = [pt[0] for pt in players_and_transports]
            if player not in players:
                self.player = player
                players_and_transports.append((self.player,self.transport))
            
        # logout
        elif msg.type == message.MsgType.LOGOUT:
            self.leave()
            
        # info
        elif msg.type == message.MsgType.INFO:
            msg = message.Msg()
            msg.type = message.MsgType.PLAYERS_AND_GAMES
            players = [pt[0] for pt in players_and_transports]
            for player in players:
                msg.players_and_games.players.add().CopyFrom(player)
            for game in games:
                msg.players_and_games.games.add().CopyFrom(game)
                
            self.transport.write(msgprotocol.packMsg(msg.SerializeToString()))
            
        else:
            logging.info("not support the type msg now")
    
async def main():
    loop = asyncio.get_running_loop()
    
    server = await loop.create_server(
        lambda: MsgServerProtocol(), '127.0.0.1', 5678)
    
    async with server:
        await server.serve_forever()
        
asyncio.run(main())