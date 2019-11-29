import asyncio
import logging
import message_pb2 as message
import msgprotocol
import msgdb

logging.basicConfig(level = logging.DEBUG)

protos = [] # proto have player and transport attr
games = []

class MsgServerProtocol(msgprotocol.MsgProtocol):
    def __init__(self):
        msgprotocol.MsgProtocol.__init__(self)
        self.player = None
        
    def leave(self):
        logging.debug("---leave invokded---")
        if self.player is not None:
            self.player = None
            global protos
            protos = [p for p in protos if self is not p]
        else:
            # connected but not login.
            # because when we connect,we do nothing,so,now we do nothing
            logging.debug("---connected,but not login.---")


    # override
    def connection_made(self,transport):
        logging.debug("connection maded")
        msgprotocol.MsgProtocol.connection_made(self,transport)
    
    # override
    def connection_lost(self,exc):
        logging.debug("connection losted")
        msgprotocol.MsgProtocol.connection_lost(self,exc)
        self.leave()
            
    def process_msg(self,bin):
        msg = message.Msg()
        msg.ParseFromString(bin)
        
        logging.debug(msg)
        
        # login 
        if msg.type == message.MsgType.LOGIN:
            player = msgdb.get_player(msg.login.pid,msg.login.passwd)
            if player is not None:
                logging.debug("---login ok!---")
                self.player = player
                if self not in protos:
                    protos.append(self)

                msg = message.Msg()
                msg.type = message.MsgType.LOGIN_OK

                msg.login_ok.player.CopyFrom(self.player)
                for myp in  [p.player for p in protos]:
                    msg.login_ok.data.players.add().CopyFrom(myp)
                for game in games:
                    msg.login_ok.data.games.add().CopyFrom(game)
                self.transport.write(msgprotocol.packMsg(msg.SerializeToString()))

            else:
                logging.debug("---login error!---")
                msg = message.Msg()
                msg.type = message.MsgType.LOGIN_FAIL
                self.transport.write(msgprotocol.packMsg(msg.SerializeToString()))

        # logout
        elif msg.type == message.MsgType.LOGOUT:
            self.leave()
            
        # info
        elif msg.type == message.MsgType.INFO:
            msg = message.Msg()
            msg.type = message.MsgType.DATA
            for player in [p.player for p in protos]:
                msg.data.players.add().CopyFrom(player)
            for game in games:
                msg.data.games.add().CopyFrom(game)
                
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