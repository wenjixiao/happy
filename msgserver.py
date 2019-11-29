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

    def send_msg(self,msg):
        self.transport.write(msgprotocol.packMsg(msg.SerializeToString()))

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

    def make_data(self,data_parent):
        players = [p.player for p in protos]
        for p in players:
            data_parent.data.players.add().CopyFrom(p)
        for g in games:
            data_parent.data.games.add().CopyFrom(g)
            
    def process_msg(self,bin):
        msg = message.Msg()
        msg.ParseFromString(bin)
        
        logging.debug(msg)
        
        # login 
        if msg.type == message.MsgType.LOGIN:
            player = msgdb.get_player(msg.login.pid,msg.login.passwd)
            if player is not None:
                # clear the old player infomation,so we can change my role free
                self.leave()

                logging.debug("---login ok!---")
                self.player = player
                if self not in protos:
                    protos.append(self)

                msg = message.Msg()
                msg.type = message.MsgType.LOGIN_OK

                msg.login_ok.player.CopyFrom(self.player)
                self.make_data(msg.login_ok)
                self.send_msg(msg)

            else:
                logging.debug("---login error!---")
                msg = message.Msg()
                msg.type = message.MsgType.LOGIN_FAIL
                self.send_msg(msg)

        # logout
        elif msg.type == message.MsgType.LOGOUT:
            self.leave()
            
        # server data
        elif msg.type == message.MsgType.DATA:
            msg = message.Msg()
            msg.type = message.MsgType.DATA
            players = [p.player for p in protos]
            self.make_data(msg)
            self.send_msg(msg)
            
        else:
            logging.info("not support the type msg now")
    
async def main():
    loop = asyncio.get_running_loop()
    
    server = await loop.create_server(
        lambda: MsgServerProtocol(), '127.0.0.1', 5678)
    
    async with server:
        await server.serve_forever()
        
asyncio.run(main())