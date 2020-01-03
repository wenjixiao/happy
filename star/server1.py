from msg_util import MsgProtocol,Player
import asyncio

class MsgServerProtocol(MsgProtocol):
    def __init__(self,context):
        super(MsgServerProtocol,self).__init__()
        self.context = context
        self.player = None

    def process_msg(self,msg):
        print(msg)
        if msg['type'] == 'login':
            player = self.context.getPlayer(msg['pid'],msg['passwd'])
            if player:
                self.player = player
                self.context.add(self)
                loginOk = {'type':'loginOk','player':{'pid':player.pid,'age':player.age}}
                self.send_msg(loginOk)
            else:
                print("***no that player!***")

        elif msg['type'] == 'logout':
            self.context.remove(self)

        elif msg['type'] == 'say':
            toProtocol = self.context.getProtocol(msg['toPid'])
            del msg['toPid']
            msg['fromPid'] = self.player.pid
            self.send_msg(msg)

    def connection_lost(self,exc):
        print("connection lost: ",exc)
        # if exc is not None:
        #     print("---exit EXCEPTION---")
        # else:
        #     print("---exit normal---")

class Context:
    def __init__(self):
        self.players = [Player('wen',passwd='123',age=40),Player('zhong',passwd='456',age=10)]
        self.protocols = set()

    def getPlayer(self,pid,passwd):
        for player in self.players:
            if player.pid == pid and player.passwd == passwd:
                return player
        return None

    def add(self,protocol):
        self.protocols.add(protocol)

    def remove(self,protocol):
        self.protocol.remove(protocol)

    def getProtocol(self,pid):
        for protocol in self.protocols:
            if protocol.player.pid == pid:
                return protocol
        return None

async def main():
    context = Context()
    loop = asyncio.get_running_loop()
    server = await loop.create_server(lambda: MsgServerProtocol(context), '127.0.0.1', 20000)
    async with server:
        await server.serve_forever()
        
asyncio.run(main())