from msg_util import *
import struct
import asyncio
import pb.msg_pb2 as pb


def send_msg(transport, msg):
    transport.write(add_header(msg.SerializeToString()))


# -----------------------------------------------------------------------------


class MsgProtocol(asyncio.Protocol):
    def __init__(self, context):
        super(MsgProtocol, self).__init__()
        self.buf = bytes()
        self.headSize = 4
        self.context = context
        self.player = None
        self.transport = None

    def connection_made(self, transport):
        print("get conn", transport)
        self.transport = transport

    def data_received(self, data):
        self.buf += data
        if len(self.buf) < self.headSize:
            return
        body_size, = struct.unpack('<I', self.buf[:self.headSize])
        if len(self.buf) < self.headSize + body_size:
            return
        bin_data = self.buf[self.headSize:self.headSize + body_size]
        # we should override the *process_msg* method!
        msg = pb.Msg()
        msg.ParseFromString(bin_data)
        self.process_msg(msg)

        self.buf = self.buf[self.headSize + body_size:]

    def process_msg(self, msg):
        print("got msg: ",msg)
        send_msg(self.transport,msg)

    def connection_lost(self, exc):
        print("connection lost: ", exc)
        # if exc is not None:
        #     print("---exit EXCEPTION---")
        # else:
        #     print("---exit normal---")


# -----------------------------------------------------------------------------


class Context:
    def __init__(self):
        self.players = [Player('wen', passwd='123', age=40), Player('zhong', passwd='456', age=10)]
        self.protocols = set()

    def get_player(self, pid, passwd):
        for player in self.players:
            if player.pid == pid and player.passwd == passwd:
                return player
        return None

    def add(self, protocol):
        self.protocols.add(protocol)

    def remove(self, protocol):
        self.protocol.remove(protocol)

    def get_protocol(self, pid):
        for protocol in self.protocols:
            if protocol.player.pid == pid:
                return protocol
        return None


# -----------------------------------------------------------------------------


async def main():
    context = Context()
    loop = asyncio.get_running_loop()
    server = await loop.create_server(lambda: MsgProtocol(context), '127.0.0.1', 20000)
    async with server:
        await server.serve_forever()


# -----------------------------------------------------------------------------
asyncio.run(main())
# -----------------------------------------------------------------------------
