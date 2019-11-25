import asyncio
import msgprotocol
import message_pb2 as message

class MsgServerProtocol(msgprotocol.MsgProtocol):
    def connection_made(self,transport):
        self.transport = transport
 
    def process_msg(self,bin):
        msg = message.Msg()
        msg.ParseFromString(bin)
        print("get client msg:\n",msg)
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