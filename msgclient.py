import asyncio
import msgprotocol
import message_pb2 as message

class MsgClientProtocol(msgprotocol.MsgProtocol):
    def process_msg(self,bin):
        msg = message.Msg()
        msg.ParseFromString(bin)
        print("get server msg:\n",msg)
    
async def getCommand():
    return input(">>>")

async def main():
    loop = asyncio.get_running_loop()

    transport,protocol = await loop.create_connection(
        lambda: MsgClientProtocol(),'127.0.0.1',5678)
    
    while True:
        cmd = await loop.create_task(getCommand())
        if not cmd == "bye":
            msg = message.Msg()
            msg.name = cmd
            transport.write(msgprotocol.packMsg(msg.SerializeToString()))
            # give a chance to other coroutines
            await asyncio.sleep(1)
        else:
            transport.close()
            break
            
    loop.stop()
    
asyncio.run(main())