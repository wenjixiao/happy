import asyncio
import msgprotocol
import message_pb2 as message

class MsgClientProtocol(msgprotocol.MsgProtocol):
    def process_msg(self,bin):
        msg = message.Msg()
        msg.ParseFromString(bin)
        print("get server msg:\n",msg)
    
async def getLine():
    return input(">>>")
    
async def main():
    loop = asyncio.get_running_loop()

    transport,protocol = await loop.create_connection(
        lambda: MsgClientProtocol(),'127.0.0.1',5678)
    
    while True:
        line = await loop.create_task(getLine())
        myline = line.split()
        cmd = myline[0]
        
        if cmd == "logout":
            msg = message.Msg()
            msg.type = message.MsgType.LOGOUT
            transport.write(msgprotocol.packMsg(msg.SerializeToString()))
            transport.close()
            break
            
        elif cmd == "login":
            msg = message.Msg()
            msg.type = message.MsgType.LOGIN
            msg.login.uid = myline[1]
            msg.login.passwd = myline[2]
            
            transport.write(msgprotocol.packMsg(msg.SerializeToString()))
        elif cmd == "info":
            
        else:
            print("***no that command!***\n")

        # give a chance to other coroutines
        await asyncio.sleep(1)
            
    loop.stop()
    
asyncio.run(main())