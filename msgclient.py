import asyncio
import msgprotocol
import message_pb2 as message

class MsgClientProtocol(msgprotocol.MsgProtocol):
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
        print(msg)
    
async def getLine():
    return input(">>>")
    
async def main():
    loop = asyncio.get_running_loop()

    try:
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
                msg.login.pid = myline[1]
                msg.login.passwd = myline[2]
                
                transport.write(msgprotocol.packMsg(msg.SerializeToString()))
                
            elif cmd == "info":
                print(protocol.transport)
                msg = message.Msg()
                msg.type = message.MsgType.INFO
                print("transport is closed? ",transport.is_closing())
                transport.write(msgprotocol.packMsg(msg.SerializeToString()))
                
            else:
                print("***no that command!***\n")
    
            # give a chance to other coroutines
            await asyncio.sleep(1)

    except IOError as exc:
        print("----exception----")
        print(exc)
            
    loop.stop()
    
asyncio.run(main())