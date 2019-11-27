import asyncio
import struct

def packMsg(bin):
    header = struct.pack('I',len(bin))
    return header+bin
        
class MsgProtocol(asyncio.Protocol):
    def __init__(self):
        asyncio.Protocol.__init__(self)
        self.bufSize = 1024
        self.buf = bytes()
        self.headSize = 4

    def connection_made(self,transport):
        self.transport = transport

    def data_received(self,data):
        self.buf += data
        if len(self.buf) < self.headSize:
            # print("dataSize < headSize!")
            return
        bodySize, = struct.unpack('<I',self.buf[:self.headSize])
        # print("bodySize={}".format(bodySize))
        if len(self.buf) < self.headSize+bodySize:
            # print("message data not enougth!")
            return
        bin = self.buf[self.headSize:self.headSize+bodySize]
        self.process_msg(bin)
        self.buf = self.buf[self.headSize+bodySize:]
            
    def process_msg(self,bin):
        pass

    def connection_lost(self,exc):
        # print("connection lost exception:{}".format(exc))
        if exc is not None:
            print("---exit EXCEPTION---")
            
            self.buf = bytes()
        else:
            print("---exit normal---")
