import asyncio
import struct
import message_pb2 as message
import logging

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
            logging.debug("dataSize < headSize!")
            return
        bodySize, = struct.unpack('<I',self.buf[:self.headSize])
        logging.debug("bodySize={}".format(bodySize))
        if len(self.buf) < self.headSize+bodySize:
            logging.debug("message data not enougth!")
            return
        bin = self.buf[self.headSize:self.headSize+bodySize]
        
        msg = message.Msg()
        msg.ParseFromString(bin)
		
        self.process_msg(msg)
        
        self.buf = self.buf[self.headSize+bodySize:]
            
    def process_msg(self,msg):
        pass
    
    def send_msg(self,msg):
    	self.transport.write(packMsg(msg.SerializeToString()))

    def connection_lost(self,exc):
        logging.debug("connection lost exception:{}".format(exc))
        if exc is not None:
            logging.info("---exit EXCEPTION---")
            self.buf = bytes()
        else:
            logging.info("---exit normal---")
