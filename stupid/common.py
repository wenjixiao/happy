import pb.msg_pb2 as pb
import logging

def otherColor(color):
	return pb.Color.BLACK if color == pb.Color.WHITE else pb.Color.WHITE
