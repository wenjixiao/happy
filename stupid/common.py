import pb.msg_pb2 as pb
import logging

def validate(point):
	return point.x >= -9 and point.x <= 9 and point.y >= -9 and point.y <= 9

def otherColor(color):
	return pb.Color.BLACK if color == pb.Color.WHITE else pb.Color.WHITE
