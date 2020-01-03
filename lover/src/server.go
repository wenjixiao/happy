package main

import (
	"log"
	"mynet"
	"net"
	"pb"
)

const addr = ":20000"

type Context struct {
	players   []*pb.Player
	protocols []*ServerMsgProtocol
}

func (context *Context) addProtocol(protocol *ServerMsgProtocol) {
	context.protocols = append(context.protocols, protocol)
}

type ServerMsgProtocol struct {
	Context *Context
	Conn    net.Conn
	Player  *pb.Player
}

func (protocol *ServerMsgProtocol) ConnectionMade(conn net.Conn) {
	protocol.Conn = conn
}

func (protocol *ServerMsgProtocol) MsgReceived(msg *pb.Msg) {
	log.Printf("protocol received: %v", msg)
	mynet.SendMsg(protocol.Conn, msg)
}

func (protocol *ServerMsgProtocol) ConnectionLost(err error) {
	log.Printf("connection lost: %v", err)
}

func ListenAndServing() {
	c := &Context{}
	listener, err := net.Listen("tcp", addr)
	defer listener.Close()

	if err != nil {
		log.Fatalf("listener: %v", err)
	}

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Fatalf("socket accept: %v", err)
		}

		protocol := &ServerMsgProtocol{Context: c}
		protocol.Context.addProtocol(protocol)
		go mynet.HandleConn(conn, protocol)
	}
}

func main() {
	ListenAndServing()
}
