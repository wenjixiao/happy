package main

import (
	"log"
	"mynet"
	"net"
	"pb"
)

const addr = ":20000"

//---------------------------------------------------------
type Context struct {
	PlayersDB []*pb.Player
	Protocols []*ServerMsgProtocol
}

func (context *Context) AddProtocol(protocol *ServerMsgProtocol) {
	context.Protocols = append(context.Protocols, protocol)
}

func (context *Context) GetPlayer(pid string, passwd string) (player *pb.Player, ok bool) {
	for _, p := range context.PlayersDB {
		if p.Pid == pid && p.Passwd == passwd {
			player, ok = p, true
			break
		}
	}
	return
}

func (context *Context) RemoveProtocol(protocol *ServerMsgProtocol) bool {
	index := -1

	for i, p := range context.Protocols {
		if p == protocol {
			index = i
			break
		}
	}

	removeOk := index != -1

	if removeOk {
		context.Protocols = append(context.Protocols[:index], context.Protocols[index+1:]...)
	}

	return removeOk
}

//---------------------------------------------------------
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
	switch msg.GetType() {
	case pb.Type_LOGIN:
		login := msg.GetLogin()
		var player *pb.Player
		var success bool
		player, success = protocol.Context.GetPlayer(login.GetPid(), login.GetPasswd())
		if success {
			protocol.Player = player
			protocol.Context.AddProtocol(protocol)
		}
		msg := &pb.Msg{Type: pb.Type_LOGIN_RESULT,
			Union: &pb.Msg_LoginResult{&pb.LoginResult{Player: player, Success: success}}}
		mynet.SendMsg(protocol.Conn, msg)
	case pb.Type_LOGOUT:
		if protocol.Context.RemoveProtocol(protocol) {
			log.Printf("--%v-- logout ok", protocol.Player.Pid)
		}
	}
}

func (protocol *ServerMsgProtocol) ConnectionLost(err error) {
	log.Printf("connection lost: %v", err)
}

//---------------------------------------------------------
func ListenAndServing() {
	p1 := &pb.Player{Pid: "wen", Passwd: "123", Age: 40}
	p2 := &pb.Player{Pid: "zhong", Passwd: "456", Age: 10}
	context := &Context{PlayersDB: []*pb.Player{p1, p2}}

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

		protocol := &ServerMsgProtocol{Context: context}
		protocol.Context.AddProtocol(protocol)
		go mynet.HandleConn(conn, protocol)
	}
}

func main() {
	ListenAndServing()
}
