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
	PlayersDB   []*pb.Player
	Protocols   []*ServerProtocol
	Messages    chan *Message
	OpProtocols chan *OpProtocol
}

func DefaultContext() *Context {
	p1 := &pb.Player{Pid: "wen", Passwd: "123", Age: 40}
	p2 := &pb.Player{Pid: "zhong", Passwd: "456", Age: 10}
	players := []*pb.Player{p1, p2}
	messages := make(chan *Message, 12)
	opProtocols := make(chan *OpProtocol, 6)
	protocols := make([]*ServerProtocol, 6)
	return &Context{PlayersDB: players, Protocols: protocols, Messages: messages, OpProtocols: opProtocols}
}

func (context *Context) AddProtocol(protocol *ServerProtocol) {
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

func (context *Context) RemoveProtocol(protocol *ServerProtocol) bool {
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

func (context *Context) MainService() {
	for {
		select {
		//---------------------------------------------------------
		case message := <-context.Messages:
			msg, protocol := message.Msg, message.Protocol

			switch msg.GetType() {
			case pb.Type_LOGIN:
				login := msg.GetLogin()
				var player *pb.Player
				var success bool
				player, success = protocol.Context.GetPlayer(login.GetPid(), login.GetPasswd())
				msg := &pb.Msg{Type: pb.Type_LOGIN_RESULT,
					Union: &pb.Msg_LoginResult{&pb.LoginResult{Player: player, Success: success}}}
				mynet.SendMsg(protocol.Conn, msg)
				if success {
					protocol.Player = player
				}
			case pb.Type_LOGOUT:
				protocol.Player = nil
			}

		//---------------------------------------------------------
		case opProtocol := <-context.OpProtocols:
			if opProtocol.AddOrRemove {
				context.AddProtocol(opProtocol.Protocol)
			} else {
				context.RemoveProtocol(opProtocol.Protocol)
			}
			//---------------------------------------------------------
		}
	}
	log.Println("----main service exit!----")
}

//---------------------------------------------------------
type ServerProtocol struct {
	Context *Context
	Conn    net.Conn
	Player  *pb.Player
}

type Message struct {
	Msg      *pb.Msg
	Protocol *ServerProtocol
}

type OpProtocol struct {
	AddOrRemove bool
	Protocol    *ServerProtocol
}

func (protocol *ServerProtocol) ConnectionMade(conn net.Conn) {
	log.Printf("connection made: %v", conn)
	protocol.Conn = conn
	protocol.Context.OpProtocols <- &OpProtocol{AddOrRemove: true, Protocol: protocol}
}

func (protocol *ServerProtocol) MsgReceived(msg *pb.Msg) {
	log.Printf("protocol received: %v", msg)
	protocol.Context.Messages <- &Message{msg, protocol}
}

func (protocol *ServerProtocol) ConnectionLost(err error) {
	log.Printf("connection lost: %v", err)
	protocol.Context.OpProtocols <- &OpProtocol{AddOrRemove: false, Protocol: protocol}
}

//---------------------------------------------------------
func ListenAndService() {
	context := DefaultContext()
	//启动主服务
	go context.MainService()

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

		protocol := &ServerProtocol{Context: context}
		go mynet.HandleConn(conn, protocol)
	}
	log.Println("----listenAndService exit!----")
}

func main() {
	ListenAndService()
}
