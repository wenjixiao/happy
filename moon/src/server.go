package main

import (
	"log"
	"mynet"
	"mynet/encoder/pbencoder"
	"net"
	"pb"
)

const ADDR = ":20000"

type Context struct {
	Protocols   []*ServerProtocol
	Messages    chan *Message
	OpProtocols chan *OpProtocol
}

type ServerProtocol struct {
	mynet.MsgProtocol
	Context *Context
	Conn    net.Conn
}

type Message struct {
	Msg      *pb.Msg
	Protocol *ServerProtocol
}

type OpProtocol struct {
	AddOrRemove bool
	Protocol    *ServerProtocol
}

//---------------------------------------------------------

func DefaultContext() *Context {
	messages := make(chan *Message, 12)
	opProtocols := make(chan *OpProtocol, 6)
	protocols := make([]*ServerProtocol, 6)
	return &Context{Protocols: protocols, Messages: messages, OpProtocols: opProtocols}
}

func (context *Context) AddProtocol(protocol *ServerProtocol) {
	context.Protocols = append(context.Protocols, protocol)
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

// override ProtocolFactory
func (context *Context) CreateProtocol() mynet.Protocol {
	protocol := &ServerProtocol{}
	protocol.Context = context
	protocol.MsgReceiver = protocol
	return protocol
}

func (context *Context) MainLoop() {
	for {
		select {
		//=========================================================
		case message := <-context.Messages:
			msg,protocol := message.Msg,message.Protocol
			log.Println(message.Msg)
			switch(msg.GetType()){
			case pb.Type_LOGIN:
				log.Println("****hehe****")
				loginResult := &pb.Msg{Type: pb.Type_LOGIN_RESULT,Union: &pb.Msg_LoginResult{&pb.LoginResult{Success: true}}}
				pbencoder.SendMsg(protocol.Conn,loginResult)
			}
		//=========================================================
		// protocol不光有add和remove，还有查询遍历之类的处理，所以，*不能用锁*！
		// 全部交给主线处理，简单明了，不会出错。
		case opProtocol := <-context.OpProtocols:
			if opProtocol.AddOrRemove {
				context.AddProtocol(opProtocol.Protocol)
			} else {
				context.RemoveProtocol(opProtocol.Protocol)
			}
		} // select ended
	}
	log.Println("----main service exit!----")
}

//---------------------------------------------------------

func (sp *ServerProtocol) ConnectionMade(conn net.Conn) {
	log.Printf("connection made: %v", conn)
	sp.Conn = conn
	sp.Context.OpProtocols <- &OpProtocol{AddOrRemove: true, Protocol: sp}
}

func (sp *ServerProtocol) ConnectionLost(err error) {
	log.Printf("connection lost: %v", err)
	sp.Context.OpProtocols <- &OpProtocol{AddOrRemove: false, Protocol: sp}
}

// override MsgProcessor
func (sp *ServerProtocol) ProcessMsg(msgBytes []byte) {
	log.Println("get msg")
	sp.Context.Messages <- &Message{pbencoder.DecodeMsg(msgBytes), sp}
}

//---------------------------------------------------------

func main() {
	context := DefaultContext()
	//启动主服务
	go context.MainLoop()
	mynet.ListenForever(ADDR, context)
}
