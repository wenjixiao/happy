package main

import (
	"github.com/golang/protobuf/proto"
	"log"
	"mynet"
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

func EncodeMsg(msg *pb.Msg) []byte {
	data, err := proto.Marshal(msg)
	if err != nil {
		log.Fatalf("protobuf marshal: %v\n", err)
	}
	return data
}

func DecodeMsg(msgBytes []byte) *pb.Msg {
	msg := &pb.Msg{}
	err := proto.Unmarshal(msgBytes, msg)
	if err != nil {
		log.Fatalf("protobuf unmarshal: %v", err)
	}
	return msg
}

func SendMsg(conn net.Conn, msg *pb.Msg) {
	data := EncodeMsg(msg)
	_, err := conn.Write(mynet.AddHeader(data))
	if err != nil {
		log.Fatalf("conn write: %v\n", err)
	}
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
	protocol.Receiver = protocol
	return protocol
}

func (context *Context) MainLoop() {
	for {
		select {
		//=========================================================
		case message := <-context.Messages:
			log.Println(message.Msg)
			SendMsg(message.Protocol.Conn,message.Msg)
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
	sp.Context.Messages <- &Message{DecodeMsg(msgBytes), sp}
}

//---------------------------------------------------------

func main() {
	context := DefaultContext()
	//启动主服务
	go context.MainLoop()
	mynet.ListenForever(ADDR, context)
}
