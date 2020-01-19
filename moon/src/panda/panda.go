package panda

import (
	"log"
	"mynet"
	"mynet/encoder/pbencoder"
	"net"
	"pb"
)


var MyMsgSender *MsgSender
var MyProtocolsManager *ProtocolsManager

//---------------------------------------------------------

func init() {
	MyMsgSender = NewMsgSender()
	MyProtocolsManager = NewProtocolsManager()
}

//---------------------------------------------------------

type ServerProtocol struct {
	mynet.MsgProtocol
	Conn   net.Conn
	Player *pb.Player
}

func (sp *ServerProtocol) ConnectionMade(conn net.Conn) {
	log.Printf("connection made: %v", conn.RemoteAddr())
	sp.Conn = conn
}

func (sp *ServerProtocol) ConnectionLost(err error) {
	log.Printf("connection lost: %v", err)
}

// override MsgProcessor
func (sp *ServerProtocol) ProcessMsg(msgBytes []byte) {
	msg := pbencoder.DecodeMsg(msgBytes)
	log.Printf("get msg: %v", msg)
	switch msg.GetType() {
	case pb.Type_LOGIN:
		player := &pb.Player{Pid: msg.GetLogin().GetPid()}
		sp.Player = player
		MyProtocolsManager.AddProtocol(sp)
		loginResult := &pb.Msg{Type: pb.Type_LOGIN_RESULT, Union: &pb.Msg_LoginResult{&pb.LoginResult{Success: true}}}
		MyProtocolsManager.SendPidMsg(player.GetPid(), loginResult)
	}
}

//---------------------------------------------------------

type ProtocolFactory struct{}

func (pf *ProtocolFactory) CreateProtocol() mynet.Protocol {
	protocol := &ServerProtocol{}
	protocol.MsgReceiver = protocol
	return protocol
}

//---------------------------------------------------------

type myPidMsg struct {
	pid string
	msg *pb.Msg
}

type myProtocolPlayer struct {
	protocol *ServerProtocol
	player *pb.Player
}

type ProtocolsManager struct {
	Protocols          []*ServerProtocol
	AddProtocolChan    chan *ServerProtocol
	RemoveProtocolChan chan *ServerProtocol
	ChangeProtocolChan chan *myProtocolPlayer
	PidMsgChan         chan *myPidMsg
	BesidesPidMsgChan  chan *myPidMsg
}

func NewProtocolsManager() *ProtocolsManager {
	mypm := &ProtocolsManager{
		AddProtocolChan:    make(chan *ServerProtocol, 6),
		RemoveProtocolChan: make(chan *ServerProtocol, 6),
		ChangeProtocolChan: make(chan *myProtocolPlayer, 6),
		PidMsgChan:         make(chan *myPidMsg, 6),
		BesidesPidMsgChan:  make(chan *myPidMsg, 6)}
	go mypm.Run()
	return mypm
}

func (pm *ProtocolsManager) AddProtocol(p *ServerProtocol) {
	pm.AddProtocolChan <- p
}

func (pm *ProtocolsManager) RemoveProtocol(p *ServerProtocol) {
	pm.RemoveProtocolChan <- p
}

func (pm *ProtocolsManager) ChangeProtocol(protocol *ServerProtocol,player *pb.Player){
	pm.ChangeProtocolChan <- &myProtocolPlayer{protocol,player}
}

func (pm *ProtocolsManager) SendPidMsg(pid string, msg *pb.Msg) {
	pm.PidMsgChan <- &myPidMsg{pid, msg}
}

func (pm *ProtocolsManager) SendWithoutPidMsg(pid string, msg *pb.Msg) {
	pm.BesidesPidMsgChan <- &myPidMsg{pid, msg}
}

func (pm *ProtocolsManager) Run() {
	for {
		select {
		case protocol := <-pm.AddProtocolChan:
			pm.Protocols = append(pm.Protocols, protocol)

		case protocol := <-pm.RemoveProtocolChan:
			if newProtocols, removeOk := remove_protocol(pm.Protocols, protocol); removeOk {
				pm.Protocols = newProtocols
			}

		case protocolPlayer := <-pm.ChangeProtocolChan:
			for _, p := range pm.Protocols {
				if p == protocolPlayer.protocol {
					p.Player = protocolPlayer.player
				}
			}

		case pidMsg := <-pm.PidMsgChan:
			for _, protocol := range pm.Protocols {
				if protocol.Player.Pid == pidMsg.pid {
					MyMsgSender.SendMsg(protocol.Conn, pidMsg.msg)
				}
			}

		case withoutPidMsg := <-pm.BesidesPidMsgChan:
			for _, protocol := range pm.Protocols {
				if protocol.Player.Pid != withoutPidMsg.pid {
					MyMsgSender.SendMsg(protocol.Conn, withoutPidMsg.msg)
				}
			}
		}
	}
}

//---------------------------------------------------------

func remove_protocol(protocols []*ServerProtocol, protocol *ServerProtocol) (newProtocols []*ServerProtocol, removeOk bool) {
	index := -1
	for i, p := range protocols {
		if p == protocol {
			index = i
			break
		}
	}

	removeOk = index != -1

	if removeOk {
		left := protocols[:index]
		right := protocols[index+1:]
		newProtocols = append(left, right...)
	}

	return
}

//---------------------------------------------------------

type myConnMsg struct {
	Conn net.Conn
	Msg  *pb.Msg
}

type MsgSender struct {
	myConnMsgChan chan *myConnMsg
}

func NewMsgSender() *MsgSender {
	msgSender := &MsgSender{make(chan *myConnMsg, 6)}
	go msgSender.Run()
	return msgSender
}

func (ms *MsgSender) SendMsg(conn net.Conn, msg *pb.Msg) {
	ms.myConnMsgChan <- &myConnMsg{conn, msg}
}

func (ms *MsgSender) Run() {
	for {
		myConnMsg := <-ms.myConnMsgChan
		pbencoder.SendMsg(myConnMsg.Conn, myConnMsg.Msg)
	}
}
