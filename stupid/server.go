package main

import (
	"./pb"
	"bytes"
	"encoding/binary"
	"github.com/golang/protobuf/proto"
	"io"
	"log"
	"net"
)

const (
	HeaderSize = 4
)

// ------------------------------------------------------------

var sessions []*Session = []*Session{}

var addSessionChan chan *Session = make(chan *Session, 5)
var removeSessionChan chan *Session = make(chan *Session, 5)
var listSessionsChan chan *Session = make(chan *Session, 5)
var inviteChan chan *InviteArg = make(chan *InviteArg, 5)

type InviteArg struct {
	Session *Session
	Invite *pb.Invite
}

// ------------------------------------------------------------

type Session struct {
	Conn   net.Conn
	Player *pb.Player
}

// ------------------------------------------------------------

func AddSession(session *Session) {
	sessions = append(sessions, session)
}

func RemoveSession(session *Session) {
	var index int
	var mysession *Session
	for index, mysession = range sessions {
		if mysession == session {
			break
		}
	}
	copy(sessions[index:], sessions[index+1:])
	sessions = sessions[:len(sessions)-1]
}

func GetPlayers() (players []*pb.Player) {
	for _, session := range sessions {
		players = append(players, session.Player)
	}
	return
}

func Leave(session *Session) {
	if session.Player != nil {
		session.Player = nil
		removeSessionChan <- session
	}
}

func StartServ() {
	// begin to serv
	for {
		select {
		case session := <-addSessionChan:
			AddSession(session)
			msg := &pb.Msg{
				Type: pb.MsgType_LOGIN_OK,
				Union: &pb.Msg_LoginOk{
					&pb.LoginOk{
						Player: session.Player,
						Data: &pb.Data{
							Players: GetPlayers(),
						},
					},
				},
			}
			SendMsg(session, msg)

		case session := <-removeSessionChan:
			RemoveSession(session)

		case session := <-listSessionsChan:
			msg := &pb.Msg{
				Type: pb.MsgType_DATA,
				Union: &pb.Msg_Data{
					&pb.Data{
						Players: GetPlayers(),
					},
				},
			}
			SendMsg(session, msg)

		case inviteArg := <-inviteChan:
			for _, session := range sessions {
				if session.Player.Pid == inviteArg.Pid {
					msg := &pb.Msg{
						Type: pb.MsgType_INVITE,
						Union: &pb.Msg_Invite{
							&pb.Invite{
								Pid: inviteArg.Session.Player.Pid,
								Proto: inviteArg.Proto,
							},
						},
					}
					SendMsg(session, msg)
				}
			}
		}

	}
}

func ProcessMsg(session *Session, msg *pb.Msg) {
	log.Printf("Received msg: \n%s\n", msg)
	switch msg.GetType() {
	case pb.MsgType_LOGIN:
		login := msg.GetLogin()
		myplayer := &pb.Player{
			Pid:    login.Pid,
			Passwd: login.Passwd,
		}
		if session.Player == nil {
			session.Player = myplayer
			addSessionChan <- session
		} else {
			// relogin
			session.Player = myplayer
		}

	case pb.MsgType_DATA:
		listSessionsChan <- session

	case pb.MsgType_LOGOUT:
		Leave(session)

	case pb.MsgType_INVITE:
		inviteChan <- &InviteArg{
			Session: session,
			Invite: msg.GetInvite(),
		}
	case pb.MsgType_INVITE_ANSWER:
		inviteAnswer := msg.GetInviteAnswer()
		if inviteAnswer.GetIsAgree() {
			// here, we need create the game and tell players to play
		}else{
			// here, we need to notify the player your answer who invited you
		}
	}
}

// ------------------------------------------------------------

func Listen() {
	listener, err := net.Listen("tcp", ":5678")
	defer listener.Close()
	if err != nil {
		log.Fatal(err)
	}

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Fatal(err)
		}
		session := &Session{
			Conn: conn,
		}
		go HandleConn(session)
	}
}

func HandleConn(session *Session) {
	defer session.Conn.Close()

	const MSG_BUF_LEN = 1024 * 10 //10KB
	const READ_BUF_LEN = 1024     //1KB

	log.Printf("Client: %s\n", session.Conn.RemoteAddr())

	msgBuf := bytes.NewBuffer(make([]byte, 0, MSG_BUF_LEN))
	readBuf := make([]byte, READ_BUF_LEN)

	head := uint32(0)
	bodyLen := 0 //bodyLen is a flag,when readed head,but body'len is not enougth

	for {
		n, err := session.Conn.Read(readBuf)
		if err != nil {
			if err == io.EOF {
				log.Println("---connection lost normal---")
			} else {
				log.Println("Read error: %s\n", err)
			}

			Leave(session)

			break
		}
		_, err = msgBuf.Write(readBuf[:n])

		if err != nil {
			log.Fatalf("Buffer write error: %s\n", err)
		}

		for {
			//read the msg head
			if bodyLen == 0 && msgBuf.Len() >= HeaderSize {
				err := binary.Read(msgBuf, binary.LittleEndian, &head)
				if err != nil {
					log.Printf("Msg head Decode error: %s\n", err)
				}
				bodyLen = int(head)

				if bodyLen > MSG_BUF_LEN {
					log.Fatalf("Msg body too long: %d\n", bodyLen)
				}
			}
			//has head,now read body
			if bodyLen > 0 && msgBuf.Len() >= bodyLen {
				msg := &pb.Msg{}
				err := proto.Unmarshal(msgBuf.Next(bodyLen), msg)
				if err != nil {
					log.Fatalf("Protobuf Unmarshal error: %s\n", err)
				}
				ProcessMsg(session, msg)
				bodyLen = 0
			} else {
				//msgBuf.Len() < bodyLen ,one msg receiving is not complete
				//need to receive again
				break
			}
		}
	}
}

func AddHeader(msgBytes []byte) []byte {
	head := make([]byte, HeaderSize)
	binary.LittleEndian.PutUint32(head, uint32(len(msgBytes)))
	return append(head, msgBytes...)
}

func SendMsg(session *Session, msg *pb.Msg) {
	data, err := proto.Marshal(msg)
	if err != nil {
		log.Fatalf("Protobuf marshal error: %s\n", err)
	}
	_, err = session.Conn.Write(AddHeader(data))
	if err != nil {
		log.Fatalf("Write error: %s\n", err)
	}
}

// ------------------------------------------------------------

func main() {
	go StartServ()
	Listen()
}
