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

	CMD_ADD_SESSION = 1;
	CMD_REMOVE_SESSION = 2;
	CMD_DATA =3;
	CMD_INVITE = 4;
	CMD_INVITE_ANSWER = 5;
)

// ------------------------------------------------------------

var sessions []*Session = []*Session{}

var cmdChan chan int = make(chan int,5)
var sessionChan chan *Session = make(chan *Session,5)
var inviteChan chan *pb.Invite = make(chan *pb.Invite,5)
var inviteAnswerChan chan *pb.InviteAnswer = make(chan *pb.InviteAnswer,5)

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
		cmdChan <- CMD_REMOVE_SESSION
		sessionChan <- session
	}
}

func StartServ() {
	// begin to serv
	for {
		switch cmd := <-cmdChan; cmd {
		case CMD_ADD_SESSION:
			session := <- sessionChan
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

		case CMD_REMOVE_SESSION:
			session := <- sessionChan
			RemoveSession(session)

		case CMD_DATA:
			fromSession := <- sessionChan
			msg := &pb.Msg{
				Type: pb.MsgType_DATA,
				Union: &pb.Msg_Data{
					&pb.Data{
						Players: GetPlayers(),
					},
				},
			}
			SendMsg(fromSession, msg)

		case CMD_INVITE:
			fromSession := <- sessionChan
			invite := <- inviteChan
			for _, session := range sessions {
				if session.Player.Pid == invite.Pid {
					msg := &pb.Msg{
						Type: pb.MsgType_INVITE,
						Union: &pb.Msg_Invite{
							&pb.Invite{
								Pid: fromSession.Player.Pid,
								Proto: invite.Proto,
							},
						},
					}
					SendMsg(session, msg)
				}
			}
		case CMD_INVITE_ANSWER:
			fromSession := <- sessionChan
			inviteAnswer := <- inviteAnswerChan
			if inviteAnswer.GetIsAgree() {
				// here, we need create the game and tell players to play
			}else{
				// here, we need to notify the player your answer who invited you
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
			cmdChan <- CMD_ADD_SESSION
			sessionChan <- session
		} else {
			// relogin
			session.Player = myplayer
		}

	case pb.MsgType_DATA:
		cmdChan <- CMD_DATA
		sessionChan <- session

	case pb.MsgType_LOGOUT:
		Leave(session)

	case pb.MsgType_INVITE:
		cmdChan <- CMD_INVITE
		inviteChan <- msg.GetInvite()

	case pb.MsgType_INVITE_ANSWER:
		cmdChan <- CMD_INVITE_ANSWER
		inviteAnswerChan <- msg.GetInviteAnswer()
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
