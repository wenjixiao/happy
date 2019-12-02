package main

import (
	"github.com/golang/protobuf/proto"
	"./pb"
	"encoding/binary"
	"bytes"
	"log"
	"net"
	"io"
)

const (
	HeaderSize = 4
)

var sessions []*Session
var addSessionChan chan *Session
var removeSessionChan chan *Session

type Session struct {
	Conn net.Conn
	Player *pb.Player
}

func AddSession(session *Session){
	sessions = append(sessions,session)
}

func RemoveSession(session *Session){
	var index int
	var mysession *Session
	for index,mysession = range sessions {
		if mysession == session {
			break
		}
	}
	copy(sessions[index:],sessions[index+1:])
	sessions = sessions[:len(sessions)-1]
}

func Init(){
	sessions = []*Session{}
	addSessionChan = make(chan *Session,5)
	removeSessionChan = make(chan *Session,5)
}

func StartService(){
	Init()
	for {
		select {
		case session := <-addSessionChan:
			AddSession(session)
		case session := <-removeSessionChan:
			RemoveSession(session)
		}
	}
}

func Listen(){
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
		go HandleConn(conn)
	}
}

func HandleConn(conn net.Conn) {
	defer conn.Close()

	const MSG_BUF_LEN = 1024 * 100 //10KB 
	const READ_BUF_LEN = 1024       //1KB

	// log.Printf("Client: %s\n", conn.RemoteAddr())

	msgBuf := bytes.NewBuffer(make([]byte, 0, MSG_BUF_LEN))
	readBuf := make([]byte, READ_BUF_LEN)

	head := uint32(0)
	bodyLen := 0 //bodyLen is a flag,when readed head,but body'len is not enougth

	for {
		n, err := conn.Read(readBuf)
		if err != nil {
			if err == io.EOF {
				log.Printf("---connection lost normal---")
			} else {
				log.Fatalf("Read error: %s\n", err)
			}
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
					log.Printf("msg head Decode error: %s\n", err)
				}
				bodyLen = int(head)

				if bodyLen > MSG_BUF_LEN {
					log.Fatalf("msg body too long: %d\n", bodyLen)
				}
			}
			//has head,now read body
			if bodyLen > 0 && msgBuf.Len() >= bodyLen {
				msg := &pb.Msg{}
				err := proto.Unmarshal(msgBuf.Next(bodyLen), msg)
				if err != nil {
					log.Fatalf("protobuf Unmarshal error: %s\n",err)
				}
				ProcessMsg(conn,msg)
				bodyLen = 0
			} else {
				//msgBuf.Len() < bodyLen ,one msg receiving is not complete
				//need to receive again
				break
			}
		}
	}
}

func ProcessMsg(conn net.Conn,msg *pb.Msg) {
	log.Println(msg)
	switch msg.GetType(){
	case pb.MsgType_LOGIN:
		login := msg.GetLogin()
		player := &pb.Player{
			Pid: login.Pid,
			Passwd: login.Passwd,
		}
		session := &Session{
			Conn: conn,
			Player: player,
		}
		addSessionChan <- session
	}
	SendMsg(conn,msg)
}

func AddHeader(msgBytes []byte) []byte {
	head := make([]byte, HeaderSize)
	binary.LittleEndian.PutUint32(head, uint32(len(msgBytes)))
	return append(head, msgBytes...)
}

func SendMsg(conn net.Conn,msg *pb.Msg) {
	data,err := proto.Marshal(msg)
	if err != nil {
		log.Fatalf("protobuf marshal error: %s\n",err)
	}
	_,err = conn.Write(AddHeader(data))
	if err != nil {
		log.Fatalf("write error: %s\n",err)
	}
}


func main(){
	go StartService()
	Listen()
}
