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

const HeaderSize = 4

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
