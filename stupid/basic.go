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

func ListenAndServ() {
	listener, err := net.Listen("tcp", ":5678")
	defer listener.Close()
	if err != nil {
		log.Fatalf("listener: %s",err)
	}

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Fatalf("socket accept: %s",err)
		}

		go HandleConn(&Session{Conn: conn})
	}
}

func HandleConn(session *Session) {
	defer session.Conn.Close()

	const MSG_BUF_LEN = 1024 * 10 //10KB
	const READ_BUF_LEN = 1024     //1KB

	log.Printf("client: %s\n", session.Conn.RemoteAddr())

	msgBuf := bytes.NewBuffer(make([]byte, 0, MSG_BUF_LEN))
	readBuf := make([]byte, READ_BUF_LEN)

	head := uint32(0)
	bodyLen := 0 //bodyLen is a flag,when readed head,but body'len is not enougth

	for {
		n, err := session.Conn.Read(readBuf)
		if err != nil {
			if err == io.EOF {
				log.Println("conn closed")
			} else {
				log.Printf("conn read: %s\n", err)
			}
			LineBroken(session)
			break
		}

		_, err = msgBuf.Write(readBuf[:n])

		if err != nil {
			log.Fatalf("buf write: %s\n", err)
		}

		for {
			//read the msg head
			if bodyLen == 0 && msgBuf.Len() >= HeaderSize {
				err := binary.Read(msgBuf, binary.LittleEndian, &head)
				if err != nil {
					log.Printf("msg head decode: %s\n", err)
				}
				bodyLen = int(head)

				if bodyLen > MSG_BUF_LEN {
					log.Fatalf("msg body overflow: %d\n", bodyLen)
				}
			}
			//has head,now read body
			if bodyLen > 0 && msgBuf.Len() >= bodyLen {
				msg := &pb.Msg{}
				err := proto.Unmarshal(msgBuf.Next(bodyLen), msg)
				if err != nil {
					log.Fatalf("proto unmarshal: %s\n", err)
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
		log.Fatalf("proto marshal: %s\n", err)
	}
	_, err = session.Conn.Write(AddHeader(data))
	if err != nil {
		log.Fatalf("conn write: %s\n", err)
	}
}
