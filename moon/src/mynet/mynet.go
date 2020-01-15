package mynet

import (
	"bytes"
	"encoding/binary"
	"io"
	"log"
	"net"
)

const HeaderSize = 4

//-----------------------------------------------------------------------------

type Protocol interface {
	ConnectionMade(conn net.Conn)
	DataReceived(data []byte)
	ConnectionLost(err error)
}

type MsgReceiver interface {
	ProcessMsg(msgBytes []byte)
}

type ProtocolFactory interface {
	CreateProtocol() Protocol
}

//-----------------------------------------------------------------------------

type MsgProtocol struct {
	Protocol
	MsgReceiver
	msgBuf   bytes.Buffer
	head     uint32
	bodyLen  int
}

func (mp *MsgProtocol) DataReceived(data []byte) {
	_, err := mp.msgBuf.Write(data)
	if err != nil {
		log.Fatalf("buf write: %s\n", err)
	}
	for {
		if mp.bodyLen == 0 && mp.msgBuf.Len() >= HeaderSize {
			err := binary.Read(&mp.msgBuf, binary.LittleEndian, &mp.head)
			if err != nil {
				log.Printf("msg head decode: %v", err)
			}
			mp.bodyLen = int(mp.head)
		}
		//has head,now read body
		if mp.bodyLen > 0 && mp.msgBuf.Len() >= mp.bodyLen {
			mp.ProcessMsg(mp.msgBuf.Next(mp.bodyLen))
			mp.bodyLen = 0
		} else {
			//msgBuf.Len() < bodyLen ,one pb receiving is not complete
			//need to receive again
			break
		}
	}
}

//-----------------------------------------------------------------------------

func HandleConn(conn net.Conn, protocol Protocol) {
	defer conn.Close()

	protocol.ConnectionMade(conn)

	log.Printf("got client: %v", conn.RemoteAddr())

	readBuf := make([]byte, 1024)

	for {
		n, err := conn.Read(readBuf)
		if err != nil {
			if err == io.EOF {
				log.Println("conn closed")
				// conn closed, means normal exit
				protocol.ConnectionLost(nil)
			} else {
				log.Printf("conn read: %v\n", err)
				// something happened,exception
				protocol.ConnectionLost(err)
			}
			break
		}
		protocol.DataReceived(readBuf[:n])
	}
}

func ListenForever(addr string, protocolFactory ProtocolFactory) {
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		log.Fatalf("listener: %v", err)
	}

	defer listener.Close()

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Fatalf("socket accept: %v", err)
		}
		go HandleConn(conn, protocolFactory.CreateProtocol())
	}
	log.Println("----listenForever exit!----")
}

//-----------------------------------------------------------------------------

func AddHeader(msgBytes []byte) []byte {
	head := make([]byte, HeaderSize)
	binary.LittleEndian.PutUint32(head, uint32(len(msgBytes)))
	return append(head, msgBytes...)
}
