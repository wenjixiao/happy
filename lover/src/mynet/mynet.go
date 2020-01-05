package mynet

import (
	"bytes"
	"encoding/binary"
	"io"
	"log"
	"net"
)

const HeaderSize = 4

func AddHeader(msgBytes []byte) []byte {
	head := make([]byte, HeaderSize)
	binary.LittleEndian.PutUint32(head, uint32(len(msgBytes)))
	return append(head, msgBytes...)
}

type Protocol interface {
	ConnectionMade(conn net.Conn)
	DataReceived(data []byte)
	ConnectionLost(err error)
}

type MsgReceiver interface {
	ProcessMsg(msgBytes []byte)
}

type MsgProtocol struct {
	Protocol
	Receiver MsgReceiver
	msgBuf   []byte
	head     uint32
	bodyLen  int
}

func (mp *MsgProtocol) DataReceived(data []byte) {
	mp.msgBuf = append(mp.msgBuf, data...)
	for {
		if mp.bodyLen == 0 && len(mp.msgBuf) >= HeaderSize {
			err := binary.Read(bytes.NewReader(mp.msgBuf[:HeaderSize]), binary.LittleEndian, &mp.head)
			if err != nil {
				log.Printf("msg head decode: %v", err)
			}
			mp.bodyLen = int(mp.head)
		}
		//has head,now read body
		if mp.bodyLen > 0 && len(mp.msgBuf) >= mp.bodyLen+HeaderSize {
			bin := mp.msgBuf[HeaderSize : mp.bodyLen+HeaderSize]
			mp.Receiver.ProcessMsg(bin)
			mp.msgBuf = mp.msgBuf[mp.bodyLen+HeaderSize:]
			mp.bodyLen = 0
		} else {
			//msgBuf.Len() < bodyLen ,one pb receiving is not complete
			//need to receive again
			break
		}
	}
}

func HandleConn(conn net.Conn, protocol Protocol) {
	defer conn.Close()

	protocol.ConnectionMade(conn)

	log.Printf("client: %v", conn.RemoteAddr())

	readBuf := make([]byte, 1024)

	for {
		n, err := conn.Read(readBuf)
		if err != nil {
			if err == io.EOF {
				log.Println("conn closed")
				//conn被关闭。正常退出！
				protocol.ConnectionLost(nil)
			} else {
				log.Printf("conn read: %v\n", err)
				//异常退出
				protocol.ConnectionLost(err)
			}
			break
		}
		protocol.DataReceived(readBuf[:n])
	}
}
